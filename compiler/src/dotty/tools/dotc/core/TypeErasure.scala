package dotty.tools
package dotc
package core

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._, Phases._
import Flags.JavaDefined
import Uniques.unique
import TypeOps.makePackageObjPrefixExplicit
import transform.ExplicitOuter._
import transform.ValueClasses._
import transform.TypeUtils._
import transform.ContextFunctionResults._
import unpickleScala2.Scala2Erasure
import Decorators._
import Definitions.MaxImplementedFunctionArity
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** The language in which the definition being erased was written. */
enum SourceLanguage {
  case Java, Scala2, Scala3
  def isJava = this eq Java
}
object SourceLanguage {
  /** The language in which `sym` was defined. */
  def ofSymbol(sym: Symbol)(using Context): SourceLanguage =
    if sym.is(JavaDefined) then SourceLanguage.Java
    else if (sym.isClass && sym.is(Scala2x) || sym.maybeOwner.is(Scala2x)) then SourceLanguage.Scala2
    else SourceLanguage.Scala3

  /** Number of bits needed to represent this enum. */
  def bits: Int =
    val len = values.length
    val log2 = 31 - Integer.numberOfLeadingZeros(len)
    if len == 1 << log2 then
      log2
    else
      log2 + 1

   def commonLanguage(x: SourceLanguage, y: SourceLanguage): SourceLanguage =
     if x.ordinal > y.ordinal then x else y
}

/** Erased types are:
 *
 *  ErasedValueType
 *  TypeRef(prefix is ignored, denot is ClassDenotation)
 *  TermRef(prefix is ignored, denot is SymDenotation)
 *  JavaArrayType
 *  AnnotatedType
 *  MethodType
 *  ThisType
 *  SuperType
 *  ClassInfo (NoPrefix, ...)
 *  NoType
 *  NoPrefix
 *  WildcardType
 *  ErrorType
 *
 *  only for isInstanceOf, asInstanceOf: PolyType, TypeParamRef, TypeBounds
 *
 */
object TypeErasure {

  private def erasureDependsOnArgs(sym: Symbol)(using Context) =
    sym == defn.ArrayClass || sym == defn.PairClass || isDerivedValueClass(sym)

  def normalizeClass(cls: ClassSymbol)(using Context): ClassSymbol = {
    if (cls.owner == defn.ScalaPackageClass) {
      if (defn.specialErasure.contains(cls))
        return defn.specialErasure(cls)
      if (cls == defn.UnitClass)
        return defn.BoxedUnitClass
    }
    cls
  }

  /** A predicate that tests whether a type is a legal erased type. Only asInstanceOf and
   *  isInstanceOf may have types that do not satisfy the predicate.
   *  ErasedValueType is considered an erased type because it is valid after Erasure (it is
   *  eliminated by ElimErasedValueType).
   */
  def isErasedType(tp: Type)(using Context): Boolean = tp match {
    case _: ErasedValueType =>
      true
    case tp: TypeRef =>
      val sym = tp.symbol
      sym.isClass &&
      (!erasureDependsOnArgs(sym) || isDerivedValueClass(sym)) &&
      !defn.specialErasure.contains(sym) &&
      !defn.isSyntheticFunctionClass(sym)
    case _: TermRef =>
      true
    case JavaArrayType(elem) =>
      isErasedType(elem)
    case AnnotatedType(tp, _) =>
      isErasedType(tp)
    case ThisType(tref) =>
      isErasedType(tref)
    case tp: MethodType =>
      tp.paramInfos.forall(isErasedType) && isErasedType(tp.resultType)
    case tp @ ClassInfo(pre, _, parents, decls, _) =>
      isErasedType(pre) && parents.forall(isErasedType) //&& decls.forall(sym => isErasedType(sym.info)) && isErasedType(tp.selfType)
    case NoType | NoPrefix | WildcardType | _: ErrorType | SuperType(_, _) =>
      true
    case _ =>
      false
  }

  /** A type representing the semi-erasure of a derived value class, see SIP-15
   *  where it's called "C$unboxed" for a class C.
   *  Derived value classes are erased to this type during Erasure (when
   *  semiEraseVCs = true) and subsequently erased to their underlying type
   *  during ElimErasedValueType. This type is outside the normal Scala class
   *  hierarchy: it is a subtype of no other type and is a supertype only of
   *  Nothing. This is because this type is only useful for type adaptation (see
   *  [[Erasure.Boxing#adaptToType]]).
   *
   *  @param   tycon             A TypeRef referring to the value class symbol
   *  @param   erasedUnderlying  The erased type of the single field of the value class
   */
  abstract case class ErasedValueType(tycon: TypeRef, erasedUnderlying: Type)
  extends CachedGroundType with ValueType {
    override def computeHash(bs: Hashable.Binders): Int = doHash(bs, tycon, erasedUnderlying)
  }

  final class CachedErasedValueType(tycon: TypeRef, erasedUnderlying: Type)
    extends ErasedValueType(tycon, erasedUnderlying)

  object ErasedValueType {
    def apply(tycon: TypeRef, erasedUnderlying: Type)(using Context): ErasedValueType = {
      assert(erasedUnderlying.exists)
      unique(new CachedErasedValueType(tycon, erasedUnderlying))
    }
  }

  private def erasureIdx(sourceLanguage: SourceLanguage, semiEraseVCs: Boolean, isConstructor: Boolean, wildcardOK: Boolean) =
    extension (b: Boolean) def toInt = if b then 1 else 0
    val sourceBits = SourceLanguage.bits
    sourceLanguage.ordinal
    +   (semiEraseVCs.toInt << sourceBits)
    +  (isConstructor.toInt << (sourceBits + 1))
    +     (wildcardOK.toInt << (sourceBits + 2))

  private val erasures = new Array[TypeErasure](1 << (SourceLanguage.bits + 3))

  for
    sourceLanguage <- SourceLanguage.values
    semiEraseVCs <- List(false, true)
    isConstructor <- List(false, true)
    wildcardOK <- List(false, true)
  do
    erasures(erasureIdx(sourceLanguage, semiEraseVCs, isConstructor, wildcardOK)) =
      new TypeErasure(sourceLanguage, semiEraseVCs, isConstructor, wildcardOK)

  /** Produces an erasure function. See the documentation of the class [[TypeErasure]]
   *  for a description of each parameter.
   */
  private def erasureFn(sourceLanguage: SourceLanguage, semiEraseVCs: Boolean, isConstructor: Boolean, wildcardOK: Boolean): TypeErasure =
    erasures(erasureIdx(sourceLanguage, semiEraseVCs, isConstructor, wildcardOK))

  /** The current context with a phase no later than erasure */
  def preErasureCtx(using Context) =
    if (ctx.erasedTypes) ctx.withPhase(erasurePhase) else ctx

  /** The standard erasure of a Scala type. Value classes are erased as normal classes.
   *
   *  @param tp            The type to erase.
  */
  def erasure(tp: Type)(using Context): Type =
    erasureFn(sourceLanguage = SourceLanguage.Scala3, semiEraseVCs = false, isConstructor = false, wildcardOK = false)(tp)(using preErasureCtx)

  /** The value class erasure of a Scala type, where value classes are semi-erased to
   *  ErasedValueType (they will be fully erased in [[ElimErasedValueType]]).
   *
   *  @param tp            The type to erase.
   */
  def valueErasure(tp: Type)(using Context): Type =
    erasureFn(sourceLanguage = SourceLanguage.Scala3, semiEraseVCs = true, isConstructor = false, wildcardOK = false)(tp)(using preErasureCtx)

  /** Like value class erasure, but value classes erase to their underlying type erasure */
  def fullErasure(tp: Type)(using Context): Type =
    valueErasure(tp) match
      case ErasedValueType(_, underlying) => erasure(underlying)
      case etp => etp

  def sigName(tp: Type, sourceLanguage: SourceLanguage)(using Context): TypeName = {
    val normTp = tp.translateFromRepeated(toArray = sourceLanguage.isJava)
    val erase = erasureFn(sourceLanguage, semiEraseVCs = false, isConstructor = false, wildcardOK = true)
    erase.sigName(normTp)(using preErasureCtx)
  }

  /** The erasure of a top-level reference. Differs from normal erasure in that
   *  TermRefs are kept instead of being widened away.
   */
  def erasedRef(tp: Type)(using Context): Type = tp match {
    case tp: TermRef =>
      assert(tp.symbol.exists, tp)
      val tp1 = makePackageObjPrefixExplicit(tp)
      if (tp1 ne tp) erasedRef(tp1)
      else TermRef(erasedRef(tp.prefix), tp.symbol.asTerm)
    case tp: ThisType =>
      tp
    case tp =>
      valueErasure(tp)
  }

  /**  The symbol's erased info. This is the type's erasure, except for the following symbols:
   *
   *   - For $asInstanceOf           : [T]T
   *   - For $isInstanceOf           : [T]Boolean
   *   - For all abstract types      : = ?
   *   - For Scala2-defined symbols: : the erasure of their type with sourceLanguage = Scala2,
   *   - For Java-defined symbols:   : the erasure of their type with sourceLanguage = Java,
   *                                   semiEraseVCs = false. Semi-erasure never happens in Java.
   *   - For all other symbols       : the semi-erasure of their types, with
   *                                   isJava, isConstructor set according to symbol.
   */
  def transformInfo(sym: Symbol, tp: Type)(using Context): Type = {
    val sourceLanguage = SourceLanguage.ofSymbol(sym)
    val semiEraseVCs = !sourceLanguage.isJava
    val erase = erasureFn(sourceLanguage, semiEraseVCs, sym.isConstructor, wildcardOK = false)

    def eraseParamBounds(tp: PolyType): Type =
      tp.derivedLambdaType(
        tp.paramNames, tp.paramNames map (Function.const(TypeBounds.upper(defn.ObjectType))), tp.resultType)

    if (defn.isPolymorphicAfterErasure(sym)) eraseParamBounds(sym.info.asInstanceOf[PolyType])
    else if (sym.isAbstractType) TypeAlias(WildcardType)
    else if (sym.isConstructor) outer.addParam(sym.owner.asClass, erase(tp)(using preErasureCtx))
    else if (sym.is(Label)) erase.eraseResult(sym.info)(using preErasureCtx)
    else erase.eraseInfo(tp, sym)(using preErasureCtx) match {
      case einfo: MethodType =>
        if (sym.isGetter && einfo.resultType.isRef(defn.UnitClass))
          MethodType(Nil, defn.BoxedUnitClass.typeRef)
        else if (sym.isAnonymousFunction && einfo.paramInfos.length > MaxImplementedFunctionArity)
          MethodType(nme.ALLARGS :: Nil, JavaArrayType(defn.ObjectType) :: Nil, einfo.resultType)
        else if (sym.name == nme.apply && sym.owner.derivesFrom(defn.PolyFunctionClass))
          // The erasure of `apply` in subclasses of PolyFunction has to match
          // the erasure of FunctionN#apply, since after `ElimPolyFunction` we replace
          // a `PolyFunction` parent by a `FunctionN` parent.
          einfo.derivedLambdaType(
            paramInfos = einfo.paramInfos.map(_ => defn.ObjectType),
            resType = defn.ObjectType
          )
        else
          einfo
      case einfo =>
        // Erase the parameters of `apply` in subclasses of PolyFunction
        // Preserve PolyFunction argument types to support PolyFunctions with
        // PolyFunction arguments
        if (sym.is(TermParam) && sym.owner.name == nme.apply
            && sym.owner.owner.derivesFrom(defn.PolyFunctionClass)
            && !(tp <:< defn.PolyFunctionType))
          defn.ObjectType
        else
          einfo
    }
  }

  /** Underlying type that does not contain aliases or abstract types
   *  at top-level, treating opaque aliases as transparent.
   */
  def classify(tp: Type)(using Context): Type =
    if (tp.typeSymbol.isClass) tp
    else tp match {
      case tp: TypeProxy => classify(tp.translucentSuperType)
      case tp: AndOrType => tp.derivedAndOrType(classify(tp.tp1), classify(tp.tp2))
      case _ => tp
    }

  /** Is `tp` an abstract type or polymorphic type parameter that has `Any`, `AnyVal`,
   *  or a universal trait as upper bound and that is not Java defined? Arrays of such types are
   *  erased to `Object` instead of `Object[]`.
   */
  def isUnboundedGeneric(tp: Type)(using Context): Boolean = tp.dealias match {
    case tp: TypeRef if !tp.symbol.isOpaqueAlias =>
      !tp.symbol.isClass &&
      !classify(tp).derivesFrom(defn.ObjectClass) &&
      !tp.symbol.is(JavaDefined)
    case tp: TypeParamRef =>
      !classify(tp).derivesFrom(defn.ObjectClass)
    case tp: TypeAlias => isUnboundedGeneric(tp.alias)
    case tp: TypeBounds =>
      val upper = classify(tp.hi)
      !upper.derivesFrom(defn.ObjectClass) &&
      !upper.isPrimitiveValueType
    case tp: TypeProxy => isUnboundedGeneric(tp.translucentSuperType)
    case tp: AndType => isUnboundedGeneric(tp.tp1) && isUnboundedGeneric(tp.tp2)
    case tp: OrType => isUnboundedGeneric(tp.tp1) || isUnboundedGeneric(tp.tp2)
    case _ => false
  }

  /** Is `tp` an abstract type or polymorphic type parameter, or another unbounded generic type? */
  def isGeneric(tp: Type)(using Context): Boolean = tp.dealias match {
    case tp: TypeRef if !tp.symbol.isOpaqueAlias => !tp.symbol.isClass
    case tp: TypeParamRef => true
    case tp: TypeProxy => isGeneric(tp.translucentSuperType)
    case tp: AndType => isGeneric(tp.tp1) || isGeneric(tp.tp2)
    case tp: OrType => isGeneric(tp.tp1) || isGeneric(tp.tp2)
    case _ => false
  }

  /** The erased least upper bound of two erased types is computed as follows
   *  - if both argument are arrays of objects, an array of the erased lub of the element types
   *  - if both arguments are arrays of same primitives, an array of this primitive
   *  - if one argument is array of primitives and the other is array of objects, Object
   *  - if one argument is an array, Object
   *  - otherwise a common superclass or trait S of the argument classes, with the
   *    following two properties:
   *      S is minimal: no other common superclass or trait derives from S
   *      S is last   : in the linearization of the first argument type `tp1`
   *                    there are no minimal common superclasses or traits that
   *                    come after S.
   *  The reason to pick last is that we prefer classes over traits that way,
   *  which leads to more predictable bytecode and (?) faster dynamic dispatch.
   */
  def erasedLub(tp1: Type, tp2: Type)(using Context): Type = {
    // After erasure, C | {Null, Nothing} is just C, if C is a reference type.
    // We need to short-circuit this case here because the regular lub logic below
    // relies on the class hierarchy, which doesn't properly capture `Null`s subtyping
    // behaviour.
    if (tp1.isBottomTypeAfterErasure && tp2.derivesFrom(defn.ObjectClass)) return tp2
    if (tp2.isBottomTypeAfterErasure && tp1.derivesFrom(defn.ObjectClass)) return tp1
    tp1 match {
      case JavaArrayType(elem1) =>
        import dotty.tools.dotc.transform.TypeUtils._
        tp2 match {
          case JavaArrayType(elem2) =>
            if (elem1.isPrimitiveValueType || elem2.isPrimitiveValueType)
              if (elem1.classSymbol eq elem2.classSymbol) // same primitive
                JavaArrayType(elem1)
              else defn.ObjectType
            else JavaArrayType(erasedLub(elem1, elem2))
          case _ => defn.ObjectType
        }
      case _ =>
        tp2 match {
          case JavaArrayType(_) => defn.ObjectType
          case _ =>
            val cls2 = tp2.classSymbol

            /** takeWhile+1 */
            def takeUntil[T](l: List[T])(f: T => Boolean): List[T] = {
              @tailrec def loop(tail: List[T], acc: List[T]): List[T] =
                tail match {
                  case h :: t => loop(if (f(h)) t else Nil, h :: acc)
                  case Nil    => acc.reverse
                }
              loop(l, Nil)
            }

            // We are not interested in anything that is not a supertype of tp2
            val tp2superclasses = tp1.baseClasses.filter(cls2.derivesFrom)

            // From the spec, "Linearization also satisfies the property that a
            // linearization of a class always contains the linearization of its
            // direct superclass as a suffix"; it's enough to consider every
            // candidate up to the first class.
            val candidates = takeUntil(tp2superclasses)(!_.is(Trait))

            // Candidates st "no other common superclass or trait derives from S"
            // Also, drop `PairClass` since it is not valid after erasue
            val minimums = candidates.filter { cand =>
              cand != defn.PairClass
              && candidates.forall(x => !x.derivesFrom(cand) || x.eq(cand))
            }

            // Pick the last minimum to prioritise classes over traits
            minimums.lastOption match {
              // TODO: valueErasure is suspect, should this be in the class and "this(" ?
              case Some(lub) => valueErasure(lub.typeRef)
              case _ => defn.ObjectType
            }
        }
    }
  }

  /** The erased greatest lower bound of two erased type picks one of the two argument types.
   *  It prefers, in this order:
   *  - arrays over non-arrays
   *  - subtypes over supertypes, unless isJava is set
   *  - real classes over traits
   */
  def erasedGlb(tp1: Type, tp2: Type, isJava: Boolean)(using Context): Type = tp1 match {
    case JavaArrayType(elem1) =>
      tp2 match {
        case JavaArrayType(elem2) => JavaArrayType(erasedGlb(elem1, elem2, isJava))
        case _ => tp1
      }
    case _ =>
      tp2 match {
        case JavaArrayType(_) => tp2
        case _ =>
          val tsym1 = tp1.typeSymbol
          val tsym2 = tp2.typeSymbol
          if (!tsym2.exists) tp1
          else if (!tsym1.exists) tp2
          else if (!isJava && tsym1.derivesFrom(tsym2)) tp1
          else if (!isJava && tsym2.derivesFrom(tsym1)) tp2
          else if (tp1.typeSymbol.isRealClass) tp1
          else if (tp2.typeSymbol.isRealClass) tp2
          else tp1
      }
  }


  /** Does the (possibly generic) type `tp` have the same erasure in all its
   *  possible instantiations?
   */
  def hasStableErasure(tp: Type)(using Context): Boolean = tp match {
    case tp: TypeRef if !tp.symbol.isOpaqueAlias =>
      tp.info match {
        case TypeAlias(alias) => hasStableErasure(alias)
        case _: ClassInfo => true
        case _ => false
      }
    case tp: TypeParamRef => false
    case tp: TypeBounds => false
    case tp: TypeProxy => hasStableErasure(tp.translucentSuperType)
    case tp: AndType => hasStableErasure(tp.tp1) && hasStableErasure(tp.tp2)
    case tp: OrType  => hasStableErasure(tp.tp1) && hasStableErasure(tp.tp2)
    case _ => false
  }
}

import TypeErasure._

/**
 *  @param sourceLanguage XX
 *  @param semiEraseVCs  If true, value classes are semi-erased to ErasedValueType
 *                       (they will be fully erased in [[ElimErasedValueType]]).
 *                       If false, they are erased like normal classes.
 *  @param isConstructor Argument forms part of the type of a constructor
 *  @param wildcardOK    Wildcards are acceptable (true when using the erasure
 *                       for computing a signature name).
 */
class TypeErasure(sourceLanguage: SourceLanguage, semiEraseVCs: Boolean, isConstructor: Boolean, wildcardOK: Boolean) {

  /**  The erasure |T| of a type T. This is:
   *
   *   - For a refined type scala.Array+[T]:
   *      - if T is Nothing or Null, []Object
   *      - otherwise, if T <: Object, []|T|
   *      - otherwise, if T is a type parameter coming from Java, []Object
   *      - otherwise, Object
   *   - For a term ref p.x, the type <noprefix> # x.
   *   - For a refined type scala.PolyFunction { def apply[...](x_1, ..., x_N): R }, scala.FunctionN
   *   - For a typeref scala.Any, scala.AnyVal, scala.Singleton, scala.Tuple, or scala.*: : |java.lang.Object|
   *   - For a typeref scala.Unit, |scala.runtime.BoxedUnit|.
   *   - For a typeref scala.FunctionN, where N > MaxImplementedFunctionArity, scala.FunctionXXL
   *   - For a typeref scala.ContextFunctionN, | scala.FunctionN |
   *   - For a typeref P.C where C refers to a class, <noprefix> # C.
   *   - For a typeref P.C where C refers to an alias type, the erasure of C's alias.
   *   - For a typeref P.C where C refers to an abstract type, the erasure of C's upper bound.
   *   - For a this-type C.this, the type itself.
   *   - For all other type proxies: The erasure of the underlying type.
   *   - For T1 & T2, the erased glb of |T1| and |T2| (see erasedGlb)
   *   - For T1 | T2, the first base class in the linearization of T which is also a base class of T2
   *   - For => T, ()T
   *   - For a method type (Fs)scala.Unit, (|Fs|)scala.Unit.
   *   - For any other uncurried method type (Fs)T, (|Fs|)|T|.
   *   - For a curried method type (Fs1)(Fs2)T, (|Fs1|,Es2)ET where (Es2)ET = |(Fs2)T|.
   *   - For a polymorphic type [Ts](Ps)T, |(Ps)T|
   *   _ For a polymorphic type [Ts]T where T is not a method type, ()|T|
   *   - For the class info type of java.lang.Object, the same type without any parents.
   *   - For a class info type of a value class, the same type without any parents.
   *   - For any other class info type with parents Ps, the same type with
   *     parents |Ps|, but with duplicate references of Object removed.
   *   - For NoType or NoPrefix, the type itself.
   *   - For any other type, exception.
   */
  private def apply(tp: Type)(using Context): Type = tp match {
    case _: ErasedValueType =>
      tp
    case tp: TypeRef =>
      val sym = tp.symbol
      if (!sym.isClass) this(tp.translucentSuperType)
      else if (semiEraseVCs && isDerivedValueClass(sym)) eraseDerivedValueClass(tp)
      else if (defn.isSyntheticFunctionClass(sym)) defn.erasedFunctionType(sym)
      else eraseNormalClassRef(tp)
    case tp: AppliedType =>
      val tycon = tp.tycon
      if (tycon.isRef(defn.ArrayClass)) eraseArray(tp)
      else if (tycon.isRef(defn.PairClass)) erasePair(tp)
      else if (tp.isRepeatedParam) apply(tp.translateFromRepeated(toArray = sourceLanguage.isJava))
      else if (semiEraseVCs && isDerivedValueClass(tycon.classSymbol)) eraseDerivedValueClass(tp)
      else apply(tp.translucentSuperType)
    case _: TermRef | _: ThisType =>
      this(tp.widen)
    case SuperType(thistpe, supertpe) =>
      SuperType(this(thistpe), this(supertpe))
    case ExprType(rt) =>
      defn.FunctionType(0)
    case RefinedType(parent, nme.apply, refinedInfo) if parent.typeSymbol eq defn.PolyFunctionClass =>
      assert(refinedInfo.isInstanceOf[PolyType])
      val res = refinedInfo.resultType
      val paramss = res.paramNamess
      assert(paramss.length == 1)
      this(defn.FunctionType(paramss.head.length, isContextual = res.isImplicitMethod, isErased = res.isErasedMethod))
    case tp: TypeProxy =>
      this(tp.underlying)
    case tp @ AndType(tp1, tp2) =>
      sourceLanguage match
        case SourceLanguage.Scala2 =>
          this(Scala2Erasure.intersectionDominator(Scala2Erasure.parents(tp)))
        case _ =>
          erasedGlb(this(tp1), this(tp2), isJava = sourceLanguage.isJava)
    case OrType(tp1, tp2) =>
      TypeComparer.orType(this(tp1), this(tp2), isErased = true)
    case tp: MethodType =>
      def paramErasure(tpToErase: Type) =
        erasureFn(sourceLanguage, semiEraseVCs, isConstructor, wildcardOK)(tpToErase)
      val (names, formals0) = if (tp.isErasedMethod) (Nil, Nil) else (tp.paramNames, tp.paramInfos)
      val formals = formals0.mapConserve(paramErasure)
      eraseResult(tp.resultType) match {
        case rt: MethodType =>
          tp.derivedLambdaType(names ++ rt.paramNames, formals ++ rt.paramInfos, rt.resultType)
        case rt =>
          tp.derivedLambdaType(names, formals, rt)
      }
    case tp: PolyType =>
      this(tp.resultType)
    case tp @ ClassInfo(pre, cls, parents, decls, _) =>
      if (cls.is(Package)) tp
      else {
        def eraseParent(tp: Type) = tp.dealias match { // note: can't be opaque, since it's a class parent
          case tp: AppliedType if tp.tycon.isRef(defn.PairClass) => defn.ObjectType
          case _ => apply(tp)
        }
        val erasedParents: List[Type] =
          if ((cls eq defn.ObjectClass) || cls.isPrimitiveValueClass) Nil
          else parents.mapConserve(eraseParent) match {
            case tr :: trs1 =>
              assert(!tr.classSymbol.is(Trait), i"$cls has bad parents $parents%, %")
              val tr1 = if (cls.is(Trait)) defn.ObjectType else tr
              tr1 :: trs1.filterNot(_.isAnyRef)
            case nil => nil
          }
        val erasedDecls = decls.filteredScope(sym => !sym.isType || sym.isClass)
        val selfType1 = if cls.is(Module) then cls.sourceModule.termRef else NoType
        tp.derivedClassInfo(NoPrefix, erasedParents, erasedDecls, selfType1)
          // can't replace selftype by NoType because this would lose the sourceModule link
      }
    case _: ErrorType | JavaArrayType(_) =>
      tp
    case tp: WildcardType if wildcardOK =>
      tp
    case tp if (tp `eq` NoType) || (tp `eq` NoPrefix) =>
      tp
  }

  private def eraseArray(tp: Type)(using Context) = {
    val defn.ArrayOf(elemtp) = tp
    if (classify(elemtp).derivesFrom(defn.NullClass)) JavaArrayType(defn.ObjectType)
    else if (isUnboundedGeneric(elemtp) && !sourceLanguage.isJava) defn.ObjectType
    else JavaArrayType(erasureFn(sourceLanguage, semiEraseVCs = false, isConstructor, wildcardOK)(elemtp))
  }

  private def erasePair(tp: Type)(using Context): Type = {
    val arity = tp.tupleArity
    if (arity < 0) defn.ProductClass.typeRef
    else if (arity <= Definitions.MaxTupleArity) defn.TupleType(arity)
    else defn.TupleXXLClass.typeRef
  }

  /** The erasure of a symbol's info. This is different from `apply` in the way `ExprType`s and
   *  `PolyType`s are treated. `eraseInfo` maps them them to method types, whereas `apply` maps them
   *  to the underlying type.
   */
  def eraseInfo(tp: Type, sym: Symbol)(using Context): Type =
    val tp1 = tp match
      case tp: MethodicType => integrateContextResults(tp, contextResultCount(sym))
      case _ => tp
    tp1 match
      case ExprType(rt) =>
        if sym.is(Param) then apply(tp1)
            // Note that params with ExprTypes are eliminated by ElimByName,
            // but potentially re-introduced by ResolveSuper, when we add
            // forwarders to mixin methods.
            // See doc comment for ElimByName for speculation how we could improve this.
        else
          MethodType(Nil, Nil,
            eraseResult(sym.info.finalResultType.translateFromRepeated(toArray = sourceLanguage.isJava)))
      case tp1: PolyType =>
        eraseResult(tp1.resultType) match
          case rt: MethodType => rt
          case rt => MethodType(Nil, Nil, rt)
      case tp1 => this(tp1)

  private def eraseDerivedValueClass(tp: Type)(using Context): Type = {
    val cls = tp.classSymbol.asClass
    val unbox = valueClassUnbox(cls)
    if unbox.exists then
      val genericUnderlying = unbox.info.resultType
      val underlying = tp.select(unbox).widen.resultType

      val erasedUnderlying = erasure(underlying)

      // Ideally, we would just use `erasedUnderlying` as the erasure of `tp`, but to
      // be binary-compatible with Scala 2 we need two special cases for polymorphic
      // value classes:
      // - Given `class Foo[A](x: A) extends AnyVal`, `Foo[X]` should erase like
      //   `X`, except if its a primitive in which case it erases to the boxed
      //   version of this primitive.
      // - Given `class Bar[A](x: Array[A]) extends AnyVal`, `Bar[X]` will be
      //   erased like `Array[A]` as seen from its definition site, no matter
      //   the `X` (same if `A` is bounded).
      //
      // The binary compatibility is checked by sbt-dotty/sbt-test/scala2-compat/i8001
      val erasedValueClass =
        if erasedUnderlying.isPrimitiveValueType && !genericUnderlying.isPrimitiveValueType then
          defn.boxedType(erasedUnderlying)
        else if genericUnderlying.derivesFrom(defn.ArrayClass) then
          erasure(genericUnderlying)
        else erasedUnderlying

      if erasedValueClass.exists then ErasedValueType(cls.typeRef, erasedValueClass)
      else
        assert(ctx.reporter.errorsReported, i"no erasure for $underlying")
        NoType
    else NoType
  }

  private def eraseNormalClassRef(tref: TypeRef)(using Context): Type = {
    val cls = tref.symbol.asClass
    (if (cls.owner.is(Package)) normalizeClass(cls) else cls).typeRef
  }

  /** The erasure of a function result type. */
  def eraseResult(tp: Type)(using Context): Type =
    // For a value class V, "new V(x)" should have type V for type adaptation to work
    // correctly (see SIP-15 and [[Erasure.Boxing.adaptToType]]), so the result type of a
    // constructor method should not be semi-erased.
    if semiEraseVCs && isConstructor && !tp.isInstanceOf[MethodOrPoly] then
      erasureFn(sourceLanguage, semiEraseVCs = false, isConstructor, wildcardOK).eraseResult(tp)
    else tp match
      case tp: TypeRef =>
        val sym = tp.symbol
        if (sym eq defn.UnitClass) sym.typeRef
        else this(tp)
      case tp: AppliedType =>
        val sym = tp.tycon.typeSymbol
        if (sym.isClass && !erasureDependsOnArgs(sym)) eraseResult(tp.tycon)
        else this(tp)
      case _ =>
        this(tp)

  /** The name of the type as it is used in `Signature`s.
   *  Need to ensure correspondence with erasure!
   */
  private def sigName(tp: Type)(using Context): TypeName = try
    tp match {
      case tp: TypeRef =>
        if (!tp.denot.exists)
          // println(i"missing: ${tp.toString} ${tp.denot} / ${tp.prefix.member(tp.name)}")
          throw new MissingType(tp.prefix, tp.name)
        val sym = tp.symbol
        if (!sym.isClass) {
          val info = tp.translucentSuperType
          if (!info.exists) assert(false, i"undefined: $tp with symbol $sym")
          return sigName(info)
        }
        if (isDerivedValueClass(sym)) {
          val erasedVCRef = eraseDerivedValueClass(tp)
          if (erasedVCRef.exists) return sigName(erasedVCRef)
        }
        if (defn.isSyntheticFunctionClass(sym))
          sigName(defn.erasedFunctionType(sym))
        else
          val cls = normalizeClass(sym.asClass)
          val fullName =
            if !ctx.erasedTypes then
              // It's important to use the initial symbol to compute the full name
              // because the current symbol might have a different name or owner
              // and signatures are required to be stable before erasure.
              cls.initial.fullName
            else
              cls.fullName
          fullName.asTypeName
      case tp: AppliedType =>
        val sym = tp.tycon.typeSymbol
        sigName( // todo: what about repeatedParam?
          if (erasureDependsOnArgs(sym)) this(tp)
          else if (sym.isClass) tp.underlying
          else tp.translucentSuperType)
      case ErasedValueType(_, underlying) =>
        sigName(underlying)
      case JavaArrayType(elem) =>
        sigName(elem) ++ "[]"
      case tp: TermRef =>
        sigName(tp.widen)
      case ExprType(rt) =>
        sigName(defn.FunctionOf(Nil, rt))
      case tp: TypeVar =>
        val inst = tp.instanceOpt
        if (inst.exists) sigName(inst) else tpnme.Uninstantiated
      case tp @ RefinedType(parent, nme.apply, _) if parent.typeSymbol eq defn.PolyFunctionClass =>
        // we need this case rather than falling through to the default
        // because RefinedTypes <: TypeProxy and it would be caught by
        // the case immediately below
        sigName(this(tp))
      case tp: TypeProxy =>
        sigName(tp.underlying)
      case _: ErrorType | WildcardType | NoType =>
        tpnme.WILDCARD
      case tp: WildcardType =>
        sigName(tp.optBounds)
      case _ =>
        val erasedTp = this(tp)
        assert(erasedTp ne tp, tp)
        sigName(erasedTp)
    }
  catch {
    case ex: AssertionError =>
      println(s"no sig for $tp because of ${ex.printStackTrace()}")
      throw ex
  }
}
