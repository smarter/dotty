package dotty.tools
package dotc
package core
package unpickleScala2

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._, Phases._
import Decorators._
import scala.collection.mutable.ListBuffer

/** Erasure logic specific to Scala 2 symbols.  */
object Scala2Erasure:
  /** A type that would be represented as a RefinedType in Scala 2.
   *
   *  The `RefinedType` of Scala 2 contains both a list of parents
   *  and a list of refinements, intersections are represented as a RefinedType
   *  with no refinements.
   */
  type Scala2RefinedType = RefinedType | AndType

  /** A TypeRef that is known to represent a member of a structural type. */
  type StructuralRef = TypeRef

  /** The equivalent of a Scala 2 type symbol.
   *
   *  In some situations, nsc will create a symbol for a type where we wouldn't:
   *
   *  - `A with B with C { ... }` is represented with a RefinedType whose
   *    symbol is a fresh class symbol whose parents are `A`, `B`, `C`.
   *  - Structural members also get their own symbols.
   *
   *  To emulate this, we simply use the type itself a stand-in for its symbol.
   *
   *  See also `sameSymbol` which determines if two pseudo-symbols are really the same.
   */
  type PseudoSymbol = Symbol | StructuralRef | Scala2RefinedType

  /** The pseudo symbol of `tp`, see `PseudoSymbol`.
   *
   *  The pseudo-symbol representation of a given type is chosen such that
   *  `isNonBottomSubClass` behaves like it would in Scala 2, this requires
   *  handling type aliases in a very specific way, see the implementation for
   *  details.
   */
  def pseudoSymbol(tp: Type)(using Context): PseudoSymbol = tp.widen match
    case tpw: OrType => // Could appear in Scala.js code
      // pseudoSymbol(erasure(tpw))
      pseudoSymbol(???) // ErasedUnionSymbol
    case tpw: Scala2RefinedType =>
      tpw
    case tpw: TypeRef =>
      val sym = tpw.symbol
      if !sym.exists then
        // The pseudo-symbol of a structural member type is the type itself.
        tpw
      else
        sym.info match
          case info: AliasingBounds =>
            // For the purpose of implementing `isNonBottomSubClass` one would
            // expect that aliases could always be dealiased, unfortunately this
            // doesn't quite work. In a situation such as:
            //
            //   trait A; trait B; trait C
            //   type F1 = A with B
            //   type F2 = A with B
            //   type Rec3 <: F1
            //   type Rec4 <: C with Rec3
            //   def a_51(a: F2 @foo with Rec4): Unit = {}
            //
            // We can't simply dealias `F1` and `F2` to the same `A with B`:
            // because Scala 2 does not hash-cons refinements, each of these
            // type aliases points to a different instance of RefinedType and
            // therefore each one has its own unique class symbol. We represent
            // this by simply returning the symbol of the alias as its
            // pseudo-symbol, and by having `isNonBottomSubClass` always return
            // false when comparing two distinct aliases to the same type, or an
            // alias to its underlying type.
            if info.alias.isInstanceOf[Scala2RefinedType] then
              sym
            else
              pseudoSymbol(info.alias)
          case _ =>
            sym
    case tpw: TypeProxy =>
      pseudoSymbol(tpw.underlying)
    case tpw: JavaArrayType =>
      defn.ArrayClass
    case tpw: ErrorType =>
      defn.ObjectClass
    case tpw =>
      throw new Error(s"Internal error: unhandled class ${tpw.getClass} for type $tpw in pseudoSymbol($tp)")
  end pseudoSymbol

  /** Would these two pseudo-symbols be represented with the same symbol in Scala 2? */
  def sameSymbol(psym1: PseudoSymbol, psym2: PseudoSymbol)(using Context): Boolean =
    // Pattern match on (psym1, psym2) desugared by hand to avoid allocating a tuple
    if psym1.isInstanceOf[StructuralRef] && psym2.isInstanceOf[StructuralRef] then
      val tp1 = psym1.asInstanceOf[StructuralRef]
      val tp2 = psym2.asInstanceOf[StructuralRef]
      // Two structural members will have the same Scala 2 symbol if they
      // point to the same member. We can't just call `=:=` since different
      // prefixes will still have the same symbol.
      (tp1.name eq tp2.name) && sameSymbol(pseudoSymbol(tp1.prefix), pseudoSymbol(tp2.prefix))
    else
      // We intentionally use referential equality here even though we may end
      // up comparing two equivalent intersection types, because Scala 2 will
      // create fresh symbols for each appearance of an intersection type in
      // source code.
      psym1 eq psym2

  /** The dealiased version of this pseudo-symbol, see `pseudoSymboŀ` for details
   *  on what it means for a pseudo-symbol to be an alias. */
  def dealias(psym: PseudoSymbol)(using Context): PseudoSymbol = psym match
    case sym: Symbol =>
      sym.info match
        case TypeAlias(ref) =>
          pseudoSymbol(ref.dealias)
        case _ =>
          sym
    case _ =>
      psym

  /** Is this a class or an alias of a class? Also returns true for refinements
   *  since they get a class symbol in Scala 2. */
  def isClass(psym: PseudoSymbol)(using Context): Boolean = psym match
    case tp: Scala2RefinedType =>
      true
    case tp: StructuralRef =>
      false
    case sym: Symbol =>
      val sym1 = dealias(sym)
      if sym1 ne sym then isClass(sym1)
      else sym.isClass

  /** Is this a trait or an alias of a trait? */
  def isTrait(psym: PseudoSymbol)(using Context): Boolean = psym match
    case tp: Scala2RefinedType =>
      false
    case tp: StructuralRef =>
      false
    case sym: Symbol =>
      val sym1 = dealias(sym)
      if sym1 ne sym then isTrait(sym1)
      else sym.is(Trait)

  /** An emulation of `Symbol#isNonBottomSubClass` from Scala 2.
   *
   *  The documentation of the original method is:
   *
   *  > Is this class symbol a subclass of that symbol,
   *  > and is this class symbol also different from Null or Nothing?
   *
   *  Which sounds fine, except that it is also used with non-class symbols,
   *  so what does it do then? Its implementation delegates to `Type#baseTypeSeq`
   *  whose documentation states:
   *
   *  > The base type sequence of T is the smallest set of [...] class types Ti, so that [...]
   *
   *  But this is also wrong: the sequence returned by `baseTypeSeq` can
   *  contain non-class symbols.
   *
   *  Given that we cannot rely on the documentation and that the
   *  implementation is extremely complex, this reimplementation is mostly
   *  based on reverse-engineering rules derived from the observed behavior of
   *  the original method.
   */
  def isNonBottomSubClass(psym1: PseudoSymbol, psym2: PseudoSymbol)(using Context): Boolean =
    /** Recurse on the upper-bound of `psym1`:
     *  an abstract type or type alias is a sub of a pseudo-symbol, if
     *  its upper-bound is a sub of that pseudo-symbol.
     */
    def goUpperBound(psym1: Symbol | StructuralRef): Boolean =
      val info = psym1 match
        case sym: Symbol => sym.info
        case tp: StructuralRef => tp.info
      info match
        case info: TypeBounds =>
          go(pseudoSymbol(info.hi))
        case _ =>
          false

    def go(psym1: PseudoSymbol): Boolean =
      sameSymbol(psym1, psym2) ||
      // As mentioned in the documentation of `Scala2RefinedType`, in Scala 2
      // these types get their own unique synthetic class symbol, therefore they
      // don't have any sub-class Note that we must return false even if the lhs
      // is a type alias or abstract type upper-bounded by this refinement, see
      // the handling of aliases in `pseudoSymbol` for details.
      !psym2.isInstanceOf[Scala2RefinedType] && psym1.match
        case sym1: Symbol => psym2 match
          case sym2: Symbol =>
            if sym1.isClass && sym2.isClass then
              sym1.derivesFrom(sym2)
            else if !sym1.isClass then
              goUpperBound(sym1)
            else
              // sym2 is either a type alias or an abstract type:
              // - If it's a type alias, by the definition of `pseudoSymbol` we
              //   know it must be an alias of a refinement and that it shouldn't
              //   be considered equal to that refinement for the purpose of
              //   `isNonBottomSubClass`, so we can return false.
              // - If it's an abstract type, we can also return false because
              //   `isNonBottomSubClass` in Scala 2 never considers a class C to be
              //   a a sub of an abstract type T, even if it was declared as
              //  `type T >: C`.
              false
          case _ =>
            goUpperBound(sym1)
        case tp1: StructuralRef =>
          goUpperBound(tp1)
        case tp1: RefinedType =>
          go(pseudoSymbol(tp1.parent))
        case AndType(tp11, tp12) =>
          go(pseudoSymbol(tp11)) || go(pseudoSymbol(tp12))
    end go

    go(psym1)
  end isNonBottomSubClass

  /** An emulation of `Erasure#intersectionDominator` from Scala 2.
   *
   *  Accurately reproducing the behavior of this method is extremely difficult
   *  because it operates on the symbols of the _non-erased_ parent types, an
   *  implementation detail of the compiler. Furthermore, these non-class
   *  symbols are passed to methods such as `isNonBottomSubClass` whose behavior
   *  is only specified for class symbols. Therefore, the accuracy of this
   *  method cannot be guaranteed, the best we can do is make sure it works on
   *  as many test cases as possible which can be run from sbt using:
   *  > sbt-dotty/scripted scala2-compat/types
   *
   *  The body of this method is made to look as much as the Scala 2 version as
   *  possible to make them easier to compare, cf:
   *  https://github.com/scala/scala/blob/v2.13.5/src/reflect/scala/reflect/internal/transform/Erasure.scala#L356-L389
   */
  def intersectionDominator(parents: List[Type])(using Context): Type =
    val psyms = parents.map(pseudoSymbol)
    if (psyms.contains(defn.ArrayClass)) {
      defn.ArrayOf(
        intersectionDominator(parents.collect { case defn.ArrayOf(arg) => arg }))
    } else {
      def isUnshadowed(psym: PseudoSymbol) =
        !(psyms.exists(qsym => !sameSymbol(psym, qsym) && isNonBottomSubClass(qsym, psym)))
      val cs = parents.iterator.filter { p =>
        val psym = pseudoSymbol(p)
        isClass(psym) && !isTrait(psym) && isUnshadowed(psym)
      }
      (if (cs.hasNext) cs else parents.iterator.filter(p => isUnshadowed(pseudoSymbol(p)))).next()
    }

  /** A flattened list of parents of this intersection.
   *
   *  Mimic what Scala 2 does: intersections like `A with (B with C)` are
   *  flattened to three parents.
   *
   *  @throws TypeError if our implementation of `intersectionDominator` does
   *                    not support this type.
   */
  def flattenedParents(tp: AndType)(using Context): List[Type] =
    val parents = ListBuffer[Type]()
    def collect(parent: Type, parents: ListBuffer[Type]): Unit = parent.dealiasKeepAnnots match
      case AndType(tp1, tp2) =>
        collect(tp1, parents)
        collect(tp2, parents)
      case AnnotatedType(parent, _) =>
        // Don't try to support types of the form `(A with B) @foo with C`, as it
        // would make the implementation of `intersectionDominator`
        // significantly more complicated. The problem is that each textual
        // appearance of `A with B` in a parent corresponds to a fresh instance
        // of RefinedType (because Scala 2 does not hash-cons refinements) with
        // a fresh synthetic class symbol, thus affecting the result of
        // `isNonBottomSubClass`. To complicate the matter, the Scala 2 UnCurry
        // phase will also recursively dealias parent types, thus creating
        // distinct class symbols even in situations where the same type alias is
        // used to refer to a given refinement.
        // See scala2/erasure-unsupported/../Unsupported.scala for examples.
        if parent.dealias.isInstanceOf[Scala2RefinedType] then
          throw new TypeError(i"Unsupported Scala 2 intersection $tp: its component $parent is annotated.")
        parents += parent.dealias
      case _ =>
        parents += parent
    end collect

    collect(tp.tp1, parents)
    collect(tp.tp2, parents)
    parents.toList
  end flattenedParents
end Scala2Erasure
