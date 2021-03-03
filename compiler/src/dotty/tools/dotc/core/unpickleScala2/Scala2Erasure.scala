package dotty.tools
package dotc
package core
package unpickleScala2

import Symbols._, Types._, Contexts._, Flags._, Names._, StdNames._, Phases._
import Decorators._
import scala.collection.mutable.ListBuffer

object Scala2Erasure:
  /** A type that would be represented as a RefinedType in Scala 2.
   *
   *  The `RefinedType` of nsc contains both a list of parents
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
            // For the purpose of implementing `isNonBottomSubClass` one would expect that aliases
            // could always be dealiased, unfortunately this doesn't quite work. In a situation such
            // as:
            //
            //   trait A; trait B; trait C
            //   type AA = A
            //   type F3 = AA with B
            //   type Rec5 <: F3
            //   type Rec6 <: C with Rec5
            //   def a_53(a: F3 @foo with Rec6): Unit = {}
            //
            // Because `F3` is not in normal form, it gets normalized by the
            // `Uncurry` phase in Scala 2 whcih replaces it by a fresh
            // refinement type `A with B`, but for some reason this
            // replacement does not occur in the `baseTypeSeq` of `Rec6` which still
            // contains the original `AA with B` as a base type. Because different refinements
            // end up with different class symbols, this means that we need:
            //
            //   isNonBottomSubClass(pseudoSymbol(`Rec6`), pseudoSymbol(`F3`))` == false
            //   isNonBottomSubClass(pseudoSymbol(`F3`), pseudoSymbol(`Rec6`))` == false
            //
            // This is accomplished here by keeping aliases iff the rhs is a
            // refinement not in normal form, and by having `isNonBottomSubClass` always
            // return false when comparing a refinement type with an alias.
            // XXX
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
      throw new Error(s"Internal error: unhandled class ${tpw.getClass} for type $tpw in intersectionDominator($parents)")
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
      // these types get their own unique synthetic class symbol, therefore
      // they don't have any sub-class  Note that we must return false
      // even if the lhs is a type alias or abstract type upper-bounded by this refinement, see
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

  /** Mimic what Scala 2 does: intersections like `A with (B with C)` are
   *  flattened to three parents, but `A with ((B with C) @foo)` is kept
   *  as two parents, this can impact the result of `intersectionDominator`.
   */
  def parents(tp: AndType)(using Context): List[Type] =
    val parents = ListBuffer[Type]()
    def collectParents(parent: Type, parents: ListBuffer[Type]): Unit = parent.dealiasKeepAnnots match {
      case AndType(tp1, tp2) =>
        collectParents(tp1, parents)
        collectParents(tp2, parents)
      case _ =>
        /** A Scala 2 refinement is considered to be in normal form if none of its flattened parents
         *  are type aliases. */
        def isNormalForm(tp: Type): Boolean = tp match
          case RefinedType(parent, _, _) =>
            isNormalForm(parent)
          case AndType(tp1, tp2) =>
            isNormalForm(tp1) && isNormalForm(tp2)
          case _ =>
            tp.dealias eq tp

        val checker = new TypeTraverser {
          def traverse(part: Type): Unit = part.dealias match
            case part: (RefinedType | AndType) =>
              if !isNormalForm(part) then
                throw new TypeError(i"Could not compute the erasure of the Scala 2 type $tp because its parent $parent contains a non-normal part $part which has an alias...")
            case _ =>
              traverseChildren(part)
        }
        checker.traverse(parent)
        parents += parent
    }
    collectParents(tp.tp1, parents)
    collectParents(tp.tp2, parents)
    parents.toList
  end parents
end Scala2Erasure
