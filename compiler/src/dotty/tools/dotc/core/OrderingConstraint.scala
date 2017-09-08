package dotty.tools
package dotc
package core

import Types._, Contexts._, Symbols._, Decorators._
import util.SimpleMap
import collection.mutable
import printing.{Printer, Showable}
import printing.Texts._
import config.Config
import collection.immutable.BitSet
import reflect.ClassTag
import annotation.tailrec

object OrderingConstraint {

  type ArrayValuedMap[T] = SimpleMap[TypeLambda, Array[T]]

  /** The type of `OrderingConstraint#boundsMap` */
  type ParamBounds = ArrayValuedMap[Type]

  /** The type of `OrderingConstraint#lowerMap`, `OrderingConstraint#upperMap` */
  type ParamOrdering = ArrayValuedMap[List[TypeParamRef]]

  /** A new constraint with given maps */
  private def newConstraint(boundsMap: ParamBounds, lowerMap: ParamOrdering, upperMap: ParamOrdering)(implicit ctx: Context) : OrderingConstraint = {
    val result = new OrderingConstraint(boundsMap, lowerMap, upperMap)
    if (Config.checkConstraintsNonCyclic) result.checkNonCyclic()
    ctx.runInfo.recordConstraintSize(result, result.boundsMap.size)
    result
  }

  /** A lens for updating a single entry array in one of the three constraint maps */
  abstract class ConstraintLens[T <: AnyRef: ClassTag] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[T]
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[T])(implicit ctx: Context): OrderingConstraint
    def initial: T

    def apply(c: OrderingConstraint, poly: TypeLambda, idx: Int) = {
      val es = entries(c, poly)
      if (es == null) initial else es(idx)
    }

    /** The `current` constraint but with the entry for `param` updated to `entry`.
     *  `current` is used linearly. If it is different from `prev` it is
     *  known to be dead after the call. Hence it is OK to update destructively
     *  parts of `current` which are not shared by `prev`.
     */
    def update(prev: OrderingConstraint, current: OrderingConstraint,
        poly: TypeLambda, idx: Int, entry: T)(implicit ctx: Context): OrderingConstraint = {
      var es = entries(current, poly)
      if (es != null && (es(idx) eq entry)) current
      else {
        val result =
          if (es == null) {
            es = Array.fill(poly.paramNames.length)(initial)
            updateEntries(current, poly, es)
          }
          else if (es ne entries(prev, poly))
            current // can re-use existing entries array.
          else {
            es = es.clone
            updateEntries(current, poly, es)
          }
        es(idx) = entry
        result
      }
    }

    def update(prev: OrderingConstraint, current: OrderingConstraint,
        param: TypeParamRef, entry: T)(implicit ctx: Context): OrderingConstraint =
      update(prev, current, param.binder, param.paramNum, entry)

    def map(prev: OrderingConstraint, current: OrderingConstraint,
        poly: TypeLambda, idx: Int, f: T => T)(implicit ctx: Context): OrderingConstraint =
     update(prev, current, poly, idx, f(apply(current, poly, idx)))

    def map(prev: OrderingConstraint, current: OrderingConstraint,
        param: TypeParamRef, f: T => T)(implicit ctx: Context): OrderingConstraint =
      map(prev, current, param.binder, param.paramNum, f)
  }

  val boundsLens = new ConstraintLens[Type] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[Type] =
      c.boundsMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[Type])(implicit ctx: Context): OrderingConstraint =
      newConstraint(c.boundsMap.updated(poly, entries), c.lowerMap, c.upperMap)
    def initial = NoType
  }

  val lowerLens = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] =
      c.lowerMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(implicit ctx: Context): OrderingConstraint =
      newConstraint(c.boundsMap, c.lowerMap.updated(poly, entries), c.upperMap)
    def initial = Nil
  }

  val upperLens = new ConstraintLens[List[TypeParamRef]] {
    def entries(c: OrderingConstraint, poly: TypeLambda): Array[List[TypeParamRef]] =
      c.upperMap(poly)
    def updateEntries(c: OrderingConstraint, poly: TypeLambda, entries: Array[List[TypeParamRef]])(implicit ctx: Context): OrderingConstraint =
      newConstraint(c.boundsMap, c.lowerMap, c.upperMap.updated(poly, entries))
    def initial = Nil
  }
}

import OrderingConstraint._

/** Constraint over undetermined type parameters that keeps separate maps to
 *  reflect parameter orderings.
 *  @param boundsMap a map from TypeLambda to arrays.
 *               Each array contains twice the number of entries as there a type parameters
 *               in the TypeLambda. The first half of the array contains the type bounds that constrain the
 *               lambda's type parameters. The second half might contain type variables that
 *               track the corresponding parameters, or is left empty (filled with nulls).
 *               An instantiated type parameter is represented by having its instance type in
 *               the corresponding array entry. The dual use of arrays for poly params
 *               and typevars is to save space and hopefully gain some speed.
 *
 *  @param lowerMap a map from TypeLambdas to arrays. Each array entry corresponds
 *               to a parameter P of the type lambda; it contains all constrained parameters
 *               Q that are known to be smaller than P, i.e. Q <: P.
 *  @param upperMap a map from TypeLambdas to arrays. Each array entry corresponds
 *               to a parameter P of the type lambda; it contains all constrained parameters
 *               Q that are known to be greater than P, i.e. P <: Q.
 */
class OrderingConstraint(private val boundsMap: ParamBounds,
                         private val lowerMap : ParamOrdering,
                         private val upperMap : ParamOrdering) extends Constraint {

  type This = OrderingConstraint

// ----------- Basic indices --------------------------------------------------

  /** The number of type parameters in the given entry array */
  private def paramCount(entries: Array[Type]) = entries.length >> 1

  /** The type variable corresponding to parameter numbered `n`, null if none was created */
  private def typeVar(entries: Array[Type], n: Int): Type =
    entries(paramCount(entries) + n)

  /** The `boundsMap` entry corresponding to `param` */
  def entry(param: TypeParamRef): Type = {
    val entries = boundsMap(param.binder)
    if (entries == null) NoType
    else entries(param.paramNum)
  }

// ----------- Contains tests --------------------------------------------------

  def contains(pt: TypeLambda): Boolean = boundsMap(pt) != null

  def contains(param: TypeParamRef): Boolean = {
    val entries = boundsMap(param.binder)
    entries != null && isBounds(entries(param.paramNum))
  }

  def contains(tvar: TypeVar): Boolean = {
    val origin = tvar.origin
    val entries = boundsMap(origin.binder)
    val pnum = origin.paramNum
    entries != null && isBounds(entries(pnum)) && (typeVar(entries, pnum) eq tvar)
  }

  private def isBounds(tp: Type) = tp.isInstanceOf[TypeBounds]

// ---------- Dependency handling ----------------------------------------------

  def lower(param: TypeParamRef): List[TypeParamRef] = lowerLens(this, param.binder, param.paramNum)
  def upper(param: TypeParamRef): List[TypeParamRef] = upperLens(this, param.binder, param.paramNum)

  def minLower(param: TypeParamRef): List[TypeParamRef] = {
    val all = lower(param)
    all.filterNot(p => all.exists(isLess(p, _)))
  }

  def minUpper(param: TypeParamRef): List[TypeParamRef] = {
    val all = upper(param)
    all.filterNot(p => all.exists(isLess(_, p)))
  }

  def exclusiveLower(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef] =
    lower(param).filterNot(isLess(_, butNot))

  def exclusiveUpper(param: TypeParamRef, butNot: TypeParamRef): List[TypeParamRef] =
    upper(param).filterNot(isLess(butNot, _))

// ---------- Info related to TypeParamRefs -------------------------------------------

  def isLess(param1: TypeParamRef, param2: TypeParamRef): Boolean =
    upper(param1).contains(param2)

  def nonParamBounds(param: TypeParamRef): TypeBounds =
    entry(param).asInstanceOf[TypeBounds]

  def fullLowerBound(param: TypeParamRef)(implicit ctx: Context): Type =
    (nonParamBounds(param).lo /: minLower(param))(_ | _)

  def fullUpperBound(param: TypeParamRef)(implicit ctx: Context): Type =
    (nonParamBounds(param).hi /: minUpper(param))(_ & _)

  def fullBounds(param: TypeParamRef)(implicit ctx: Context): TypeBounds =
    nonParamBounds(param).derivedTypeBounds(fullLowerBound(param), fullUpperBound(param))

  def typeVarOfParam(param: TypeParamRef): Type = {
    val entries = boundsMap(param.binder)
    if (entries == null) NoType
    else {
      val tvar = typeVar(entries, param.paramNum)
      if (tvar != null) tvar else NoType
    }
  }

// ---------- Adding TypeLambdas --------------------------------------------------

  /** The list of parameters P such that, for a fresh type parameter Q:
   *
   *    Q <: tp  implies  Q <: P      and isUpper = true, or
   *    tp <: Q  implies  P <: Q      and isUpper = false
   */
  def dependentParams(tp: Type, isUpper: Boolean): List[TypeParamRef] = tp match {
    case param: TypeParamRef if contains(param) =>
      param :: (if (isUpper) upper(param) else lower(param))
    case tp: AndOrType =>
      val ps1 = dependentParams(tp.tp1, isUpper)
      val ps2 = dependentParams(tp.tp2, isUpper)
      if (isUpper == tp.isAnd) ps1.union(ps2) else ps1.intersect(ps2)
    case _ =>
      Nil
  }

  /** The bound type `tp` without constrained parameters which are clearly
   *  dependent. A parameter in an upper bound is clearly dependent if it appears
   *  in a hole of a context H given by:
   *
   *      H = []
   *          H & T
   *          T & H
   *
   *  (the idea is that a parameter P in a H context is guaranteed to be a supertype of the
   *   bounded parameter.)
   *  Analogously, a parameter in a lower bound is clearly dependent if it appears
   *  in a hole of a context H given by:
   *
   *      L = []
   *          L | T
   *          T | L
   *
   *  "Clearly dependent" is not synonymous with "dependent" in the sense
   *  it is defined in `dependentParams`. Dependent parameters are handled
   *  in `updateEntry`. The idea of stripping off clearly dependent parameters
   *  and to handle them separately is for efficiency, so that type expressions
   *  used as bounds become smaller.
   *
   *  @param isUpper   If true, `bound` is an upper bound, else a lower bound.
   */
  private def stripParams(tp: Type, paramBuf: mutable.ListBuffer[TypeParamRef],
      isUpper: Boolean)(implicit ctx: Context): Type = tp match {
    case param: TypeParamRef if contains(param) =>
      if (!paramBuf.contains(param)) paramBuf += param
      NoType
    case tp: AndOrType if isUpper == tp.isAnd =>
      val tp1 = stripParams(tp.tp1, paramBuf, isUpper)
      val tp2 = stripParams(tp.tp2, paramBuf, isUpper)
      if (tp1.exists)
        if (tp2.exists) tp.derivedAndOrType(tp1, tp2)
        else tp1
      else tp2
    case _ =>
      tp
  }

  /** The bound type `tp` without clearly dependent parameters.
   *  A top or bottom type if type consists only of dependent parameters.
   *  @param isUpper   If true, `bound` is an upper bound, else a lower bound.
   */
  private def normalizedType(tp: Type, paramBuf: mutable.ListBuffer[TypeParamRef],
      isUpper: Boolean)(implicit ctx: Context): Type =
    stripParams(tp, paramBuf, isUpper)
      .orElse(if (isUpper) defn.AnyType else defn.NothingType)

  def add(poly: TypeLambda, tvars: List[TypeVar])(implicit ctx: Context): This = {
    assert(!contains(poly))
    val nparams = poly.paramNames.length
    val entries1 = new Array[Type](nparams * 2)
    poly.paramInfos.copyToArray(entries1, 0)
    tvars.copyToArray(entries1, nparams)
    newConstraint(boundsMap.updated(poly, entries1), lowerMap, upperMap).init(poly)
  }

  /** Split dependent parameters off the bounds for parameters in `poly`.
   *  Update all bounds to be normalized and update ordering to account for
   *  dependent parameters.
   */
  private def init(poly: TypeLambda)(implicit ctx: Context): This = {
    var current = this
    val loBuf, hiBuf = new mutable.ListBuffer[TypeParamRef]
    var i = 0
    while (i < poly.paramNames.length) {
      val param = TypeParamRef(poly, i)
      val bounds = nonParamBounds(param)
      val lo = normalizedType(bounds.lo, loBuf, isUpper = false)
      val hi = normalizedType(bounds.hi, hiBuf, isUpper = true)
      current = updateEntry(current, param, bounds.derivedTypeBounds(lo, hi))
      current = (current /: loBuf)(order(_, _, param))
      current = (current /: hiBuf)(order(_, param, _))
      loBuf.clear()
      hiBuf.clear()
      i += 1
    }
    if (Config.checkConstraintsNonCyclic) checkNonCyclic()
    current
  }

// ---------- Updates ------------------------------------------------------------

  /** Add the fact `param1 <: param2` to the constraint `current` and propagate
   *  `<:<` relationships between parameters ("edges") but not bounds.
   */
  private def order(current: This, param1: TypeParamRef, param2: TypeParamRef)(implicit ctx: Context): This =
    if (param1 == param2 || current.isLess(param1, param2)) this
    else {
      assert(contains(param1))
      assert(contains(param2))
      val newUpper = param2 :: exclusiveUpper(param2, param1)
      val newLower = param1 :: exclusiveLower(param1, param2)
      val current1 = (current /: newLower)(upperLens.map(this, _, _, newUpper ::: _))
      val current2 = (current1 /: newUpper)(lowerLens.map(this, _, _, newLower ::: _))
      current2
    }

  def addLess(param1: TypeParamRef, param2: TypeParamRef)(implicit ctx: Context): This =
    order(this, param1, param2)

  def updateEntry(current: This, param: TypeParamRef, tp: Type)(implicit ctx: Context): This = {
    var current1 = boundsLens.update(this, current, param, tp)
    tp match {
      case TypeBounds(lo, hi) =>
        for (p <- dependentParams(lo, isUpper = false))
          current1 = order(current1, p, param)
        for (p <- dependentParams(hi, isUpper = true))
          current1 = order(current1, param, p)
      case _ =>
    }
    current1
  }

  def updateEntry(param: TypeParamRef, tp: Type)(implicit ctx: Context): This =
    updateEntry(this, param, tp)

  def unify(p1: TypeParamRef, p2: TypeParamRef)(implicit ctx: Context): This = {
    val p1Bounds = (nonParamBounds(p1) & nonParamBounds(p2)).substParam(p2, p1)
    updateEntry(p1, p1Bounds).replace(p2, p1)
  }

// ---------- Removals ------------------------------------------------------------

  /** A new constraint which is derived from this constraint by removing
   *  the type parameter `param` from the domain and replacing all top-level occurrences
   *  of the parameter elsewhere in the constraint by type `tp`, or a conservative
   *  approximation of it if that is needed to avoid cycles.
   *  Occurrences nested inside a refinement or prefix are not affected.
   *
   *  The reason we need to substitute top-level occurrences of the parameter
   *  is to deal with situations like the following. Say we have in the constraint
   *
   *      P <: Q & String
   *      Q
   *
   *  and we replace Q with P. Then substitution gives
   *
   *      P <: P & String
   *
   *  this would be a cyclic constraint is therefore changed by `normalize` and
   *  `recombine` below to
   *
   *      P <: String
   *
   *  approximating the RHS occurrence of P with Any. Without the substitution we
   *  would not find out where we need to approximate. Occurrences of parameters
   *  that are not top-level are not affected.
   */
  def replace(param: TypeParamRef, tp: Type)(implicit ctx: Context): OrderingConstraint = {
    val replacement = tp.dealias.stripTypeVar
    if (param == replacement) this
    else {
      assert(replacement.isValueTypeOrLambda)
      val poly = param.binder
      val idx = param.paramNum

      def removeParam(ps: List[TypeParamRef]) =
        ps.filterNot(p => p.binder.eq(poly) && p.paramNum == idx)

      def replaceParam(tp: Type, atPoly: TypeLambda, atIdx: Int): Type = tp match {
        case bounds @ TypeBounds(lo, hi) =>

          def recombine(andor: AndOrType, op: (Type, Boolean) => Type, isUpper: Boolean): Type = {
            val tp1 = op(andor.tp1, isUpper)
            val tp2 = op(andor.tp2, isUpper)
            if ((tp1 eq andor.tp1) && (tp2 eq andor.tp2)) andor
            else if (andor.isAnd) tp1 & tp2
            else tp1 | tp2
          }

          def normalize(tp: Type, isUpper: Boolean): Type = tp match {
            case p: TypeParamRef if p.binder == atPoly && p.paramNum == atIdx =>
              if (isUpper) defn.AnyType else defn.NothingType
            case tp: AndOrType if isUpper == tp.isAnd => recombine(tp, normalize, isUpper)
            case _ => tp
          }

          def replaceIn(tp: Type, isUpper: Boolean): Type = tp match {
            case `param` => normalize(replacement, isUpper)
            case tp: AndOrType if isUpper == tp.isAnd => recombine(tp, replaceIn, isUpper)
            case _ => tp.substParam(param, replacement)
          }

          bounds.derivedTypeBounds(replaceIn(lo, isUpper = false), replaceIn(hi, isUpper = true))
        case _ =>
          tp.substParam(param, replacement)
      }

      var current =
        if (isRemovable(poly)) remove(poly) else updateEntry(param, replacement)
      current.foreachParam {(p, i) =>
        current = boundsLens.map(this, current, p, i, replaceParam(_, p, i))
        current = lowerLens.map(this, current, p, i, removeParam)
        current = upperLens.map(this, current, p, i, removeParam)
      }
      current
    }
  }

  def remove(pt: TypeLambda)(implicit ctx: Context): This = {
    def removeFromOrdering(po: ParamOrdering) = {
      def removeFromBoundss(key: TypeLambda, bndss: Array[List[TypeParamRef]]): Array[List[TypeParamRef]] = {
        val bndss1 = bndss.map(_.filterConserve(_.binder ne pt))
        if (bndss.corresponds(bndss1)(_ eq _)) bndss else bndss1
      }
      po.remove(pt).mapValuesNow(removeFromBoundss)
    }
    newConstraint(boundsMap.remove(pt), removeFromOrdering(lowerMap), removeFromOrdering(upperMap))
  }

  def isRemovable(pt: TypeLambda): Boolean = {
    val entries = boundsMap(pt)
    @tailrec def allRemovable(last: Int): Boolean =
      if (last < 0) true
      else typeVar(entries, last) match {
        case tv: TypeVar => tv.inst.exists && allRemovable(last - 1)
        case _ => false
      }
    allRemovable(paramCount(entries) - 1)
  }

// ---------- Exploration --------------------------------------------------------

  def domainLambdas: List[TypeLambda] = boundsMap.keys

  def domainParams: List[TypeParamRef] =
    for {
      (poly, entries) <- boundsMap.toList
      n <- 0 until paramCount(entries)
      if entries(n).exists
    } yield TypeParamRef(poly, n)

  def forallParams(p: TypeParamRef => Boolean): Boolean = {
    boundsMap.foreachBinding { (poly, entries) =>
      for (i <- 0 until paramCount(entries))
        if (isBounds(entries(i)) && !p(TypeParamRef(poly, i))) return false
    }
    true
  }

  def foreachParam(p: (TypeLambda, Int) => Unit): Unit =
    boundsMap.foreachBinding { (poly, entries) =>
      0.until(poly.paramNames.length).foreach(p(poly, _))
    }

  def foreachTypeVar(op: TypeVar => Unit): Unit =
    boundsMap.foreachBinding { (poly, entries) =>
      for (i <- 0 until paramCount(entries)) {
        typeVar(entries, i) match {
          case tv: TypeVar if !tv.inst.exists => op(tv)
          case _ =>
        }
      }
    }

  def & (other: Constraint)(implicit ctx: Context) = {
    def merge[T](m1: ArrayValuedMap[T], m2: ArrayValuedMap[T], join: (T, T) => T): ArrayValuedMap[T] = {
      var merged = m1
      def mergeArrays(xs1: Array[T], xs2: Array[T]) = {
        val xs = xs1.clone
        for (i <- xs.indices) xs(i) = join(xs1(i), xs2(i))
        xs
      }
      m2.foreachBinding { (poly, xs2) =>
        merged = merged.updated(poly,
            if (m1.contains(poly)) mergeArrays(m1(poly), xs2) else xs2)
      }
      merged
    }

    def mergeParams(ps1: List[TypeParamRef], ps2: List[TypeParamRef]) =
      (ps1 /: ps2)((ps1, p2) => if (ps1.contains(p2)) ps1 else p2 :: ps1)

    def mergeEntries(e1: Type, e2: Type): Type = e1 match {
        case e1: TypeBounds =>
          e2 match {
            case e2: TypeBounds => e1 & e2
            case _ if e1 contains e2 => e2
            case _ => mergeError
          }
        case tv1: TypeVar =>
          e2 match {
            case tv2: TypeVar if tv1.instanceOpt eq tv2.instanceOpt => e1
            case _ => mergeError
          }
        case _ if e1 eq e2 => e1
        case _ => mergeError
    }

    def mergeError = throw new AssertionError(i"cannot merge $this with $other")

    val that = other.asInstanceOf[OrderingConstraint]
    new OrderingConstraint(
        merge(this.boundsMap, that.boundsMap, mergeEntries),
        merge(this.lowerMap, that.lowerMap, mergeParams),
        merge(this.upperMap, that.upperMap, mergeParams))
  }

  override def checkClosed()(implicit ctx: Context): Unit = {
    def isFreeTypeParamRef(tp: Type) = tp match {
      case TypeParamRef(binder: TypeLambda, _) => !contains(binder)
      case _ => false
    }
    def checkClosedType(tp: Type, where: String) =
      if (tp != null)
        assert(!tp.existsPart(isFreeTypeParamRef), i"unclosed constraint: $this refers to $tp in $where")
    boundsMap.foreachBinding((_, tps) => tps.foreach(checkClosedType(_, "bounds")))
    lowerMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "lower"))))
    upperMap.foreachBinding((_, paramss) => paramss.foreach(_.foreach(checkClosedType(_, "upper"))))
  }

  private var myUninstVars: mutable.ArrayBuffer[TypeVar] = _

  /** The uninstantiated typevars of this constraint */
  def uninstVars: collection.Seq[TypeVar] = {
    if (myUninstVars == null) {
      myUninstVars = new mutable.ArrayBuffer[TypeVar]
      boundsMap.foreachBinding { (poly, entries) =>
        for (i <- 0 until paramCount(entries)) {
          typeVar(entries, i) match {
            case tv: TypeVar if !tv.inst.exists && isBounds(entries(i)) => myUninstVars += tv
            case _ =>
          }
        }
      }
    }
    myUninstVars
  }

// ---------- Cyclic checking -------------------------------------------

  def checkNonCyclic()(implicit ctx: Context): Unit =
    domainParams.foreach(checkNonCyclic)

  private def checkNonCyclic(param: TypeParamRef)(implicit ctx: Context): Unit =
    assert(!isLess(param, param), i"cyclic constraint involving $param in $this")

// ---------- toText -----------------------------------------------------

  override def toText(printer: Printer): Text = {
    def entryText(tp: Type) = tp match {
      case tp: TypeBounds =>
        tp.toText(printer)
      case _ =>
        " := " ~ tp.toText(printer)
    }
    val indent = 3
    val header: Text = "Constraint("
    val uninstVarsText = " uninstVars = " ~
      Text(uninstVars map (_.toText(printer)), ", ") ~ ";"
    val constrainedText =
      " constrained types = " ~ Text(domainLambdas map (_.toText(printer)), ", ")
    val boundsText =
      " bounds = " ~ {
        val assocs =
          for (param <- domainParams)
          yield (" " * indent) ~ param.toText(printer) ~ entryText(entry(param))
        Text(assocs, "\n")
      }
    val orderingText =
      " ordering = " ~ {
        val deps =
          for {
            param <- domainParams
            ups = minUpper(param)
            if ups.nonEmpty
          }
          yield
            (" " * indent) ~ param.toText(printer) ~ " <: " ~
              Text(ups.map(_.toText(printer)), ", ")
        Text(deps, "\n")
      }
    Text.lines(List(header, uninstVarsText, constrainedText, boundsText, orderingText, ")"))
  }

  override def toString: String = {
    def entryText(tp: Type): String = tp match {
      case tp: TypeBounds => tp.toString
      case _ => " := " + tp
    }
    val constrainedText =
      " constrained types = " + domainLambdas.mkString("\n")
    val boundsText = domainLambdas
      " bounds = " + {
        val assocs =
          for (param <- domainParams)
          yield
            param.binder.paramNames(param.paramNum) + ": " + entryText(entry(param))
        assocs.mkString("\n")
    }
    constrainedText + "\n" + boundsText
  }
}
