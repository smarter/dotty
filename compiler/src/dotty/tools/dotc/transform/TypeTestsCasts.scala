package dotty.tools.dotc
package transform

import core._
import Contexts._, Symbols._, Types._, Constants._, StdNames._, Decorators._
import ast.Trees._
import ast.untpd
import Erasure.Boxing._
import TypeErasure._
import ValueClasses._
import SymUtils._
import core.Flags._
import util.Positions._
import reporting.diagnostic.messages.TypeTestAlwaysSucceeds
import reporting.trace
import config.Printers.{ transforms => debug }

/** This transform normalizes type tests and type casts,
 *  also replacing type tests with singleton argument type with reference equality check
 *  Any remaining type tests
 *   - use the object methods $isInstanceOf and $asInstanceOf
 *   - have a reference type as receiver
 *   - can be translated directly to machine instructions
 *
 * Unfortunately this phase ended up being not Y-checkable unless types are erased. A cast to an ConstantType(3) or x.type
 * cannot be rewritten before erasure. That's why TypeTestsCasts is called from Erasure.
 */
object TypeTestsCasts {
  import ast.tpd._
  import typer.Inferencing.maximizeType
  import typer.ProtoTypes.constrained

  /** Whether `(x:X).isInstanceOf[P]` can be checked at runtime?
   *
   *  First do the following substitution:
   *  (a) replace `T @unchecked` and pattern binder types (e.g., `_$1`) in P with WildcardType
   *  (b) replace pattern binder types (e.g., `_$1`) in X:
   *      - variance = 1  : hiBound
   *      - variance = -1 : loBound
   *      - variance = 0  : OrType(Any, Nothing) // TODO: use original type param bounds
   *
   *  Then check:
   *
   *  1. if `X <:< P`, TRUE
   *  2. if `P` is a singleton type, TRUE
   *  3. if `P` refers to an abstract type member or type parameter, FALSE
   *  4. if `P = Array[T]`, checkable(E, T) where `E` is the element type of `X`, defaults to `Any`.
   *  5. if `P` is `pre.F[Ts]` and `pre.F` refers to a class which is not `Array`:
   *     (a) replace `Ts` with fresh type variables `Xs`
   *     (b) constrain `Xs` with `pre.F[Xs] <:< X`
   *     (c) instantiate Xs and check `pre.F[Xs] <:< P`
   *  6. if `P = T1 | T2` or `P = T1 & T2`, checkable(X, T1) && checkable(X, T2).
   *  7. if `P` is a refinement type, FALSE
   *  8. otherwise, TRUE
   */
  def checkable(X: Type, P: Type, pos: Position)(implicit ctx: Context): Boolean = {
    def isAbstract(P: Type) = !P.dealias.typeSymbol.isClass
    def isPatternTypeSymbol(sym: Symbol) = !sym.isClass && sym.is(Case)

    def replaceP(tp: Type)(implicit ctx: Context) = new TypeMap {
      def apply(tp: Type) = tp match {
        case tref: TypeRef
        if isPatternTypeSymbol(tref.typeSymbol) => WildcardType
        case AnnotatedType(_, annot)
        if annot.symbol == defn.UncheckedAnnot => WildcardType
        case _ => mapOver(tp)
      }
    }.apply(tp)

    def replaceX(tp: Type)(implicit ctx: Context) = new TypeMap {
      def apply(tp: Type) = tp match {
        case tref: TypeRef
        if isPatternTypeSymbol(tref.typeSymbol) =>
          if (variance > 0) tref.info.hiBound
          else if (variance < 0) tref.info.loBound
          else OrType(defn.AnyType, defn.NothingType)
        case _ => mapOver(tp)
      }
    }.apply(tp)

    /** Approximate type parameters depending on variance */
    def stripTypeParam(tp: Type)(implicit ctx: Context) = new ApproximatingTypeMap {
      def apply(tp: Type): Type = tp match {
        case tp: TypeRef if tp.underlying.isInstanceOf[TypeBounds] =>
          val lo = apply(tp.info.loBound)
          val hi = apply(tp.info.hiBound)
          range(lo, hi)
        case _ =>
          mapOver(tp)
      }
    }.apply(tp)

    def isClassDetermined(X: Type, P: AppliedType)(implicit ctx: Context) = {
      val AppliedType(tycon, _) = P
      val typeLambda = tycon.ensureLambdaSub.asInstanceOf[TypeLambda]
      val tvars = constrained(typeLambda, untpd.EmptyTree, alwaysAddTypeVars = true)._2.map(_.tpe)
      val P1 = tycon.appliedTo(tvars)

      debug.println("P : " + P)
      debug.println("P1 : " + P1)
      debug.println("X : " + X)

      P1 <:< X       // constraint P1

      // use fromScala2x to avoid generating pattern bound symbols
      maximizeType(P1, pos, fromScala2x = true)

      val res = P1 <:< P
      debug.println("P1 : " + P1)
      debug.println("P1 <:< P = " + res)

      res
    }

    def recur(X: Type, P: Type): Boolean = (X <:< P) || (P match {
      case _: SingletonType     => true
      case _: TypeProxy
      if isAbstract(P)          => false
      case defn.ArrayOf(tpT)    =>
        X match {
          case defn.ArrayOf(tpE)   => recur(tpE, tpT)
          case _                   => recur(defn.AnyType, tpT)
        }
      case tpe: AppliedType     =>
        // first try withou striping type parameters for performance
        isClassDetermined(X, tpe)(ctx.fresh.setNewTyperState()) ||
        isClassDetermined(stripTypeParam(X), tpe)(ctx.fresh.setNewTyperState())
      case AndType(tp1, tp2)    => recur(X, tp1) && recur(X, tp2)
      case OrType(tp1, tp2)     => recur(X, tp1) && recur(X, tp2)
      case AnnotatedType(t, _)  => recur(X, t)
      case _: RefinedType       => false
      case _                    => true
    })

    val res = recur(replaceX(X.widen), replaceP(P))

    debug.println(i"checking  ${X.show} isInstanceOf ${P} = $res")

    res
  }

  def interceptTypeApply(tree: TypeApply)(implicit ctx: Context): Tree = trace(s"transforming ${tree.show}", show = true) {
    tree.fun match {
      case fun @ Select(expr, selector) =>
        val sym = tree.symbol

        def isPrimitive(tp: Type) = tp.classSymbol.isPrimitiveValueClass

        def derivedTree(expr1: Tree, sym: Symbol, tp: Type) =
          cpy.TypeApply(tree)(expr1.select(sym).withPos(expr.pos), List(TypeTree(tp)))

        def effectiveClass(tp: Type): Symbol =
          if (tp.isRef(defn.PairClass)) effectiveClass(erasure(tp))
          else tp.classSymbol

        def foundCls = effectiveClass(expr.tpe.widen)
        // println(i"ta $tree, found = $foundCls")

        def inMatch =
          fun.symbol == defn.Any_typeTest ||  // new scheme
          expr.symbol.is(Case)                // old scheme

        def transformIsInstanceOf(expr:Tree, testType: Type, flagUnrelated: Boolean): Tree = {
          def testCls = effectiveClass(testType.widen)

          def unreachable(why: => String) =
            if (flagUnrelated)
              if (inMatch) ctx.error(em"this case is unreachable since $why", expr.pos)
              else ctx.warning(em"this will always yield false since $why", expr.pos)

          /** Are `foundCls` and `testCls` classes that allow checks
           *  whether a test would be always false?
           */
          def isCheckable =
            foundCls.isClass && testCls.isClass &&
            !(testCls.isPrimitiveValueClass && !foundCls.isPrimitiveValueClass) &&
               // if `test` is primitive but `found` is not, we might have a case like
               // found = java.lang.Integer, test = Int, which could be true
               // (not sure why that is so, but scalac behaves the same way)
            !isDerivedValueClass(foundCls) && !isDerivedValueClass(testCls)
               // we don't have the logic to handle derived value classes

          /** Check whether a runtime test that a value of `foundCls` can be a `testCls`
           *  can be true in some cases. Issure a warning or an error if that's not the case.
           */
          def checkSensical: Boolean =
            if (!isCheckable) true
            else if (foundCls.isPrimitiveValueClass && !testCls.isPrimitiveValueClass) {
                ctx.error("cannot test if value types are references", tree.pos)
                false
              }
            else if (!foundCls.derivesFrom(testCls)) {
              if (foundCls.is(Final)) {
                unreachable(i"$foundCls is not a subclass of $testCls")
                false
              }
              else if (!testCls.derivesFrom(foundCls) &&
                       (testCls.is(Final) ||
                        !testCls.is(Trait) && !foundCls.is(Trait))) {
                unreachable(i"$foundCls and $testCls are unrelated")
                false
              }
              else true
            }
            else true

          if (expr.tpe <:< testType)
            if (expr.tpe.isNotNull) {
              ctx.warning(TypeTestAlwaysSucceeds(foundCls, testCls), tree.pos)
              constant(expr, Literal(Constant(true)))
            }
            else expr.testNotNull
          else if (!checkSensical)
            constant(expr, Literal(Constant(false)))
          else if (testCls.isPrimitiveValueClass)
            if (foundCls.isPrimitiveValueClass)
              constant(expr, Literal(Constant(foundCls == testCls)))
            else
              transformIsInstanceOf(expr, defn.boxedType(testCls.typeRef), flagUnrelated)
          else
            derivedTree(expr, defn.Any_isInstanceOf, testType)
        }

        def transformAsInstanceOf(testType: Type): Tree = {
          def testCls = testType.widen.classSymbol
          if (expr.tpe <:< testType)
            Typed(expr, tree.args.head)
          else if (foundCls.isPrimitiveValueClass) {
            if (testCls.isPrimitiveValueClass) primitiveConversion(expr, testCls)
            else derivedTree(box(expr), defn.Any_asInstanceOf, testType)
          }
          else if (testCls.isPrimitiveValueClass)
            unbox(expr.ensureConforms(defn.ObjectType), testType)
          else if (isDerivedValueClass(testCls)) {
            expr // adaptToType in Erasure will do the necessary type adaptation
          }
          else
            derivedTree(expr, defn.Any_asInstanceOf, testType)
        }

        /** Transform isInstanceOf OrType
         *
         *    expr.isInstanceOf[A | B]  ~~>  expr.isInstanceOf[A] | expr.isInstanceOf[B]
         *    expr.isInstanceOf[A & B]  ~~>  expr.isInstanceOf[A] & expr.isInstanceOf[B]
         *
         *  The transform happens before erasure of `testType`, thus cannot be merged
         *  with `transformIsInstanceOf`, which depends on erased type of `testType`.
         */
        def transformTypeTest(expr: Tree, testType: Type, flagUnrelated: Boolean): Tree = testType.dealias match {
          case _: SingletonType =>
            expr.isInstance(testType).withPos(tree.pos)
          case OrType(tp1, tp2) =>
            evalOnce(expr) { e =>
              transformTypeTest(e, tp1, flagUnrelated = false)
                .or(transformTypeTest(e, tp2, flagUnrelated = false))
            }
          case AndType(tp1, tp2) =>
            evalOnce(expr) { e =>
              transformTypeTest(e, tp1, flagUnrelated)
                .and(transformTypeTest(e, tp2, flagUnrelated))
            }
          case defn.MultiArrayOf(elem, ndims) if isUnboundedGeneric(elem) =>
            def isArrayTest(arg: Tree) =
              ref(defn.runtimeMethodRef(nme.isArray)).appliedTo(arg, Literal(Constant(ndims)))
            if (ndims == 1) isArrayTest(expr)
            else evalOnce(expr) { e =>
              derivedTree(e, defn.Any_isInstanceOf, e.tpe)
                .and(isArrayTest(e))
            }
          case _ =>
            transformIsInstanceOf(expr, erasure(testType), flagUnrelated)
        }

        if (sym.isTypeTest) {
          val argType = tree.args.head.tpe
          if (!checkable(expr.tpe, argType, tree.pos))
            ctx.warning(i"the type test for $argType cannot be checked at runtime", tree.pos)
          transformTypeTest(expr, tree.args.head.tpe, flagUnrelated = true)
        }
        else if (sym eq defn.Any_asInstanceOf)
          transformAsInstanceOf(erasure(tree.args.head.tpe))
        else tree

      case _ =>
        tree
    }
  }
}
