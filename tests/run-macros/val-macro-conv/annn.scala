package a

import scala.compiletime.erasedValue
import scala.quoted.*

sealed trait Exp[T]:
  type Param = T
object Exp:
  type Param[E <: Exp[?]] = E match
    case Exp[t] => t

  inline def reifyPred[S, E <: Exp[Boolean]]: S => Boolean = ${reifyPredImpl[S, E]}

  def reifyPredImpl[S: Type, E <: Exp[Boolean] : Type](using Quotes): Expr[S => Boolean] =
    '{ (self: S) => ${reify[S, Boolean, E]('self)} }

  def reify[S: Type, T: Type, E <: Exp[T] : Type](using Quotes)(self: Expr[S]): Expr[T] =
    def go[E1 <: Exp[?] : Type](using Quotes): Expr[Exp.Param[E1]] =
      Type.of[E1] match
        case '[Plus[a, b]] =>
          '{ ${go[a]} + ${go[b]} }.asExprOf[Exp.Param[E1]]
        case '[Or[a, b]] => // a and b must be booleans
          '{ ${go[a]} || ${go[b]} }.asExprOf[Exp.Param[E1]]
        case '[type a <: S; Self[a]] =>
          self.asExprOf[Exp.Param[E1]]
    go[E]

  // inline def reifyI[S, T, E <: Exp[T]](inline self: S): T =
  //   inline scala.compiletime.erasedValue[E] match
  //     case _: Plus[a, b] =>
  //       reifyI[S, Int, a](self) + reifyI[S, Int, b](self)
  class Reifier[S](self: S):
    // inline def reifyII[E <: Exp[Param[E]]]: Param[E] = reifyI[Param[E], E]
    inline def reifyI[T, E <: Exp[T]]: T =
      inline erasedValue[E] match
        case _: Plus[a, b] =>
          reifyI[Int, a] + reifyI[Int, b]
        case _: Eq[a, b] =>
          inline erasedValue[a] match
            case _: Exp[aa] =>
              inline erasedValue[b] match
                case _: Exp[bb] =>
                  reifyI[aa, a & Exp[aa]] == reifyI[bb, b & Exp[bb]]
        case _: Or[a, b] =>
          reifyI[Boolean, a] || reifyI[Boolean, b]
        case _: Self[S] =>
          self.asInstanceOf[T] // cast because of FIXME on Self
        case _: Sngl[a] =>
          // valueOf[a].asInstanceOf[T]
          valueOf[a].asInstanceOf[T]
end Exp

class App[T, Elem <: Exp[?], Args <: Tuple /* of Exp[?]*/] extends Exp[T]
class Sel[T, Qual <: Exp[?], Name <: String] extends Exp[T]

class Sngl[T] extends Exp[Int] // FIXME: should be T, but covariance is problematic
class Self[T] extends Exp[Int] // Idem

class Plus[S <: Exp[Int], T <: Exp[Int]] extends Exp[Int]
// Universal equality?
class Eq[L <: Exp[?], R <: Exp[?]] extends Exp[Boolean]
class Or[S <: Exp[Boolean], T <: Exp[Boolean]] extends Exp[Boolean]

// class Eq[T, L <: Exp[T], R <: Exp[T]] extends Exp[Boolean]
// class Not[S <: Exp[Boolean]] extends Exp[Boolean]

// class Pred[T, S <: Exp[Boolean]] extends Exp[T => Boolean]
// class Fun[P, R, Param <: Exp[P], Result <: Exp[R]] extends Exp[P => R]


// No need for tracking type cause it can be inferred back?
// class Plus[S <: Exp, T <: Exp] extends Exp

// No need to track type for bindings either?
// class Let[E <: Exp, U <: Exp]
// class Ref[I <: Int]
// Let[Foo, Plus[Ref[0], Ref[0]]]
// Let["x", Foo, Plus[Ref["x"], Ref["x"]]]
// ^-- need to keep track of whether we've seen "x" before to disambiguate,
//     trees aren't guaranteed to respect shadowing (I think?)

// x: Int with (_  > 0 && _ < 10)
// =>
// assert(x > 0, "failed assertion: x > 0 where x = $x and this = $this")
// x: Int with (_  + y == 0)
// =>
// assert(x + y == 0, "failed assertion: x + y == 0 where x = $x and y = $y")

class ann[E <: Exp[?]](g: Target[E]) extends annotation.StaticAnnotation with annotation.RefiningAnnotation:
  inline def reifyExp() =
    inline erasedValue[E] match
      case _: Exp[a] =>
        val r = new Exp.Reifier(42)
        r.reifyI[a, E & Exp[a]]
object ann:
  def the[T]: T = ???
end ann

class Target[E <: Exp[?]]
object Target:
  def dummy[E <: Exp[?]]: Target[E] = new Target[E]//???
  // def apply(x: T): Boolean
  // given [T]: Conversion[T => Boolean, Target[T]]
  transparent inline implicit def conv[T](inline f: T => Boolean): Target[?] = ${Macro.convImpl[T]('f)}
object Macro:
  def convImpl[T: Type](using Quotes)(f: Expr[T => Boolean]): Expr[Target[?]] =
    import quotes.reflect.*

    val plusSym = defn.IntClass.methodMember("+").find(_.paramSymss match
      case List(List(paramSym)) =>
        paramSym.termRef.typeSymbol == defn.IntClass
      case _ => false
    ).get

    class ToExp extends TreeAccumulator[TypeTree]:
      def foldTree(acc: TypeTree, tree: Tree)(owner: Symbol): TypeTree = //Type[? <: Exp] =
        tree match
          case i: Ident =>
            // todo: need a typetree to workaround issue with refersToParam
            // i.tpe.asType match case '[t] => Type.of[Id[t]]
            i.tpe match
              case tp @ TermRef(_: NoPrefix, _) =>
                val tptSngl = TypeTree.ref(Symbol.requiredClass("a.Sngl"))
                Applied(tptSngl, List(Singleton(Ref.term(tp))))
              case _ =>
                i.tpe.asType match
                  case '[t] => TypeTree.of[Sngl[t]]
          case l: Literal =>
            l.tpe.asType match
              case '[t] => TypeTree.of[Sngl[t]]
          case tree @ Select(qual, name) =>
            val qualTree = foldTree(acc, qual)(owner)
            val tptSel = TypeTree.ref(Symbol.requiredClass("a.Sel"))
            tree.tpe.asType match
              case '[t] => Applied(tptSel, List(TypeTree.of[t], qualTree, Singleton(Literal(StringConstant(name)))))
          case tree @ Apply(fun @ Select(qual, _), List(arg)) if fun.symbol eq plusSym =>
            val qualTree = foldTree(acc, qual)(owner)
            val argTree = foldTree(acc, arg)(owner)
            val tptPlus = TypeTree.ref(Symbol.requiredClass("a.Plus"))
            Applied(tptPlus, List(qualTree, argTree))
          case tree @ Apply(fun @ Select(qual, "=="), List(arg)) => // should be limited to == on prim or Any#==
            val qualTree = foldTree(acc, qual)(owner)
            val argTree = foldTree(acc, arg)(owner)
            val tptEq = TypeTree.ref(Symbol.requiredClass("a.Eq"))
            Applied(tptEq, List(qualTree, argTree))
          case tree @ Apply(fun, args) =>
            val funTree = foldTree(acc, fun)(owner)
            val argsTrees = args.map(foldTree(acc, _)(owner))
            val tptApp = TypeTree.ref(Symbol.requiredClass("a.App"))
            tree.tpe.asType match
              case '[t] => Applied(tptApp, TypeTree.of[t] :: funTree :: argsTrees)
          case TypeApply(fun, args) if fun.symbol eq Symbol.requiredMethod("a.ann.the") =>
            val tptSelf = TypeTree.ref(Symbol.requiredClass("a.Self"))
            Applied(tptSelf, args)
          case _ =>
            // don't propagate acc to avoid misleading myself
            // foldOverTree(acc, tree)(owner)
            foldOverTree(TypeTree.of[Any], tree)(owner)

    // TODO: don't use `the` which ends up creating a local val,
    // inspect the lambda directly instead.
    val input = Expr.betaReduce('{$f(ann.the[T])}).asTerm
    println("input: " + input.show)
    val targ = ToExp().foldTree(TypeTree.of[Exp], input)(Symbol.spliceOwner)
    val targs = List(targ)

    val dummyRef = Symbol.requiredMethod("a.Target.dummy")
    // because subst doesn't work on trees, we'll need a conversion from @ann[E](...) to @ann[E](...)
    val z1 = Ref(dummyRef).appliedToTypeTrees(targs)
    println("z1: " + z1.show)
    z1.asExprOf[Target[?]]

    // val ClsTypeTree = TypeTree.ref(Symbol.requiredClass("a.Target"))
    // val z =
    //   Apply(
    //     TypeApply(
    //       Select.unique(New(Applied(ClsTypeTree, targs)), "<init>"),
    //       targs
    //     ), List())
    // println("z: " + z.show)
    // z.asExprOf[Target[?]]
