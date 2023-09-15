abstract sealed class Expression[T]
case class Aggregation[T]() extends Expression[T | Null]
case class Plus[T <: Int | Null, E1 <: Expression[T]](lhs: E1) extends Expression[T]

class GenericSqlTranslator:
  def translateExpression(expr: Expression[?]): Unit =
    expr match
      case Plus(lhs) => ()

    // expr match
    //   case Plus(Plus(foo)) => ()
    //   case Plus(Aggregation(...)) => ()



// simplify(Plus(_))
//   simplify(Expression[$T])
//     $T <: Int | Null
    
// Aggregation[?X] <: Expression[$T]
//  Expression[?X] <: Expression[$T]

// Aggregation[T(param)29]  <:  Expression[(param)4]
//   ADD: (param)4 <: T(param)29 | Null
//    T(param)29 | Null <: (param)4
//     ADD: T(param)29 <: (param)4
//      ADD: Null <: (param)4
//      ADD: Null <: T(param)29

