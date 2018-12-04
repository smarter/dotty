package simulacrum

import scala.annotation.StaticAnnotation

final class op(name: String, alias: Boolean = false) extends StaticAnnotation

final class typeclass(excludeParents: List[String] = Nil, generateAllOps: Boolean = true) extends StaticAnnotation
