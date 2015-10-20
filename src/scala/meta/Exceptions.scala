package scala.meta

import org.scalameta.data._
import org.scalameta.unreachable
import scala.compat.Platform.EOL

trait DottyhostException extends ScalametaException

trait DottyhostError extends ScalametaError

/*@data*/ case class InfrastructureException(message: String, cause: Option[Throwable])
extends Exception(message, cause.orNull) with DottyhostException {
  def this(message: String) = this(message, None)
  def this(message: String, cause: Throwable) = this(message, Some(cause))
  override def toString = super.toString
}

/*@data*/ case class TypecheckException(tree: Tree, message: String)
extends Exception(message) with DottyhostException {
  import scala.meta.dialects.Scala211
  override def toString = s"$message$EOL${tree.show[Syntax]}"
}
