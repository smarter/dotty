package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.data._
import org.scalameta.unreachable

/*@data*/ case class ConvertException(culprit: Any, message: String, cause: Option[Throwable] = None)
extends Exception(message, cause.orNull) with DottyhostError {
  override def toString = super.toString
}
