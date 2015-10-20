package scala.meta
package internal.hosts

package object dotty {
  implicit class XtensionDottyhostDebug(debug: org.scalameta.debug.Debug.type) {
    def logScalahost(op: => Unit): Unit = {
      if (sys.props("dottyhost.debug") != null) debug.log(op)
    }
  }
}