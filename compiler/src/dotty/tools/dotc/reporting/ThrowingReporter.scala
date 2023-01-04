package dotty.tools
package dotc
package reporting

import core.Contexts._
import Diagnostic.Error

/**
 * This class implements a Reporter that throws all errors and sends warnings and other
 * info to the underlying reporter.
 *
 * @param hasStackTraces If true, the thrown errors will have their stack traces filled in.
 */
class ThrowingReporter(reportInfo: Reporter, override val hasStackTraces: Boolean) extends Reporter {
  def doReport(dia: Diagnostic)(using Context): Unit = dia match {
    case _: Error => throw dia
    case _ => reportInfo.doReport(dia)
  }
}
