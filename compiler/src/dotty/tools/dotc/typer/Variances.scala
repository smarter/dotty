package dotty.tools.dotc
package typer

import core._
import Types._, Contexts._, Flags._, Symbols._, Annotations._

object Variances {

  type Variance = FlagSet
  val Bivariant: Variance = VarianceFlags
  val Invariant: Variance = EmptyFlags

  /** Flip between covariant and contravariant */
  def flip(v: Variance): Variance = {
    if (v == Covariant) Contravariant
    else if (v == Contravariant) Covariant
    else v
  }

  /** Map everything below Bivariant to Invariant */
  def cut(v: Variance): Variance =
    if (v == Bivariant) v else Invariant

  def compose(v: Variance, boundsVariance: Int): Variance =
    if (boundsVariance == 1) v
    else if (boundsVariance == -1) flip(v)
    else cut(v)

  def varianceString(v: Variance): String =
    if (v is Covariant) "covariant"
    else if (v is Contravariant) "contravariant"
    else "invariant"
}
