package dotty.tools.dotc
package core

import core._
import Types._, Contexts._, Flags._, Symbols._, Annotations._

case class Variance(val bits: Int) extends AnyVal {
  import Variance._

  /** Does this variance conform to variance `that` ?
   *  This relation is a partial order.
   */
  def conforms(that: Variance): Boolean =
    this == that || this == Bivariance || that == Invariance

  /** The greatest lower bound of `this` and `that`,
   *  this is a variance that conforms both to `this` and `that`.
   */
  def glb(that: Variance): Variance =
    if (this == that) this
    else if (this == Bivariance) that
    else if (that == Bivariance) this
    else Invariance

  /** Flip between covariance and contravariance */
  def flip: Variance =
    if (this == Covariance) Contravariance
    else if (this == Contravariance) Covariance
    else this

  def unary_-(): Variance = flip

  def >=(x: Int) = this.bits >= x
  def <=(x: Int) = this.bits <= x
  def >(x: Int) = this.bits > x
  def <(x: Int) = this.bits < x

  /** Map everything below Bivariance to Invariance */
  def cut: Variance =
    if (this == Bivariance) this else Invariance

  def compose(that: Variance): Variance =
    if (that == Covariance) this
    else if (that == Contravariance) this.flip
    else this.cut

  def dropContravariance: Variance =
    if (this == Contravariance) Invariance
    else if (this == Bivariance) Covariance
    else this

  override def toString: String =
    if (this == Bivariance) "bivariant"
    else if (this == Covariance) "covariant"
    else if (this == Contravariance) "contravariant"
    else "invariant"
}

object Variance {
  val Bivariance: Variance = Variance(2)
  val Covariance: Variance = Variance(1)
  val Invariance: Variance = Variance(0)
  val Contravariance: Variance = Variance(-1)

  def fromFlags(flags: FlagSet): Variance =
    if (flags is Covariant)
      if (flags is Contravariant)
        Bivariance
      else
        Covariance
    else if (flags is Contravariant)
      Contravariance
    else
      Invariance
}
