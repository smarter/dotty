// Keep synchronized with dottyApp/Api.scala
package scala2Lib

class foo extends scala.annotation.StaticAnnotation

trait A
trait B
trait SubB extends B
trait C
trait Cov[+T]

class D

class VC(val self: A) extends AnyVal

class Outer {
  class E
  trait F extends E
}

// The parameter type of `a_XX` should erase to A, `b_XX` to `B`, etc.
// This is enforced by dottyApp/Main.scala
class Z {
  def a_01(a: A with B): Unit = {}
  def b_02X(b: B with A): Unit = {}
  def a_02(a: A with B with A): Unit = {}
  def a_03(a: A with (B with A)): Unit = {}
  def b_04(b: A with (B with A) @foo): Unit = {}
  def b_04X(b: A with (B with C) @foo): Unit = {}
  def b_05(b: A with (B with A) @foo with (C with B with A) @foo): Unit = {}

  type T1 <: A with B
  def a_06(a: T1): Unit = {}

  type S <: B with T1
  def a_07(a: S): Unit = {}

  type T2 <: B with A
  type U <: T2 with S
  def b_08(b: U): Unit = {}

  val singB: B = new B {}
  def a_09(a: A with singB.type): Unit = {}
  def b_10(b: singB.type with A): Unit = {}

  type V >: SubB <: B
  def b_11(b: V): Unit = {}
  def b_12(b: V with SubB): Unit = {}

  def d_13(d: D with A): Unit = {}
  def d_14(d: A with D): Unit = {}

  val singD: D = new D {}
  def d_13x(d: singD.type with A): Unit = {}
  def d_14x(d: A with singD.type): Unit = {}

  type DEq = D
  def d_15(d: A with DEq): Unit = {}
  def d_16(d: A with (DEq @foo)): Unit = {}
  def d_17(d: DEq with A): Unit = {}
  def d_18(d: (DEq @foo) with A): Unit = {}

  val singDEq: DEq @foo = new D {}
  def d_15b(d: A with singDEq.type): Unit = {}
  def d_16b(d: A with (singDEq.type @foo)): Unit = {}

  type DSub <: D
  def a_19(a: A with DSub): Unit = {}
  def d_19x(d: DSub with A): Unit = {}
  def z_20(z: DSub with Z): Unit = {}

  type W1 <: A with Cov[Any]
  type X1 <: Cov[Int] with W1
  def a_21(a: X1): Unit = {}

  type W2 <: A with Cov[Any]
  type X2 <: Cov[Int] with W2
  def a_22(a: X2): Unit = {}

  def z_23(z: A with this.type): Unit = {}
  def z_24(z: this.type with A): Unit = {}

  def b_25(b: A with (B { type T })): Unit = {}
  def a_26(a: (A { type T }) with ((B with A) { type T })): Unit = {}

  def a_27(a: VC with B): Unit = {}
  def a_28(a: B with VC): Unit = {}

  val o1: Outer = new Outer
  val o2: Outer = new Outer
  def f_29(f: o1.E with o1.F): Unit = {}
  def f_30(f: o1.F with o1.E): Unit = {}
  def f_31(f: o1.E with o2.F): Unit = {}
  def f_32(f: o2.F with o1.E): Unit = {}
  def f_33(f: Outer#E with Outer#F): Unit = {}
  def f_34(f: Outer#F with Outer#E): Unit = {}

  val structural1: { type DSub <: D } = new { type DSub <: D }
  def a_35(a: A with structural1.DSub): Unit = {}
  def d_36(a: structural1.DSub with A): Unit = {}
  def z_37(z: Z with structural1.DSub): Unit = {}
  def z_38(z: structural1.DSub with Z): Unit = {}

  val structural2: { type SubCB <: C with B } = new { type SubCB <: C with B }
  def c_39(c: structural2.SubCB with B): Unit = {}
  def c_40(c: B with structural2.SubCB): Unit = {}

  val structural3a: { type SubB <: B; type SubCB <: C with SubB } = new { type SubB <: B; type SubCB <: C with SubB }
  val structural3b: { type SubB <: B; type SubCB <: C with SubB } = new { type SubB <: B; type SubCB <: C with SubB }
  def c_41(c: structural3a.SubB with structural3a.SubCB): Unit = {}
  def c_42(c: structural3a.SubCB with structural3a.SubB): Unit = {}
  def b_43(b: structural3a.SubB with structural3b.SubCB): Unit = {}
  def c_44(c: structural3b.SubCB with structural3a.SubB): Unit = {}

  type SubStructural <: C with structural3a.SubB
  def c_45(x: structural3a.SubB with SubStructural): Unit = {}
  def b_46(x: structural3b.SubB with SubStructural): Unit = {}
}
