// Keep synchronized with ../dottyApp/Api.scala
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

class Z {
  def a_01(a: A with B): Unit = {}
  def a_02(a: A with B with A): Unit = {}
  def a_03(a: A with (B with A)): Unit = {}
  def b_04(b: A with (B with A) @foo): Unit = {}
  def b_05(b: A with (B with A) @foo with (C with B with A) @foo): Unit = {}

  type T1 <: A with B
  def a_06(a: T1): Unit = {}

  type S <: B with T1
  def a_07(a: S): Unit = {}

  type T2 <: B with A
  type U <: T2 with S
  def b_08(b: U): Unit = {}

  val sing: B = new B {}
  def a_09(a: A with sing.type): Unit = {}
  def b_10(b: sing.type with A): Unit = {}

  type V >: SubB <: B
  def b_11(b: V): Unit = {}
  def b_12(b: V with SubB): Unit = {}

  def d_13(d: D with A): Unit = {}
  def d_14(d: A with D): Unit = {}

  type DEq = D
  def d_15(d: A with DEq): Unit = {}
  def d_16(d: A with (DEq @foo)): Unit = {}
  def d_17(d: DEq with A): Unit = {}
  def d_18(d: (DEq @foo) with A): Unit = {}

  type DSub <: D
  def a_19(a: A with DSub): Unit = {}
  def z_20(e: DSub with Z): Unit = {}

  type W1 <: A with Cov[Any]
  type X1 <: Cov[Int] with W1
  def a_21(a: X1): Unit = {}

  type W2 <: A with Cov[Any]
  type X2 <: Cov[Int] with W2
  def a_22(a: X2): Unit = {}

  def z_23(e: A with this.type): Unit = {}
  def z_24(e: this.type with A): Unit = {}

  def b_25(b: A with (B { type T })): Unit = {}
  def a_26(a: (A { type T }) with ((B with A) { type T })): Unit = {}

  def a_27(a: VC with B): Unit = {}
  def a_28(a: B with VC): Unit = {}

  val o1: Outer = new Outer
  val o2: Outer = new Outer
  def f_29(a: o1.E with o1.F): Unit = {}
  def f_30(a: o1.F with o1.E): Unit = {}
  def f_31(a: o1.E with o2.F): Unit = {}
  def f_32(a: o2.F with o1.E): Unit = {}
  def f_33(a: Outer#E with Outer#F): Unit = {}
  def f_34(a: Outer#F with Outer#E): Unit = {}
}
