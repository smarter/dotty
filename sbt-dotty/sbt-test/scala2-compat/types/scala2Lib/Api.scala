// package scala2Lib

class foo extends scala.annotation.StaticAnnotation

trait A
trait B
trait SubB extends B
trait C
trait Cov[+T]

class D

class E {
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

  val sing: B = null
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
  def e_20(e: DSub with E): Unit = {}

  type W1 <: A with Cov[Any]
  type X1 <: Cov[Int] with W1
  def a_21(a: X1): Unit = {}

  type W2 <: A with Cov[Any]
  type X2 <: Cov[Int] with W2
  def a_22(a: X2): Unit = {}

  def e_23(e: A with this.type): Unit = {}
  def e_24(e: this.type with A): Unit = {}

  // TODO: refinements, value classes, inner classes
  // class Outer {
  //   trait TraitX extends ClassY
  //   class ClassY
  // }
  // object Api {
  //   def foo(o1: Outer, o2: Outer): o1.ClassY with o2.TraitX = {}
  // }
}
