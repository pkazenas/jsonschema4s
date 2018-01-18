package pl.pkazenas.jsonschema4s.test

package object testClasses {

  class SimpleClass(a: String, b: Int)

  case class A(a: String, b: Int)

  case class B(c: Long)

  case class C(a: A, b: B)

  case class NestedClass(first: A, second: B, third: C)

  sealed trait Animal
  case class Dog(name: String) extends Animal
  case class Cat(owner: String) extends Animal
}



trait Shape
case class Square(size: Int) extends Shape
case class Circle(radius: Int) extends Shape


abstract class Plant
case class Pine(height: Int) extends Plant
case class Cactus(needleCount: Long) extends Plant

case class WrappedAbstracts(shape: Shape, plant: Plant)