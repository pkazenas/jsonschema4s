package pl.pkazenas.jsonschema4s.test

import pl.pkazenas.jsonschema4s.annotation._

package object testClasses {

  class SimpleClass(a: String, b: Int)

  case class A(a: String, b: Int)

  case class B(c: Long)

  case class C(a: A, b: B)

  case class NestedClass(first: A, second: B, third: C)

  sealed trait Animal

  case class Dog(name: String) extends Animal

  case class Cat(owner: String) extends Animal

  case class CollectionClass(array: Array[B], list: List[String], set: Set[Int], map: Map[String, String])

  @Description("test class")
  case class DescriptionTest(@Description("test string")
                             string: String,
                             @Description("test int")
                             int: Int)

}

trait Shape

case class Square(size: Int) extends Shape

case class Circle(radius: Int) extends Shape

abstract class Plant

case class Pine(height: Int) extends Plant

case class Cactus(needleCount: Long) extends Plant

case class WrappedAbstracts(shape: Shape, plant: Plant)
