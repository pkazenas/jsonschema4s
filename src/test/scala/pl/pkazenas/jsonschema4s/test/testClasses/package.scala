package pl.pkazenas.jsonschema4s.test

package object testClasses {
  class SimpleClass(a: String, b: Int)
  case class A(a: String, b: Int)
  case class B(c: Long)
  case class C(a: A, b: B)

  case class NestedClass(first: A, second: B, third: C)
}
