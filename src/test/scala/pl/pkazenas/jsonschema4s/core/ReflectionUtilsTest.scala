package pl.pkazenas.jsonschema4s.core

import org.scalatest.{FunSuite, OneInstancePerTest}
import pl.pkazenas.jsonschema4s.annotation.Description

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._
import pl.pkazenas.jsonschema4s.util.ReflectionUtils._
import pl.pkazenas.jsonschema4s.util.ModelUtils._



@Description("test")
case class ABC(@Description("a field")
               a: String)

class ReflectionUtilsTest extends FunSuite with OneInstancePerTest {
  test("isCaseClass test") {
    assert(typeOf[A].typeSymbol.isCaseClass)
    assert(!typeOf[SimpleClass].typeSymbol.isCaseClass)
    assert(!typeOf[Int].typeSymbol.isCaseClass)
  }

  test("annotation extraction test") {
    val annotations = typeOf[ABC].typeSymbol.getAnnotationsValues
    assertResult(Some(Description("test")))(annotations.description)
  }
}
