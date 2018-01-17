package pl.pkazenas.jsonschema4s.core

import org.scalatest.{FunSuite, OneInstancePerTest}
import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._
import pl.pkazenas.jsonschema4s.util.ReflectionUtils._

class ReflectionUtilsTest extends FunSuite with OneInstancePerTest {
  test("isCaseClass test") {
    assert(typeOf[A].typeSymbol.isCaseClass)
    assert(!typeOf[SimpleClass].typeSymbol.isCaseClass)
    assert(!typeOf[Int].typeSymbol.isCaseClass)
  }
}
