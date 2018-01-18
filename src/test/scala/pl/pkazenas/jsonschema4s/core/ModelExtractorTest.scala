package pl.pkazenas.jsonschema4s.core

import org.scalatest.{FunSuite, OneInstancePerTest}

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._
import pl.pkazenas.jsonschema4s.model._
import pl.pkazenas.jsonschema4s.test.WrappedAbstracts
import pl.pkazenas.jsonschema4s.util.ModelUtils._

class ModelExtractorTest extends FunSuite with OneInstancePerTest {
  test("testClass \"A\" to RootType") {
    val extracted = ModelExtractor.extract(typeOf[A])
    assertResult(RootType("A", List(ClassField("a", StringType), ClassField("b", IntType))))(extracted)
  }
}
