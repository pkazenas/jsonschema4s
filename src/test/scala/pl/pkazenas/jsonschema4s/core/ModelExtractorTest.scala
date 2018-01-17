package pl.pkazenas.jsonschema4s.core

import org.scalatest.{FunSuite, OneInstancePerTest}
import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._
import pl.pkazenas.jsonschema4s.model._

class ModelExtractorTest extends FunSuite with OneInstancePerTest {
  test("testClass \"A\" to ClassField") {
    val extracted = ModelExtractor.extract(typeOf[A])
    assertResult(RootType("A", List(ClassField("a", StringType), ClassField("b", IntType))))(extracted)
  }
}
