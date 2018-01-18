package pl.pkazenas.jsonschema4s.core

import org.scalatest.FunSuite
import pl.pkazenas.jsonschema4s.model._
import pl.pkazenas.jsonschema4s.test.testClasses.A
import pl.pkazenas.jsonschema4s.Api._
import pl.pkazenas.jsonschema4s.test.WrappedAbstracts

import scala.reflect.runtime.universe._

class ApiTest extends FunSuite {
  test("testClass \"A\" to ClassField") {
    val expected =
      RootType(
        "WrappedAbstracts",
        List(
          ClassField(
            "shape",
            TraitType(
              List(
                CaseClassType("Circle", List(ClassField("radius", IntType))),
                CaseClassType("Square", List(ClassField("size", IntType)))
              )
            )),
          ClassField(
            "plant",
            AbstractClassType(
              List(
                CaseClassType("Cactus", List(ClassField("needleCount", LongType))),
                CaseClassType("Pine", List(ClassField("height", IntType)))
              )
            )
          )
        ))

    implicit val apiConfig = ApiConfig(packagesToScan = List("pl.kazenas"))
    val actual = toModel(typeOf[WrappedAbstracts])
    assertResult(expected.name)(actual.name)
    assertResult(expected.fields(0).name)(actual.fields(0).name)
    assertResult(expected.fields(0).typeDefinition.asInstanceOf[TraitType].implementations.toSet){
      actual.fields(0).typeDefinition.asInstanceOf[TraitType].implementations.toSet
    }
    assertResult(expected.fields(1).name)(actual.fields(1).name)
    assertResult(expected.fields(1).typeDefinition.asInstanceOf[AbstractClassType].implementations.toSet){
      actual.fields(1).typeDefinition.asInstanceOf[AbstractClassType].implementations.toSet
    }
  }
}
