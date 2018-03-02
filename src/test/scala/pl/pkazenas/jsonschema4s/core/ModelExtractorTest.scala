package pl.pkazenas.jsonschema4s.core

import org.scalatest.{FunSuite, OneInstancePerTest}

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._
import pl.pkazenas.jsonschema4s.model._
import pl.pkazenas.jsonschema4s.test.{Request, WrappedAbstracts}
import pl.pkazenas.jsonschema4s.util.ModelUtils._

class ModelExtractorTest extends FunSuite with OneInstancePerTest {
  test("testClass \"A\" to RootType") {
    val extracted = ModelExtractor.extract(typeOf[A])
    assertResult(RootType("A", List(ClassField("a", StringType), ClassField("b", IntType))))(extracted)
  }

  test("extract class with description annotations") {
    val extracted = ModelExtractor.extract(typeOf[DescriptionTest])

    val expected =
      RootType(
        name = "DescriptionTest",
        description = Some("test class"),
        fields =
          List(
            ClassField("string", StringType, description = Some("test string")),
            ClassField("int", IntType, description = Some("test int"))
          )
      )

    assertResult(expected)(extracted)
  }

  test("extract class with description annotations - nested type") {
    val extracted = ModelExtractor.extract(typeOf[Request])

    val expected =
      RootType(
        name = "Request",
        description = Some("request"),
        fields =
          List(
            ClassField(
              "data",
              AbstractClassType(
                "DataType",
                implementations =
                  List(
                    CaseClassType("Data2", fields = List(ClassField("value", IntType, description = Some("value"))), description = Some("data 2"), superTypeName = Some("DataType")),
                    CaseClassType("Data1", fields = List(ClassField("name", StringType, description = Some("name"))), description = Some("data 1"), superTypeName = Some("DataType"))
                  ),
                description = Some("data type")
              )
            )
          )
      )

    def abstractClassImplementations(classField: ClassField) =
      classField.typeDefinition.asInstanceOf[AbstractClassType].implementations.toSet

    assertResult(expected.name)(extracted.name)
    assertResult(expected.description)(extracted.description)
    assertResult(abstractClassImplementations(expected.fields(0)))(abstractClassImplementations(extracted.fields(0)))
  }
}
