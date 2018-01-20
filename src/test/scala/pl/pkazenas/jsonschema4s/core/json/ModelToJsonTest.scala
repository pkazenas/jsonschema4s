package pl.pkazenas.jsonschema4s.core.json

import com.sun.org.apache.bcel.internal.util.ClassPath.ClassFile
import org.scalatest.{FunSuite, OneInstancePerTest}
import pl.pkazenas.jsonschema4s.core.json.ModelToJson._
import pl.pkazenas.jsonschema4s.model._
import spray.json._

class ModelToJsonTest extends FunSuite with OneInstancePerTest {
  test("typeDefinitionToJsValue - BooleanType") {
    assertResult(JsObject(("type", JsString("boolean"))))(typeDefinitionToJsValue(BooleanType))
  }

  test("typeDefinitionToJsValue - integral types") {
    val integralTypes = List(ByteType, CharType, ShortType, IntType, LongType)

    integralTypes
      .foreach(integralType => {
        assertResult(JsObject(("type", JsString("integer"))))(typeDefinitionToJsValue(integralType))
      })
  }

  test("typeDefinitionToJsValue - number types") {
    val numberTypes = List(FloatType, DoubleType)

    numberTypes
      .foreach(integralType => {
        assertResult(JsObject(("type", JsString("number"))))(typeDefinitionToJsValue(integralType))
      })
  }

  test("typeDefinitionToJsValue - string type") {
    assertResult(JsObject(("type", JsString("string"))))(typeDefinitionToJsValue(StringType))
  }

  test("typeDefinitionToJsValue - optional type - positive") {
    assertResult(JsObject(("type", JsString("string"))))(typeDefinitionToJsValue(OptionalType(StringType)))
  }

  test("typeDefinitionToJsValue - optional type - negative") {
    assertThrows[JsonSchemaException](typeDefinitionToJsValue(OptionalType(OptionalType(StringType))))
  }

  test("typeDefinitionToJsValue - array type") {
    assertResult {
      JsObject(("type", JsString("array")), ("items", JsObject(("type", JsString("string")))))
    } {
      typeDefinitionToJsValue(ArrayType(StringType))
    }
  }

  test("typeDefinitionToJsValue - map type") {
    // TODO:
  }

  test("typeDefinitionToJsValue - case class type") {
    assertResult(JsObject(("$ref", JsString("#/definitions/testType"))))(typeDefinitionToJsValue(CaseClassType("testType", List())))
  }

  test("typeDefinitionToJsValue - trait type") {
    assertResult(JsObject(("$ref", JsString("#/definitions/testType"))))(typeDefinitionToJsValue(TraitType("testType", List())))
  }

  test("typeDefinitionToJsValue - abstract class type") {
    assertResult(JsObject(("$ref", JsString("#/definitions/testType"))))(typeDefinitionToJsValue(AbstractClassType("testType", List())))
  }

  test("classFieldToJsField") {
    assertResult {
      ("testField", JsObject(("type", JsString("string"))))
    } {
      classFieldToJsField(ClassField("testField", StringType))
    }
  }

  test("classFieldsToPropertiesAndRequired") {
    val classFields =
      List(
        ClassField("a", StringType),
        ClassField("b", CaseClassType("B", List())),
        ClassField("c", OptionalType(IntType))
      )

    val (properties, requiredArray) = classFieldsToPropertiesAndRequired(classFields)

    // assert properties
    assertResult {
      ("properties",
        JsObject(
          ("a", JsObject(("type", JsString("string")))),
          ("b", JsObject(("$ref", JsString("#/definitions/B")))),
          ("c", JsObject(("type", JsString("integer"))))
        ))
    }(properties)

    // assert requiredArray
    assertResult(("required", JsArray(JsString("a"), JsString("b"))))(requiredArray)
  }

  test("nestedTypeToJsField") {
    val traitType =
      TraitType(
        "testTrait",
        List(
          CaseClassType("impl1", List()),
          CaseClassType("impl2", List())
        )
      )

    assertResult {
      ("testTrait",
        JsObject(
          ("anyOf",
            JsArray(
              JsObject(("$ref", JsString("#/definitions/impl1"))),
              JsObject(("$ref", JsString("#/definitions/impl2")))
            ))
        ))
    } {
      nestedTypeToJsField(traitType.typeName, traitType.implementations)
    }
  }

  test("caseClassToJsField - case class without super type") {
    val caseClassType =
      CaseClassType(
        "testClass",
        List(
          ClassField("a", StringType),
          ClassField("b", CaseClassType("c", List(ClassField("test", StringType)))),
          ClassField("d", OptionalType(IntType))
        )
      )

    assertResult {
      ("testClass",
        JsObject(
          ("type", JsString("object")),
          ("properties",
            JsObject(
              ("a", JsObject(("type", JsString("string")))),
              ("b", JsObject(("$ref", JsString("#/definitions/c")))),
              ("d", JsObject(("type", JsString("integer"))))
            )
          ),
          ("required", JsArray(JsString("a"), JsString("b")))
        ))
    } {
      caseClassToJsField(caseClassType)
    }
  }

  test("caseClassToJsField - case class with super type") {
    val caseClassType =
      CaseClassType(
        "testClass",
        List(
          ClassField("a", StringType),
          ClassField("b", CaseClassType("c", List(ClassField("test", StringType)))),
          ClassField("d", OptionalType(IntType))
        ),
        Some("superType")
      )

    assertResult {
      ("testClass",
        JsObject(
          ("type", JsString("object")),
          ("properties",
            JsObject(
              ("a", JsObject(("type", JsString("string")))),
              ("b", JsObject(("$ref", JsString("#/definitions/c")))),
              ("d", JsObject(("type", JsString("integer"))))
            )
          ),
          ("required", JsArray(JsString("a"), JsString("b"))),
          ("allOf", JsArray(JsObject(("$ref", JsString("#/definitions/superType")))))
        ))
    } {
      caseClassToJsField(caseClassType)
    }
  }
}
