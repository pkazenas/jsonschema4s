package pl.pkazenas.jsonschema4s.core.json

import org.scalatest.{FunSuite, OneInstancePerTest}
import pl.pkazenas.jsonschema4s.core.json.model._
import spray.json._

class JsonUtilTest extends FunSuite with OneInstancePerTest {
  test("stringField") {
    assertResult(("testField", JsString("testString")))(stringField("testField", "testString"))
  }

  test("objectField") {
    val testStringField = ("string", JsString("testString"))
    val testIntField = ("int", JsNumber(10))
    assertResult {
      ("testField", JsObject(testStringField, testIntField))
    } {
      objectField("testField", List(testStringField, testIntField))
    }
  }

  test("arrayObject") {
    val stringTypeObject = JsObject(("type", JsString("string")))

    assertResult {
      JsObject(("type", JsString("array")), ("items", stringTypeObject))
    } {
      arrayObject(stringTypeObject)
    }
  }

  test("anyOfObject") {
    val refTypeObject1 = JsObject(("$ref", JsString("#/definitions/type1")))
    val refTypeObject2 = JsObject(("$ref", JsString("#/definitions/type2")))

    assertResult {
      JsObject(
        ("anyOf", JsArray(refTypeObject1, refTypeObject2))
      )
    } {
      anyOfObject(List(refTypeObject1, refTypeObject2))
    }
  }

  test("allOfField") {
    val refTypeObject1 = JsObject(("$ref", JsString("#/definitions/type1")))
    assertResult {
      ("allOf", JsArray(refTypeObject1))
    } {
      allOfField(List(refTypeObject1))
    }
  }

  test("schemaId") {
    assertResult(("$id", JsString("testId")))(schemaId("testId"))
  }

  test("typeField - each js primitive") {
    assertResult(("type", JsString(JsStringType.name)))(typeField(JsStringType))
    assertResult(("type", JsString(JsObjectType.name)))(typeField(JsObjectType))
    assertResult(("type", JsString(JsNumberType.name)))(typeField(JsNumberType))
    assertResult(("type", JsString(JsBooleanType.name)))(typeField(JsBooleanType))
    assertResult(("type", JsString(JsArrayType.name)))(typeField(JsArrayType))
    assertResult(("type", JsString(JsIntegerType.name)))(typeField(JsIntegerType))
  }

  test("definitionReference") {
    assertResult(("$ref", JsString("#/definitions/type1")))(definitionReference("type1"))
  }

  test("definitionReferenceObject") {
    assertResult(JsObject(("$ref", JsString("#/definitions/type1"))))(definitionReferenceObject("type1"))
  }

  test("definitions") {
    val typeDefinition1 =
      ("type1", JsObject(("field1", JsObject(("type", JsString("string"))))))

    val typeDefinition2 =
      ("type2", JsObject(("field2", JsObject(("type", JsString("number"))))))

    assertResult {
      ("definitions", JsObject(typeDefinition1, typeDefinition2))
    } {
      definitions(List(typeDefinition1, typeDefinition2))
    }
  }

  test("properties") {
    val property1 = ("test1", JsObject(("type", JsString("string"))))
    val property2 = ("test2", JsObject(("type", JsString("number"))))
    assertResult{
      ("properties", JsObject(property1, property2))
    } {
      properties(List(property1, property2))
    }
  }

  test("requiredArray") {
    val fieldsArray = List("dog", "cat", "cookie")

    assertResult {
      ("required", JsArray(fieldsArray.map(JsString(_)): _*))
    } {
      requiredArray(fieldsArray)
    }
  }

}
