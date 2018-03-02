package pl.pkazenas.jsonschema4s.core.json

import org.scalatest.{FunSuite, OneInstancePerTest}
import pl.pkazenas.jsonschema4s.model.RootType
import pl.pkazenas.jsonschema4s.model._
import spray.json._

class JsonSchemaGeneratorTest extends FunSuite with OneInstancePerTest {
  test("generate - simple type") {
    val simpleType =
      RootType(
        "simpleType",
        List(
          ClassField("a", StringType),
          ClassField("b", ArrayType(LongType)),
          ClassField("c", OptionalType(BooleanType))
        )
      )

    val expected =
      JsObject(
        ("$id", JsString("simpleType")),
        ("title", JsString("simpleType")),
        ("$schema", JsString("http://json-schema.org/schema#")),
        ("type", JsString("object")),
        ("properties",
          JsObject(
            ("a", JsObject(("type", JsString("string")))),
            ("b", JsObject(("type", JsString("array")), ("items", JsObject(("type", JsString("integer")), ("format", JsString("int64")))))),
            ("c", JsObject(("type", JsString("boolean"))))
          )
        ),
        ("required", JsArray(JsString("a"), JsString("b"))),
        ("definitions", JsObject())
      )

    assertResult(expected)(JsonSchemaGenerator.generate(simpleType))
  }

  test("generate - complex type") {
    val complexType =
      RootType(
        "complexType",
        List(
          ClassField("owner", StringType),
          ClassField("animal",
            TraitType("Animal",
              List(
                CaseClassType("Dog", List(ClassField("name", StringType)), Some("Animal")),
                CaseClassType("Cat", List(ClassField("lives", LongType)), Some("Animal"))
              )
            )
          )
        )
      )

    val expected =
      JsObject(
        ("$id", JsString("complexType")),
        ("title", JsString("complexType")),
        ("$schema", JsString("http://json-schema.org/schema#")),
        ("type", JsString("object")),
        ("properties",
          JsObject(
            ("owner", JsObject(("type", JsString("string")))),
            ("animal", JsObject(("$ref", JsString("#/definitions/Animal"))))
          )
        ),
        ("required", JsArray(JsString("owner"), JsString("animal"))),
        ("definitions",
          JsObject(
            ("Animal",
              JsObject(
                ("anyOf",
                  JsArray(
                    JsObject(("$ref", JsString("#/definitions/Dog"))),
                    JsObject(("$ref", JsString("#/definitions/Cat")))
                  )
                )
              )
            ),
            ("Dog",
              JsObject(
                ("type", JsString("object")),
                ("properties",
                  JsObject(
                    ("name", JsObject(("type", JsString("string"))))
                  )
                ),
                ("required", JsArray(JsString("name"))),
                ("allOf", JsArray(JsObject(("$ref", JsString("#/definitions/Animal")))))
              )
            ),
            ("Cat",
              JsObject(
                ("type", JsString("object")),
                ("properties",
                  JsObject(
                    ("lives", JsObject(("type", JsString("integer")), ("format", JsString("int64"))))
                  )
                ),
                ("required", JsArray(JsString("lives"))),
                ("allOf", JsArray(JsObject(("$ref", JsString("#/definitions/Animal")))))
              )
            )
          )
        )
      )

    val actual = JsonSchemaGenerator.generate(complexType)
    assertResult(expected)(actual)
  }
}
