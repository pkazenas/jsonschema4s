package pl.pkazenas.jsonschema4s.core.json

import spray.json._

package object model {

  abstract class JsPrimitive(val name: String)

  case object JsStringType extends JsPrimitive("string")
  case object JsObjectType extends JsPrimitive("object")
  case object JsNumberType extends JsPrimitive("number")
  case object JsBooleanType extends JsPrimitive("boolean")
  case object JsArrayType extends JsPrimitive("array")

  def stringField(field: String, value: String): JsField = (field, JsString(value))

  def objectField(field: String, jsFields: List[JsField]): JsField = (field, JsObject(jsFields: _*))

  def arrayObject(jsValueType: JsValue): JsValue =
    JsObject(typeField(JsArrayType), ("items", jsValueType))

  def anyOfObject(jsValues: List[JsValue]): JsValue =
    JsObject(("anyOf", JsArray(jsValues: _*)))

  def allOfField(jsValues: List[JsValue]): JsField = ("allOf", JsArray(jsValues: _*))

  def simpleObject(jsFields: List[JsField]): JsValue = JsObject(jsFields: _*)

  val schemaField = stringField("$schema", "http://json-schema.org/schema#")

  def schemaId(id: String): JsField = stringField("$id", s"$id")

  def typeField(jsonType: JsPrimitive): JsField = stringField("type", s"${jsonType.name}")

  def typeObject(jsonType: JsPrimitive): JsValue = JsObject(typeField(jsonType))

  def definitionReference(typeName: String): JsField = stringField("$ref", s"#/definitions/$typeName")

  def definitionReferenceObject(typeName: String): JsValue = JsObject(definitionReference(typeName))

  def definitions(jsFields: List[JsField]): JsField = objectField("definitions", jsFields)

  def properties(jsFields: List[JsField]): JsField = objectField("properties", jsFields)

  def requiredArray(fieldNames: List[String]): JsField = ("required", JsArray(fieldNames.map(JsString(_)): _*))

}
