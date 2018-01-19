package pl.pkazenas.jsonschema4s.core.json

import spray.json._

object JsonSchemaPrinter {
  def apply(schema: JsValue): String =  schema.prettyPrint
}
