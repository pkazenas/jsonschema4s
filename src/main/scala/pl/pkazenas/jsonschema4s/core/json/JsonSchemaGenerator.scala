package pl.pkazenas.jsonschema4s.core.json

import pl.pkazenas.jsonschema4s.model._
import pl.pkazenas.jsonschema4s.core.json.model._
import pl.pkazenas.jsonschema4s.util.ModelUtils
import spray.json._
import pl.pkazenas.jsonschema4s.core.json.ModelToJson._

import scala.annotation.tailrec

object JsonSchemaGenerator {
  def generate(rootType: RootType): JsValue = {
    val (properties, requiredTypesArray) = classFieldsToPropertiesAndRequired(rootType.fields)
    val typesDefinitions =
      ModelUtils
        .findAllComplexTypes(rootType)
        .map(complexType => complexType match {
          case caseClassType: CaseClassType => caseClassToJsField(caseClassType)
          case TraitType(typeName, implementations, description) => nestedTypeToJsField(typeName, implementations)
          case AbstractClassType(typeName, implementations, description) => nestedTypeToJsField(typeName, implementations)
        })

    val description =
      rootType.description
        .map(descriptionField(_))
        .fold[List[JsField]](List())(List(_))

    simpleObject(
      List(
        schemaField,
        schemaId(rootType.name),
        schemaTitle(rootType.name),
        typeField(JsObjectType),
        properties,
        requiredTypesArray,
        objectField("definitions", typesDefinitions)
      ) ++ description
    )
  }
}
