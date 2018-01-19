package pl.pkazenas.jsonschema4s.core.json

import pl.pkazenas.jsonschema4s.model._
import pl.pkazenas.jsonschema4s.core.json.model._
import pl.pkazenas.jsonschema4s.util.ModelUtils
import spray.json._

import scala.annotation.tailrec

object JsonSchemaGenerator {

  def isOptionalType(typeDefinition: TypeDefinition) = typeDefinition.isInstanceOf[OptionalType]

  def isArrayType(typeDefinition: TypeDefinition) = typeDefinition.isInstanceOf[TypeDefinition]

  def typeDefinitionToJsValue(typeDefinition: TypeDefinition): JsValue =
    typeDefinition match {
      case BooleanType => typeObject(JsBooleanType)
      case ByteType | CharType | ShortType | IntType | LongType | FloatType | DoubleType => typeObject(JsNumberType)
      case StringType => typeObject(JsStringType)
      case OptionalType(wrappedType) =>
        if (isOptionalType(wrappedType)) throw new JsonSchemaException("Nested Option[_] are not supported.")
        else typeDefinitionToJsValue(typeDefinition)
      case ArrayType(typeDefinition) =>
        if (isArrayType(typeDefinition)) throw new JsonSchemaException("")
        else arrayObject(typeDefinitionToJsValue(typeDefinition))
      case MapType(keyTypeDefinition, valueTypeDefinition) => ??? // TODO: figure out how to handle Maps
      case CaseClassType(typeName, _) => definitionReferenceObject(typeName)
      case TraitType(implementations) => anyOfObject(implementations.map(typeDefinitionToJsValue(_)))
      case AbstractClassType(implementations) => anyOfObject(implementations.map(typeDefinitionToJsValue(_)))
    }

  def classFieldToJsField(classField: ClassField): JsField =
    (classField.name, typeDefinitionToJsValue(classField.typeDefinition))

  def caseClassToJsField(caseClassType: CaseClassType): JsField = {
    val (properties, requiredTypesArray) = classFieldsToPropertiesAndRequired(caseClassType.fields)

    objectField(
      caseClassType.typeName,
      List(
        typeField(JsObjectType),
        properties,
        requiredTypesArray
      ))
  }

  def classFieldsToPropertiesAndRequired(classFields: List[ClassField]): (JsField, JsField) ={
    val jsFields = classFields.map(classFieldToJsField _)
    val requiredFieldsArray =
      requiredArray(classFields.filter(cf => !isOptionalType(cf.typeDefinition)).map(_.name))

    (objectField("properties", jsFields), requiredFieldsArray)
  }

  def generate(rootType: RootType): JsValue = {
    val (properties, requiredTypesArray) = classFieldsToPropertiesAndRequired(rootType.fields)
    val typesDefinitions = ModelUtils.findAllCaseClassTypes(rootType).map(caseClassToJsField _)

    simpleObject(
      List(
        schemaField,
        schemaId(rootType.name),
        typeField(JsObjectType),
        properties,
        requiredTypesArray,
        objectField("definitions", typesDefinitions)
      )
    )
  }
}
