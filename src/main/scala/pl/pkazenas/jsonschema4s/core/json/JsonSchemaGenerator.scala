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
      case CaseClassType(typeName, _, _) => definitionReferenceObject(typeName)
      case TraitType(typeName, _) => definitionReferenceObject(typeName)
      case AbstractClassType(typeName, _) => definitionReferenceObject(typeName)
    }

  def classFieldToJsField(classField: ClassField): JsField =
    (classField.name, typeDefinitionToJsValue(classField.typeDefinition))

  def caseClassToJsField(caseClassType: CaseClassType): JsField = {
    val (properties, requiredTypesArray) = classFieldsToPropertiesAndRequired(caseClassType.fields)
    val superTypeRef: List[JsField] =
      caseClassType
        .superTypeName
        .map(superTypeName => allOfField(List(definitionReferenceObject(superTypeName))))
        .fold(List[JsField]())(t => List(t))

    val fields =
      List(
        typeField(JsObjectType),
        properties,
        requiredTypesArray)

    objectField(
      caseClassType.typeName,
      fields ++ superTypeRef)
  }

  def nestedTypeToJsField(typeName: String, implementations: List[CaseClassType]): JsField =
    (typeName, anyOfObject(implementations.map(typeDefinitionToJsValue(_))))

  def classFieldsToPropertiesAndRequired(classFields: List[ClassField]): (JsField, JsField) ={
    val jsFields = classFields.map(classFieldToJsField _)
    val requiredFieldsArray =
      requiredArray(classFields.filter(cf => !isOptionalType(cf.typeDefinition)).map(_.name))

    (objectField("properties", jsFields), requiredFieldsArray)
  }

  def generate(rootType: RootType): JsValue = {
    val (properties, requiredTypesArray) = classFieldsToPropertiesAndRequired(rootType.fields)
    val typesDefinitions =
      ModelUtils
        .findAllComplexTypes(rootType)
        .map(complexType => complexType match {
          case caseClassType: CaseClassType => caseClassToJsField(caseClassType)
          case TraitType(typeName, implementations) => nestedTypeToJsField(typeName, implementations)
          case AbstractClassType(typeName, implementations) => nestedTypeToJsField(typeName, implementations)
        })

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
