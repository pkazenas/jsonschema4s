package pl.pkazenas.jsonschema4s.core.json

import pl.pkazenas.jsonschema4s.util.ModelUtils.{isArrayType, isOptionalType}
import pl.pkazenas.jsonschema4s.core.json.model._
import pl.pkazenas.jsonschema4s.model._
import spray.json.{JsField, JsValue}

object ModelToJson {
  def typeDefinitionToJsValue(typeDefinition: TypeDefinition): JsValue =
    typeDefinition match {
      case BooleanType => typeObject(JsBooleanType)
      case ByteType | CharType | ShortType | IntType => typeObject(JsIntegerType)
      case LongType => typeObjectWithFormat(JsIntegerType, Int64TypeFormat)
      case FloatType | DoubleType => typeObject(JsNumberType)
      case StringType => typeObject(JsStringType)
      case OptionalType(wrappedType) =>
        if (isOptionalType(wrappedType)) throw new JsonSchemaException("Nested Option[_] are not supported.")
        else typeDefinitionToJsValue(wrappedType)
      case ArrayType(typeDefinition) => arrayObject(typeDefinitionToJsValue(typeDefinition))
      case MapType(keyTypeDefinition, valueTypeDefinition) => ??? // TODO: figure out how to handle Maps
      case CaseClassType(typeName, _, _, _) => definitionReferenceObject(typeName)
      case TraitType(typeName, _, _) => definitionReferenceObject(typeName)
      case AbstractClassType(typeName, _, _) => definitionReferenceObject(typeName)
    }

  def classFieldToJsField(classField: ClassField): JsField = {
    val jsValue = typeDefinitionToJsValue(classField.typeDefinition).asJsObject
    val description = classField.description.map(descriptionField(_))

    val withDescription =
      description.fold(jsValue)(description => jsValue.copy(fields = jsValue.fields + description))

    (classField.name, withDescription)
  }

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

    val description =
      caseClassType
        .description
        .map(v => List(descriptionField(v)))
        .getOrElse(List())

    objectField(
      caseClassType.typeName,
      fields ++ superTypeRef ++ description)
  }

  def nestedTypeToJsField(typeName: String, implementations: List[CaseClassType]): JsField =
    (typeName, anyOfObject(implementations.map(typeDefinitionToJsValue(_))))

  def classFieldsToPropertiesAndRequired(classFields: List[ClassField]): (JsField, JsField) = {
    val jsFields = classFields.map(classFieldToJsField _)
    val requiredFieldsArray =
      requiredArray(classFields.filter(cf => !isOptionalType(cf.typeDefinition)).map(_.name))

    (objectField("properties", jsFields), requiredFieldsArray)
  }
}
