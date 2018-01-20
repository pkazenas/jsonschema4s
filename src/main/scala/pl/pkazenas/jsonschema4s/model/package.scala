package pl.pkazenas.jsonschema4s

package object model {

  sealed trait TypeDefinition

  case class ClassField(name: String,
                        typeDefinition: TypeDefinition)

  // simple types
  case object BooleanType extends TypeDefinition
  case object ByteType extends TypeDefinition
  case object CharType extends TypeDefinition
  case object ShortType extends TypeDefinition
  case object IntType extends TypeDefinition
  case object LongType extends TypeDefinition
  case object DoubleType extends TypeDefinition
  case object FloatType extends TypeDefinition
  case object StringType extends TypeDefinition

  // Other types
  case class OptionalType(wrappedType: TypeDefinition) extends TypeDefinition
  case class ArrayType(typeDefinition: TypeDefinition) extends TypeDefinition
  case class MapType(keyTypeDefinition: TypeDefinition, valueTypeDefinition: TypeDefinition) extends TypeDefinition

  // Complex types
  sealed trait ComplexType extends TypeDefinition

  case class CaseClassType(typeName: String,
                           fields: List[ClassField],
                           superTypeName: Option[String] = None) extends ComplexType

  case class TraitType(typeName: String,
                       implementations: List[CaseClassType]) extends ComplexType
  case class AbstractClassType(typeName: String,
                               implementations: List[CaseClassType]) extends ComplexType

  // root model type
  case class RootType(name: String,
                      fields: List[ClassField])
}
