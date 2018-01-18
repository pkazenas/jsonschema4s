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
  case class CaseClassType(typeName: String,
                           fields: List[ClassField]) extends TypeDefinition

  case class TraitType(implementations: List[CaseClassType]) extends TypeDefinition
  case class AbstractClassType(implementations: List[CaseClassType]) extends TypeDefinition

  // root model type
  case class RootType(name: String,
                      fields: List[ClassField])
}
