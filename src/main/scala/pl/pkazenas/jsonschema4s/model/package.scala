package pl.pkazenas.jsonschema4s

package object model {

  sealed trait TypeDefinition

  case class ClassField(name: String, typeDefinition: TypeDefinition)

  // simple types
  case object ByteType extends TypeDefinition
  case object IntType extends TypeDefinition
  case object LongType extends TypeDefinition
  case object DoubleType extends TypeDefinition
  case object FloatType extends TypeDefinition
  case object StringType extends TypeDefinition
  case class ArrayType(typeDefinition: TypeDefinition) extends TypeDefinition
  case class MapType(keyTypeDefinition: TypeDefinition, valueTypeDefinition: TypeDefinition) extends TypeDefinition

  // Complex types
  case class CaseClassType(typeName: String,
                           fields: List[ClassField]) extends TypeDefinition

  case class TraitType(implementations: List[CaseClassType])
  case class AbstractClassType(implementations: List[CaseClassType])

  case class RootType(name: String,
                      description: String = "",
                      dependentTypes: List[ClassField])
}
