package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.model._
import ReflectionUtils._
import pl.pkazenas.jsonschema4s.core.ModelExtractionException
import pl.pkazenas.jsonschema4s.annotation.Description
import scala.annotation.tailrec

object ModelUtils {
  def isOptionalType(typeDefinition: TypeDefinition) = typeDefinition.isInstanceOf[OptionalType]

  def isArrayType(typeDefinition: TypeDefinition) = typeDefinition.isInstanceOf[ArrayType]

  def findAllComplexTypes(rootType: RootType): List[ComplexType] = {
    def extractComplexTypes(typeDefinition: TypeDefinition): List[ComplexType] =
      typeDefinition match {
        case caseClassType: CaseClassType => List(caseClassType)
        case traitType: TraitType => traitType :: traitType.implementations
        case abstractClassType: AbstractClassType => abstractClassType :: abstractClassType.implementations
        case _ => List()
      }

    def extractWrappedTypes(typeDefinition: TypeDefinition): List[ComplexType] = {
      typeDefinition match {
        case optional@OptionalType(wrapped) => extractComplexTypes(wrapped)
        case array@ArrayType(wrapped) => extractComplexTypes(wrapped)
        case map@MapType(key, value) => extractComplexTypes(key) ++ extractComplexTypes(value)
        case _ => List()
      }
    }

    @tailrec
    def loop(fields: List[ClassField], accum: List[ComplexType] = List()): List[ComplexType] =
      if (fields.isEmpty) accum
      else {
        val complexTypes: List[ComplexType] =
          fields
            .flatMap(field => {
              field.typeDefinition match {
                case complexType: ComplexType => extractComplexTypes(complexType)
                case simpleType => extractWrappedTypes(simpleType)
              }
            })
            .distinct

        loop(complexTypes.collect { case c: CaseClassType => c }.flatMap(_.fields), accum ++ complexTypes)
      }

    loop(rootType.fields).distinct
  }

  implicit class ClassModelImplicits(classSymbol: ClassSymbol) {
    def classFields(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default) =
      classSymbol
        .primaryConstructorParams
        .map(_.map(symbol => symbol.toClassField))
        .getOrElse(List())

    def sealedTraitHierarchy(superTypeName: String)(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default) =
      classSymbol
        .knownDirectSubclasses
        .filter(_.isCaseClass)
        .map(symbol => CaseClassType(symbol.name.toString, symbol.asClass.classFields, Option(superTypeName)))
        .toList
  }

  object TypeNames {
    val booleanName = typeOf[Boolean].dealiasedFullName
    val byteName = typeOf[Byte].dealiasedFullName
    val charName = typeOf[Char].dealiasedFullName
    val shortName = typeOf[Short].dealiasedFullName
    val intName = typeOf[Int].dealiasedFullName
    val longName = typeOf[Long].dealiasedFullName
    val floatName = typeOf[Float].dealiasedFullName
    val doubleName = typeOf[Double].dealiasedFullName
    val stringName = typeOf[String].dealiasedFullName
    val optionName = typeOf[Option[_]].dealiasedFullName
    val arrayName = typeOf[Array[_]].dealiasedFullName
    val listName = typeOf[List[_]].dealiasedFullName
    val setName = typeOf[Set[_]].dealiasedFullName
    val mapName = typeOf[Map[_, _]].dealiasedFullName
  }

  import TypeNames._

  implicit class TypeModelImplicits(`type`: Type) {
    final def toTypeDefinition(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default): TypeDefinition = {
      val dealiasedType = `type`.dealias

      def matchComplexType = {
        // extract supported type annotations (right now it's only description)
        val typeAnnotations = dealiasedType.typeSymbol.getAnnotationsValues
        val description = typeAnnotations.description.map(_.value)

        dealiasedType match {
          // complex types
          case t if t.isCaseClass => CaseClassType(t.typeName, t.asClass.classFields, description)
          case t if t.isSealedTrait => TraitType(t.typeName, t.asClass.sealedTraitHierarchy(t.typeName), description)
          case t if t.isTrait => TraitType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName), description)
          case t if t.isAbstractClass => AbstractClassType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName), description)
          case t => throw new ModelExtractionException(s"Unsupported type encountered: ${t.typeSymbol.fullName}")
        }
      }

      def arrayFromType = ArrayType(dealiasedType.typeArgs(0).toTypeDefinition)

      val fullName = `type`.dealiasedFullName
      fullName match {
        case `booleanName` => BooleanType
        case `byteName` => ByteType
        case `charName` => CharType
        case `shortName` => ShortType
        case `intName` => IntType
        case `longName` => LongType
        case `floatName` => FloatType
        case `doubleName` => DoubleType
        case `stringName` => StringType
        case `optionName` => OptionalType(dealiasedType.typeArgs.head.toTypeDefinition)
        case `arrayName` => arrayFromType
        case `listName` => arrayFromType
        case `setName` => arrayFromType
        case `mapName` => MapType(dealiasedType.typeArgs(0).toTypeDefinition, dealiasedType.typeArgs(1).toTypeDefinition)
        case _ => matchComplexType
      }
    }
  }

  implicit class SymbolModelImplicits(symbol: Symbol) {
    def toClassField(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default): ClassField = {
      val annotations = symbol.getAnnotationsValues
      ClassField(
        name = symbol.name.toString,
        typeDefinition = symbol.typeSignature.toTypeDefinition,
        description = annotations.description.map(_.value))
    }
  }

}
