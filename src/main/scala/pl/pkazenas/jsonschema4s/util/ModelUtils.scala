package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.model._
import ReflectionUtils._
import pl.pkazenas.jsonschema4s.core.ModelExtractionException

import scala.annotation.tailrec

object ModelUtils {
  def isOptionalType(typeDefinition: TypeDefinition) = typeDefinition.isInstanceOf[OptionalType]

  def isArrayType(typeDefinition: TypeDefinition) = typeDefinition.isInstanceOf[ArrayType]

  def findAllComplexTypes(rootType: RootType): List[ComplexType] = {
    @tailrec
    def loop(fields: List[ClassField], accum: List[ComplexType] = List()): List[ComplexType] =
      if (fields.isEmpty) accum
      else {
        val complexTypes: List[ComplexType] =
          fields
            .flatMap(field => {
              field.typeDefinition match {
                case caseClassType: CaseClassType => List(caseClassType)
                case traitType: TraitType => traitType :: traitType.implementations
                case abstractClassType: AbstractClassType => abstractClassType :: abstractClassType.implementations
                case _ => List()
              }
            })
            .distinct

        loop(complexTypes.collect{ case c: CaseClassType => c}.flatMap(_.fields), accum ++ complexTypes)
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

  implicit class TypeModelImplicits(`type`: Type) {
    final def toTypeDefinition(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default): TypeDefinition = {
      `type` match {
        // primitive types
        case t if t =:= typeOf[Boolean] => BooleanType
        case t if t =:= typeOf[Byte] => ByteType
        case t if t =:= typeOf[Char] => CharType
        case t if t =:= typeOf[Short] => ShortType
        case t if t =:= typeOf[Int] => IntType
        case t if t =:= typeOf[Long] => LongType
        case t if t =:= typeOf[Float] => FloatType
        case t if t =:= typeOf[Double] => DoubleType
        // other builtin types
        case t if t =:= typeOf[String] => StringType
        case t if t <:< typeOf[Option[_]] => OptionalType(t.dealias.typeArgs.head.toTypeDefinition)
        //collection types
        case t if t <:< typeOf[Array[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
        case t if t <:< typeOf[Seq[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
        case t if t <:< typeOf[Set[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
        case t if t <:< typeOf[Map[_, _]] => MapType(t.dealiasedTypeArg(0).toTypeDefinition, t.dealiasedTypeArg(1).toTypeDefinition)
        // complex types
        case t if t.isCaseClass => CaseClassType(t.typeName, t.asClass.classFields)
        case t if t.isSealedTrait => TraitType(t.typeName, t.asClass.sealedTraitHierarchy(t.typeName))
        case t if t.isTrait => TraitType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName))
        case t if t.isAbstractClass => AbstractClassType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName))
        case t => throw new ModelExtractionException(s"Unsupported type encountered: ${t.typeSymbol.fullName}")
      }
    }
  }

  implicit class SymbolModelImplicits(symbol: Symbol) {
    def toClassField(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default): ClassField = {
      ClassField(symbol.name.toString, symbol.typeSignature.toTypeDefinition)
    }
  }

}
