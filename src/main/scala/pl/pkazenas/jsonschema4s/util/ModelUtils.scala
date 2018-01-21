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
        case t if t <:< typeOf[scala.Boolean] => BooleanType
        case t if t <:< typeOf[scala.Byte] => ByteType
        case t if t <:< typeOf[scala.Char] => CharType
        case t if t <:< typeOf[scala.Short] => ShortType
        case t if t <:< typeOf[scala.Int] => IntType
        case t if t <:< typeOf[scala.Long] => LongType
        case t if t <:< typeOf[scala.Float] => FloatType
        case t if t <:< typeOf[scala.Double] => DoubleType
        // other builtin types
        case t if t <:< typeOf[String] => StringType
        case t if t <:< typeOf[scala.Option[_]] => OptionalType(t.dealias.typeArgs.head.toTypeDefinition)
        //collection types
        case t if t <:< typeOf[scala.Array[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
        case t if t <:< typeOf[scala.Seq[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
        case t if t <:< typeOf[scala.collection.Set[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
        case t if t <:< typeOf[scala.collection.Map[_, _]] => MapType(t.dealiasedTypeArg(0).toTypeDefinition, t.dealiasedTypeArg(1).toTypeDefinition)
        // complex types
        case t if t.isCaseClass => CaseClassType(t.typeName, t.asClass.classFields)
        case t if t.isSealedTrait => TraitType(t.typeName, t.asClass.sealedTraitHierarchy(t.typeName))
        case t if t.isTrait => TraitType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName))
        case t if t.isAbstractClass => AbstractClassType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName))
        case t => throw new ModelExtractionException(s"Unsupported type encountered: ${t.typeSymbol.fullName}")
      }
    }

//    final def toTypeDefinition(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default): TypeDefinition = {
////      def convertOtherTypes()
//      val dealiasedType = `type`.dealias
//      dealiasedType.typeFullName match {
//        // primitive types
//        case `booleanType` => BooleanType
//        case `byteType` => ByteType
//        case `charType` => CharType
//        case `shortType` => ShortType
//        case `intType` => IntType
//        case `longType` => LongType
//        case `floatType` => FloatType
//        case `doubleType` => DoubleType
//        // other builtin types
////        case t if t <:< typeOf[String] => StringType
////        case t if t <:< typeOf[scala.Option[_]] => OptionalType(t.dealias.typeArgs.head.toTypeDefinition)
////        //collection types
////        case t if t <:< typeOf[scala.Array[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
////        case t if t <:< typeOf[scala.Seq[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
////        case t if t <:< typeOf[scala.collection.Set[_]] => ArrayType(t.dealiasedTypeArg(0).toTypeDefinition)
////        case t if t <:< typeOf[scala.collection.Map[_, _]] => MapType(t.dealiasedTypeArg(0).toTypeDefinition, t.dealiasedTypeArg(1).toTypeDefinition)
////        // complex types
////        case t if t.isCaseClass => CaseClassType(t.typeName, t.asClass.classFields)
////        case t if t.isSealedTrait => TraitType(t.typeName, t.asClass.sealedTraitHierarchy(t.typeName))
////        case t if t.isTrait => TraitType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName))
////        case t if t.isAbstractClass => AbstractClassType(t.typeName, HierarchyExtractor.extractSubclasses(t, t.typeName))
//        case t => throw new ModelExtractionException(s"Unsupported type encountered: ${dealiasedType.typeFullName}")
//      }
//    }
  }

  implicit class SymbolModelImplicits(symbol: Symbol) {
    def toClassField(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default): ClassField = {
      ClassField(symbol.name.toString, symbol.typeSignature.toTypeDefinition)
    }
  }

}
