package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.model._
import ReflectionUtils._
import pl.pkazenas.jsonschema4s.core.ModelExtractionException

object ModelUtils {
  implicit class ClassModelImplicits(classSymbol: ClassSymbol) {
    def classFields =
      classSymbol
        .primaryConstructorParams
        .map(_.map(symbol => symbol.toClassField))
        .getOrElse(List())

    def sealedTraitHierarchy =
      classSymbol
        .knownDirectSubclasses
        .filter(_.isCaseClass)
        .map(symbol => CaseClassType(symbol.name.toString, symbol.asClass.classFields))
        .toList
  }

  implicit class TypeModelImplicits(`type`: Type) {
    final def toTypeDefinition(implicit classPathScanner: ClasspathScanner = ClasspathScanner.default): TypeDefinition = {
      `type` match {
        // primitive types
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
        //collection types, // TODO: implement this
        // complex types
        case t if t.isCaseClass => CaseClassType(t.typeSymbol.name.toString, t.asClass.classFields) // TODO:
        case t if t.isSealedTrait => TraitType(t.asClass.sealedTraitHierarchy)
        case t if t.isTrait => TraitType(HierarchyExtractor.findSubclasses(t))
        case t if t.isAbstractClass => AbstractClassType(HierarchyExtractor.findSubclasses(t))
        case t => throw new ModelExtractionException(s"Unsupported type encountered: ${t.typeSymbol.fullName}")
      }
    }
  }

  implicit class SymbolModelImplicits(symbol: Symbol) {
    def toClassField: ClassField = {
      ClassField(symbol.name.toString, symbol.typeSignature.toTypeDefinition)
    }
  }

}
