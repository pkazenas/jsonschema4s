package pl.pkazenas.jsonschema4s.util

import pl.pkazenas.jsonschema4s.annotation.{AnnotationsValues, Description}

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

object ReflectionUtils {

  implicit class SymbolImplicits(symbol: Symbol) {
    def isCaseClass = if (symbol.isClass) symbol.asClass.isCaseClass else false

    def isTrait = asClassOpt.fold(false)(_.isTrait)

    def isSealedTrait = asClassOpt.fold(false)(_.isSealed)

    def isAbstractClass = asClassOpt.fold(false)(_.isAbstract)

    def asClassOpt: Option[ClassSymbol] = scala.util.Try(symbol.asClass).toOption

    def getAnnotationsValues: AnnotationsValues = {
      def findAnnotation[T: TypeTag] = {
        symbol.annotations.find(ann => ann.tree.tpe <:< typeOf[T])
      }

      val description = {
        findAnnotation[Description]
          .flatMap(ann =>
            ann.tree.children
              .tail
              .collect { case Literal(Constant(description: String)) => Description(description) }
              .headOption
          )
      }

      AnnotationsValues(
        description = description
      )
    }
  }

  implicit class TypeImplicits(`type`: Type) {
    def asClass = `type`.typeSymbol.asClass

    def isClass = `type`.typeSymbol.isClass

    def isCaseClass = `type`.typeSymbol.isCaseClass

    def isTrait = `type`.typeSymbol.isTrait

    def isSealedTrait = `type`.typeSymbol.isSealedTrait

    def isAbstractClass = `type`.typeSymbol.isAbstractClass

    def dealiasedTypeArg(index: Int) = `type`.dealias.typeArgs(index)

    def typeName = `type`.typeSymbol.name.toString

    def typeFullName = `type`.typeSymbol.fullName.toString

    def dealiasedFullName = `type`.dealias.typeSymbol.fullName.toString

    def asJavaClass(implicit classLoader: ClassLoader): Option[Class[_]] =
      scala.util.Try(runtimeMirror(classLoader).runtimeClass(`type`)).toOption
  }

  implicit class ClassImplicits(classSymbol: ClassSymbol) {
    def primaryConstructorParams =
      classSymbol
        .primaryConstructor
        .typeSignature
        .paramLists
        .headOption
  }

}
