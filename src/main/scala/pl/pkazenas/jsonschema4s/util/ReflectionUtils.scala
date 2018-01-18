package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._

object ReflectionUtils {

  implicit class SymbolImplicits(symbol: Symbol) {
    def isCaseClass = if (symbol.isClass) symbol.asClass.isCaseClass else false

    def isTrait = asClassOpt.fold(false)(_.isTrait)

    def isSealedTrait = asClassOpt.fold(false)(_.isSealed)

    def isAbstractClass = asClassOpt.fold(false)(_.isAbstract)

    def asClassOpt: Option[ClassSymbol] = scala.util.Try(symbol.asClass).toOption

  }

  implicit class TypeImplicits(`type`: Type) {
    def asClass = `type`.typeSymbol.asClass

    def isClass = `type`.typeSymbol.isClass

    def isCaseClass = `type`.typeSymbol.isCaseClass

    def isTrait = `type`.typeSymbol.isTrait

    def isSealedTrait = `type`.typeSymbol.isSealedTrait

    def isAbstractClass = `type`.typeSymbol.isAbstractClass

    def dealiasedTypeArg(index: Int) = `type`.dealias.typeArgs(index)

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
