package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._

object ReflectionUtils {
  final val intType = typeOf[Int].typeFullName
  final val byteType = typeOf[Byte].typeFullName
  final val booleanType = typeOf[Boolean].typeFullName
  final val charType = typeOf[Char].typeFullName
  final val shortType = typeOf[Short].typeFullName
  final val longType = typeOf[Long].typeFullName
  final val floatType = typeOf[Float].typeFullName
  final val doubleType = typeOf[Double].typeFullName
  final val stringType = typeOf[String].typeFullName

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

    def typeName = `type`.typeSymbol.name.toString

    def typeFullName = `type`.typeSymbol.fullName.toString

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
