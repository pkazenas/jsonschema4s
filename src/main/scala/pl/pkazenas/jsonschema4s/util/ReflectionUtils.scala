package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._

object ReflectionUtils {
  implicit class SymbolImplicits(symbol: Symbol) {
    def isCaseClass = if(symbol.isClass) symbol.asClass.isCaseClass else false

    def isTrait = asClassOpt.fold(false)(_.isTrait)

    def isAbstractClass = asClassOpt.fold(false)(_.isAbstract)

    def asClassOpt: Option[ClassSymbol] = scala.util.Try(symbol.asClass).toOption

    def asJavaClass(implicit classLoader: ClassLoader): Option[Class[_]] =
      scala.util.Try(runtimeMirror(classLoader).runtimeClass(symbol.typeSignature)).toOption
  }

  implicit class TypeImplicits(`type`: Type) {
    def isClass = `type`.typeSymbol.isClass

    def toModel = ???
  }

}
