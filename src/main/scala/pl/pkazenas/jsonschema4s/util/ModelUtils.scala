package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.model._
import ReflectionUtils._

object ModelUtils {

  implicit class SymbolModelImplicits(symbol: Symbol) {
    def toClassField: ClassField = {
      val fieldName = symbol.name.toString

      val (modelType, required) =
      symbol.typeSignature match {
        // primitive types
        case t if t =:= typeOf[Byte] => (ByteType, true)
        case t if t =:= typeOf[Char] => (CharType, true)
        case t if t =:= typeOf[Short] => (ShortType, true)
        case t if t =:= typeOf[Int] => (IntType, true)
        case t if t =:= typeOf[Long] => (LongType, true)
        case t if t =:= typeOf[Float] => (FloatType, true)
        case t if t =:= typeOf[Double] => (DoubleType, true)
        // other builtin types
        case t if t =:= typeOf[String] => (StringType, true)
        case t if t <:< typeOf[Option[_]] => ??? // TODO: implement this
        //collection types, // TODO: implement this
        // complex types
        case t if t.isCaseClass => ??? // TODO:
        case t if t.isTrait => ??? // TODO: handle hierarchy
        case t if t.isAbstractClass => ??? // TODO: handle hierarchy
        case t if t.isSealedTrait => ??? // TODO: handle this case
      }

      ClassField(fieldName, modelType, required)
    }
  }

}
