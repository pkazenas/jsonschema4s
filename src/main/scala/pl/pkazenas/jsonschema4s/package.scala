package pl.pkazenas

import pl.pkazenas.jsonschema4s.core.ModelExtractor
import pl.pkazenas.jsonschema4s.model._

import scala.reflect.runtime.universe._

package object jsonschema4s {
  object Lib {
    implicit def toModel(`type`: Type)(
      implicit classLoader: ClassLoader = ClassLoader.getSystemClassLoader): RootType = ModelExtractor.extract(`type`)

    //implicit def toModel[T](implicit tag: TypeTag[T]): RootType = toModel(tag.tpe)
  }

}
