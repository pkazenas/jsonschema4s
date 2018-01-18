package pl.pkazenas

import pl.pkazenas.jsonschema4s.core.ModelExtractor
import pl.pkazenas.jsonschema4s.model._
import pl.pkazenas.jsonschema4s.util.ClasspathScanner

import scala.reflect.runtime.universe._

package object jsonschema4s {
  object Api {
    case class ApiConfig(classLoader: ClassLoader = ClassLoader.getSystemClassLoader,
                         packagesToScan: List[String] = List())

    implicit def toModel(`type`: Type)(implicit apiConfig: ApiConfig = ApiConfig()): RootType = {

      implicit val classPathScanner: ClasspathScanner = ClasspathScanner(apiConfig.classLoader, apiConfig.packagesToScan)
      ModelExtractor.extract(`type`)
    }

    //implicit def toModel[T](implicit tag: TypeTag[T]): RootType = toModel(tag.tpe)
  }

}
