package pl.pkazenas

import pl.pkazenas.jsonschema4s.core.ModelExtractor
import pl.pkazenas.jsonschema4s.core.json.{JsonSchemaGenerator, JsonSchemaPrinter}
import pl.pkazenas.jsonschema4s.model._
import pl.pkazenas.jsonschema4s.util.ClasspathScanner

import scala.reflect.runtime.universe._

package object jsonschema4s {
  object Api {
    case class ApiConfig(classLoader: ClassLoader = ClassLoader.getSystemClassLoader,
                         packagesToScan: List[String] = List())

    object Implicits {
      implicit val defaultApiConfig: ApiConfig = ApiConfig()
    }

    def toModel(`type`: Type)(implicit apiConfig: ApiConfig): RootType = {
      implicit val classPathScanner: ClasspathScanner = ClasspathScanner(apiConfig.classLoader, apiConfig.packagesToScan)
      ModelExtractor.extract(`type`)
    }

    def toModel[T](implicit tag: TypeTag[T], apiConfig: ApiConfig): RootType =
      toModel(tag.tpe)(apiConfig)

    def toJsonSchema(`type`: Type)(implicit apiConfig: ApiConfig): String =
      modelAsJsonSchema(toModel(`type`))

    def toJsonSchema[T](implicit tag: TypeTag[T], apiConfig: ApiConfig): String =
      modelAsJsonSchema(toModel(tag.tpe))

    def modelAsJsonSchema(model: RootType): String = JsonSchemaPrinter(JsonSchemaGenerator.generate(model))
  }
}
