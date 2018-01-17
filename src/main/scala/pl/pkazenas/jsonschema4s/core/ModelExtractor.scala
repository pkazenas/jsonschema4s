package pl.pkazenas.jsonschema4s.core

import pl.pkazenas.jsonschema4s.model._
import scala.reflect.runtime.universe._


object ModelExtractor {
  import pl.pkazenas.jsonschema4s.util.ModelUtils._
  import pl.pkazenas.jsonschema4s.util.ReflectionUtils._

  private def extractFields(`type`: Type): List[ClassField] =
    `type`
      .typeSymbol
      .asClass
      .primaryConstructorParams
      .map(_.map(s => s.toClassField))
      .getOrElse(List())

  private def extractCaseClass(`type`: Type): RootType = {
    val name = `type`.typeSymbol.name.toString
    val members = extractFields(`type`)

    RootType(
      name = name,
      fields = members)
  }

  def extract(`type`: Type)(implicit classLoader: ClassLoader = ClassLoader.getSystemClassLoader): RootType =
    if(`type`.isCaseClass) extractCaseClass(`type`)
    else throw new ModelExtractionException("Provided type is not a case class.")
}
