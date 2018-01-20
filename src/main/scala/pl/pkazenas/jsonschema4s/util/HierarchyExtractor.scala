package pl.pkazenas.jsonschema4s.util

import scala.reflect.runtime.universe._
import ReflectionUtils._
import ModelUtils._
import pl.pkazenas.jsonschema4s.model._

object HierarchyExtractor {
  def extractSubclasses(`type`: Type, superTypeName: String)(implicit classpathScanner: ClasspathScanner): List[CaseClassType] = {
    classpathScanner.findSubclasses(`type`)
      .filter(_.isCaseClass)
      .map(t => {
        val symbol = t.typeSymbol
        CaseClassType(symbol.name.toString, symbol.asClass.classFields, Option(superTypeName))
      })
  }
}
