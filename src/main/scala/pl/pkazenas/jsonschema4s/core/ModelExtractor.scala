package pl.pkazenas.jsonschema4s.core
import pl.pkazenas.jsonschema4s.model._
import scala.reflect.runtime.universe._
object ModelExtractor {
  def extract(`type`: Type)(implicit classLoader: ClassLoader): RootType = {
???
  }

  def symbolToModel(symbol: Symbol): Unit = {
    symbol match {
      case s if s.isMethod => ???
    }
  }
}
