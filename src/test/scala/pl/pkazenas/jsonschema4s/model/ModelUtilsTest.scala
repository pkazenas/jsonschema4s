package pl.pkazenas.jsonschema4s.model

import org.scalatest.{FunSuite, OneInstancePerTest}
import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._
import pl.pkazenas.jsonschema4s.util.ModelUtils._

class ModelUtilsTest extends FunSuite with OneInstancePerTest {
  test("Int type parsing") {
    assertResult(IntType)(typeOf[Int].toTypeDefinition)
  }

  test("Long type parsing") {
    assertResult(LongType)(typeOf[Long].toTypeDefinition)
  }

  test("Short type parsing") {
    assertResult(ShortType)(typeOf[Short].toTypeDefinition)
  }

  test("Byte type parsing") {
    assertResult(ByteType)(typeOf[Byte].toTypeDefinition)
  }

  test("Char type parsing") {
    assertResult(CharType)(typeOf[Char].toTypeDefinition)
  }

  test("Float type parsing") {
    assertResult(FloatType)(typeOf[Float].toTypeDefinition)
  }

  test("Double type parsing") {
    assertResult(DoubleType)(typeOf[Double].toTypeDefinition)
  }

  test("String type parsing") {
    assertResult(StringType)(typeOf[String].toTypeDefinition)
  }

  test("Option type parsing") {
    assertResult(OptionalType(IntType))(typeOf[Option[Int]].toTypeDefinition)
  }
}
