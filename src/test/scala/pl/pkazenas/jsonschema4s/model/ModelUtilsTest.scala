package pl.pkazenas.jsonschema4s.model

import com.sun.org.apache.bcel.internal.util.ClassPath.ClassFile
import org.scalatest.{FunSuite, OneInstancePerTest}

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._
import pl.pkazenas.jsonschema4s.util.ModelUtils
import pl.pkazenas.jsonschema4s.util.ModelUtils._

class ModelUtilsTest extends FunSuite with OneInstancePerTest {
  test("Boolean type parsing") {
    assertResult(BooleanType)(typeOf[Boolean].toTypeDefinition)
  }

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

  val expectedAType = CaseClassType("A", List(ClassField("a", StringType), ClassField("b", IntType)))
  val expectedBType = CaseClassType("B", List(ClassField("c", LongType)))
  val expectedCType = CaseClassType("C", List(ClassField("a", expectedAType), ClassField("b", expectedBType)))

  test("testClass \"A\" parsing") {
    assertResult(expectedAType)(typeOf[A].toTypeDefinition)
  }

  test("testClass \"NestedClass\" parsing") {
    val expected =
      CaseClassType(
        "NestedClass",
        List(
          ClassField("first", expectedAType),
          ClassField("second", expectedBType),
          ClassField("third", expectedCType)))

    val actual = typeOf[NestedClass].toTypeDefinition
    assertResult(expected)(actual)
  }

  test("sealed trait hierarchy parsing") {
    val expected =
      TraitType(
        "Animal",
        List(
          CaseClassType("Cat", List(ClassField("owner", StringType)), Some("Animal")),
          CaseClassType("Dog", List(ClassField("name", StringType)), Some("Animal")))
      )

    assertResult(expected)(typeOf[Animal].toTypeDefinition)
  }

  test("List type parsing") {
    assertResult(ArrayType(StringType))(typeOf[List[String]].toTypeDefinition)
  }

  test("Array type parsing") {
    assertResult(ArrayType(StringType))(typeOf[Array[String]].toTypeDefinition)
  }

  test("Set type parsing") {
    assertResult(ArrayType(StringType))(typeOf[Set[String]].toTypeDefinition)
  }

  test("Map type parsing") {
    assertResult(MapType(StringType, LongType))(typeOf[Map[String, Long]].toTypeDefinition)
  }

  test("findAllCaseClassTypes") {
    val rootType =
      RootType(
        "test",
        List(
          ClassField("a", CaseClassType("Test1", List(ClassField("b", CaseClassType("Test2", List(ClassField("c", IntType))))))),
          ClassField("b", TraitType("TestTrait", List(CaseClassType("Test3", List(ClassField("d", StringType)))))),
          ClassField("optional", OptionalType(CaseClassType("wrappedClass", List(ClassField("c", StringType))))),
          ClassField("array", ArrayType(TraitType("wrappedTrait", List(CaseClassType("wrappedImpl", List(ClassField("g", LongType)))))))
        )
      )

    val expected =
      List(
        CaseClassType("Test1", List(ClassField("b", CaseClassType("Test2", List(ClassField("c", IntType)))))),
        TraitType("TestTrait", List(CaseClassType("Test3", List(ClassField("d", StringType))))),
        CaseClassType("Test3", List(ClassField("d", StringType))),
        CaseClassType("Test2", List(ClassField("c", IntType))),
        CaseClassType("wrappedClass", List(ClassField("c", StringType))),
        TraitType("wrappedTrait", List(CaseClassType("wrappedImpl", List(ClassField("g", LongType))))),
        CaseClassType("wrappedImpl", List(ClassField("g", LongType)))
      )

    val actual = ModelUtils.findAllComplexTypes(rootType)

    assertResult(expected.toSet)(actual.toSet)
  }
}
