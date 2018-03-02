package pl.pkazenas.jsonschema4s.core

import org.scalatest.{FunSuite, OneInstancePerTest}
import pl.pkazenas.jsonschema4s.annotation.JsonDataContract
import pl.pkazenas.jsonschema4s.test._
import pl.pkazenas.jsonschema4s.util.ClasspathScanner

import scala.reflect.runtime.universe._
import pl.pkazenas.jsonschema4s.test.testClasses._

class ClasspathScannerTest extends FunSuite with OneInstancePerTest {
  val scanner = ClasspathScanner(ClassLoader.getSystemClassLoader, List("pl.pkazenas"))

  test("Scan for scala trait subclasses") {
    val expected = Set(typeOf[Square], typeOf[Circle])
    val actual = scanner.findSubclasses(typeOf[Shape]).toSet

    assertResult(expected)(actual)
  }

  test("Scan for abstract class subclasses") {
    val expected = Set(typeOf[Cactus], typeOf[Pine])
    val actual = scanner.findSubclasses(typeOf[Plant]).toSet

    assertResult(expected)(actual)
  }

  test("Scan for annotated classes") {
    val expected = Set(typeOf[AnnotatedClass2], typeOf[AnnotatedClass1])
    assertResult(expected)(scanner.findAnnotatedTypes(classOf[JsonDataContract]).toSet)
  }
}
