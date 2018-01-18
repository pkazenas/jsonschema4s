package pl.pkazenas.jsonschema4s.util

import org.reflections.Reflections

import scala.reflect.runtime.universe._
import ReflectionUtils._
import org.reflections.util.ConfigurationBuilder

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

class ClasspathScanner(classLoader: ClassLoader, packages: List[String] = List()) {
  lazy val reflections = {
    val configuration =
      new ConfigurationBuilder()
        .addClassLoader(classLoader)
        .forPackages(packages: _*)

    new Reflections(configuration)
  }
  lazy val mirror = runtimeMirror(classLoader)

  def findSubclasses(`type`: Type): List[Type] =
    `type`
      .asJavaClass(classLoader)
      .map(javaClass => {
        reflections
          .getSubTypesOf(javaClass)
          .asScala
          .map(clazz => mirror.staticClass(clazz.getName).toType)
          .toList
      })
      .getOrElse(List())
}

object ClasspathScanner {
  def apply(classLoader: ClassLoader, packages: List[String] = List()): ClasspathScanner = new ClasspathScanner(classLoader, packages)
}
