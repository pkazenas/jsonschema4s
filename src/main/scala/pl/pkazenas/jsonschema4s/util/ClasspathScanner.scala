package pl.pkazenas.jsonschema4s.util

import org.reflections.Reflections

import scala.reflect.runtime.universe._
import ReflectionUtils._
import org.reflections.util.ConfigurationBuilder
import pl.pkazenas.jsonschema4s.annotation.JsonDataContract

import scala.collection.JavaConverters.iterableAsScalaIterableConverter

class ClasspathScanner(classLoader: ClassLoader, packages: List[String] = List()) {
  lazy val reflections =
    new Reflections(ConfigurationBuilder.build(classLoader).forPackages(packages: _*))


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

  def findAnnotatedTypes(annotation: Class[_ <: java.lang.annotation.Annotation]): List[Type] =
    reflections
      .getTypesAnnotatedWith(annotation)
      .asScala
      .map(clazz => mirror.staticClass(clazz.getName).toType)
      .toList
}

object ClasspathScanner {
  def apply(classLoader: ClassLoader, packages: List[String] = List()): ClasspathScanner = new ClasspathScanner(classLoader, packages)

  lazy val default = ClasspathScanner(ClassLoader.getSystemClassLoader)
}
