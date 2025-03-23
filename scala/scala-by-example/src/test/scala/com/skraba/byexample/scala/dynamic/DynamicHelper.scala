package com.skraba.byexample.scala.dynamic

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

/** Helper class to make the toolbox available to Java. */
object DynamicHelper {
  def compile(code: String): Any = compile(code, getClass.getClassLoader)

  def compile(code: String, cl: ClassLoader): Any = {
    val tb = universe.runtimeMirror(cl).mkToolBox()
    tb.eval(tb.parse(code))
  }
}
