package com.skraba.byexample.scala.dynamic

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

/** Helper class to make the toolbox available to Java. */
class DynamicHelper {
  def compile(cl: ClassLoader, code: String): Any = {
    val tb = universe.runtimeMirror(cl).mkToolBox()
    tb.eval(tb.parse(code))
  }
}
