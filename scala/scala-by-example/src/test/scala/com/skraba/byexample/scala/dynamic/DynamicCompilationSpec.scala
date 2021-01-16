package com.skraba.byexample.scala.dynamic

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.runtime.universe
import scala.tools.reflect.ToolBox

/** Running a class dynamically compiled from a string.
  *
  * @see https://stackoverflow.com/questions/39137175/dynamically-compiling-scala-class-files-at-runtime-in-scala-2-11
  */
class DynamicCompilationSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  /** The common interface that will be implemented by the scala code. */
  trait Greeter {
    def greet(name: String): String
  }

  describe("Dynamic Compilation") {

    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

    describe("Of an expression") {
      it("should return Hello!.") {
        val parsed = tb.parse("""{"Hello!"}""")
        val result = tb.eval(parsed)
        result shouldBe "Hello!"
      }
    }

    describe("of a class file") {

      it("should return a callable class.") {
        val parsed = tb.parse(
          DynamicCompilationSpec.myGreeterClass.format("Hello, ", "!!")
        )
        val g = tb.eval(parsed).asInstanceOf[Greeter]
        g shouldBe a[Greeter]

        g.greet("dynamic class") shouldBe "Hello, dynamic class!!"
      }
    }

  }

}

object DynamicCompilationSpec {

  /** The string that we want to compile and reuse in calls.  It implements a well-known trait and returns an instance of that trait that can be called. */
  val myGreeterClass =
    """
      |class CustomizableGreeter(prefix: String, postfix: String) extends com.skraba.byexample.scala.dynamic.DynamicCompilationSpec$Greeter {
      |  override def greet(name: String): String = {
      |    prefix + name + postfix
      |  }
      |}
      |scala.reflect.classTag[CustomizableGreeter].runtimeClass
      |new CustomizableGreeter("%s", "%s")
      |""".stripMargin

  /** Helper class to make the toolbox available to Java. */
  def compile(cl: ClassLoader, code: String): Any = {
    val tb = universe.runtimeMirror(cl).mkToolBox()
    tb.eval(tb.parse(code))
  }

}
