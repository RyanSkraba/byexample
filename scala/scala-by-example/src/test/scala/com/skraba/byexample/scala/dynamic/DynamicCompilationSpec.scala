package com.skraba.byexample.scala.dynamic

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.runtime.universe
import scala.tools.reflect.{ToolBox, ToolBoxError}

/** Running a class dynamically compiled from a string.
  *
  * @see
  *   [[https://stackoverflow.com/questions/39137175/dynamically-compiling-scala-class-files-at-runtime-in-scala-2-11]]
  */
class DynamicCompilationSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  /** The common interface that will be implemented by the dynamic scala code. */
  trait Greeter {
    def greet(name: String): String
  }

  describe("Dynamic Compilation") {

    val tb = universe.runtimeMirror(getClass.getClassLoader).mkToolBox()

    describe("of an expression") {
      it("should return Hello!") {
        val parsed = tb.parse("\"Hello!\"")
        val result = tb.eval(parsed)
        result shouldBe "Hello!"
      }

      it("should do simple math") {
        DynamicHelper.compile("1 + 2 * 3") shouldBe 7
      }

      it("should not have access to variables in scope") {
        val x = 3
        (1 + 2 * x) shouldBe 7
        a[ToolBoxError] should be thrownBy tb.eval(tb.parse("1 + 2 * x"))
      }

      it("should have access to classes and members in scope") {
        a[ToolBoxError] should be thrownBy tb.eval(tb.parse("greeterSnippet"))
        a[ToolBoxError] should be thrownBy tb.eval(tb.parse("DynamicCompilationSpec.greeterSnippet"))
        tb.eval(tb.parse("com.skraba.byexample.scala.dynamic.DynamicCompilationSpec.greeterSnippet")) shouldBe a[String]
      }
    }

    describe("of a snippet") {
      it("should return a new instance of a dynamic class") {
        val parsed = tb.parse(DynamicCompilationSpec.greeterSnippet.format("Hello, ", "!!"))
        val g = tb.eval(parsed).asInstanceOf[Greeter]
        g shouldBe a[Greeter]

        g.greet("dynamic class") shouldBe "Hello, dynamic class!!"

        // and the internal class can be defined again independently in another snippet
        val parsed2 = tb.parse(DynamicCompilationSpec.greeterSnippet.format("Greetings, ", "!!!"))
        val g2 = tb.eval(parsed2).asInstanceOf[Greeter]
        g2 shouldBe a[Greeter]

        g2.greet("other class") shouldBe "Greetings, other class!!!"

        // The two greeters both implement Greeter, but are not the same classes.
        g.getClass shouldNot be(g2.getClass)
        g.getClass.getInterfaces shouldBe g2.getClass.getInterfaces
      }
    }
  }
}

object DynamicCompilationSpec {

  /** The string that we want to compile and reuse in calls. It implements a well-known trait and returns an instance of
    * that trait that can be called.
    */
  val greeterSnippet: String =
    """
      |class CustomizableGreeter(prefix: String, postfix: String) extends com.skraba.byexample.scala.dynamic.DynamicCompilationSpec$Greeter {
      |  override def greet(name: String): String = {
      |    prefix + name + postfix
      |  }
      |}
      |new CustomizableGreeter("%s", "%s")
      |""".stripMargin
}
