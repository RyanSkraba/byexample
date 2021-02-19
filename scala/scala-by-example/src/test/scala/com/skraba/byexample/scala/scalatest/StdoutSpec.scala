package com.skraba.byexample.scala.scalatest

import com.skraba.byexample.scala.scalatest.StdoutSpec._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import scala.reflect.io.Streamable

/** Matchers and assertions on stdout, stderr streams.
  *
  * See the helper methods in the StdoutSpec object for the technique to do this.
  */
class StdoutSpec extends AnyFunSpecLike with Matchers {

  describe("Matching standard streams") {

    it("should match stdout and stderr using withConsoleMatch.") {
      withConsoleMatch {
        // Run any arbritrary code here.
        Console.out.println("Hello")
        println("World")
        Console.err.println("WARNING!")
        99
      } { case (x, stdout, stderr) =>
        x shouldBe 99
        stdout shouldBe "Hello\nWorld\n"
        stderr shouldBe "WARNING!\n"
      }
    }

    it("should match stdout and stderr using withGoMatch.") {
      withScalaGoMatch("one", "two", "three") { case (stdout, stderr) =>
        stdout shouldBe "one\nthree\n"
        stderr shouldBe "two\n\n"
      }
    }

    it("should return stdout and stderr using withGo") {
      withScalaGo("one", "two", "three") shouldBe ("one\nthree\n", "two\n\n")
    }
  }
}

object StdoutSpec {

  /** A function to test. */
  def go(args: String*): Unit = {
    for (x <- args.grouped(2)) {
      Console.out.println(x.head)
      Console.err.println(x.tail.mkString)
    }
  }

  /** A helper method used to capture the console and apply it to a partial function.
    * @param thunk code to execute that may use Console.out and Console.err print streams
    * @param pf A partial function to apply matchers
    * @tparam T The return value type of the thunk code to execute
    * @tparam U The return value type of the partial function to return.
    * @return The return value of the partial function.
    */
  def withConsoleMatch[T, U](
      thunk: => T
  )(pf: scala.PartialFunction[(T, String, String), U]): U = {
    Streamable.closing(new ByteArrayOutputStream()) { out =>
      Streamable.closing(new ByteArrayOutputStream()) { err =>
        Console.withOut(out) {
          Console.withErr(err) {
            val t = thunk
            Console.out.flush()
            Console.err.flush()
            // The return value
            pf(
              t,
              new String(out.toByteArray, StandardCharsets.UTF_8),
              new String(err.toByteArray, StandardCharsets.UTF_8)
            )
          }
        }
      }
    }
  }

  /** A helper method used to capture the console of a method execution and apply it to a partial function.
    * @param args String arguments to pass to the go method
    * @param pf A partial function to apply matchers
    * @tparam T The return value type of the thunk code to execute
    * @tparam U The return value type of the partial function to return.
    * @return The return value of the partial function.
    */
  def withScalaGoMatch[T, U](
      args: String*
  )(pf: scala.PartialFunction[(String, String), U]): U = {
    withConsoleMatch(go(args: _*)) { case (_, stdout, stderr) =>
      pf(stdout, stderr)
    }
  }

  /** A helper method used to capture the console of a method execution and return the output.
    * @param args String arguments to pass to the go method
    * @return A tuple of the stdout and stderr
    */
  def withScalaGo(args: String*): (String, String) = {
    withScalaGoMatch(args: _*) { case any => any }
  }
}
