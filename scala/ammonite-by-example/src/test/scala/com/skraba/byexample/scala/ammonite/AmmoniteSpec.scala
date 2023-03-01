package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteSpec.withConsoleMatch
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.reflect.io.{Directory, Path, Streamable}

/** Testing ammonite scripts can be a bit tricky!
  */
class AmmoniteSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** The path containing ammonite scripts. */
  val ScriptPath: Path = Path(
    Paths.get(getClass.getResource("/ammonite_example.sc").toURI).toFile
  ).parent

  /** Create a temporary directory to use for all tests. */
  val TempFolder: Path = Directory.makeTemp(getClass.getSimpleName)

  /** And delete it after the tests. */
  override protected def afterAll(): Unit =
    try {
      TempFolder.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  describe("Running the ammonite example script in memory") {

    it("should print help") {
      Streamable.closing(new ByteArrayInputStream(Array.empty[Byte])) { in =>
        Console.withIn(in) {
          withConsoleMatch(
            ammonite.AmmoniteMain.main0(
              List(
                "--silent",
                "--home",
                TempFolder.toString,
                (ScriptPath / "ammonite_example.sc").toString,
                "help"
              ),
              in,
              Console.out,
              Console.err
            )
          ) { case (result, stdout, stderr) =>
            result shouldBe true
            stderr shouldBe empty
            stdout should include regex ("ammonite_example.sc.*- Demonstrate how to script with Ammonite")
          }
        }
      }
    }
  }
}

object AmmoniteSpec {

  /** A helper method used to capture the console and apply it to a partial
    * function.
    *
    * @param thunk
    *   code to execute that may use Console.out and Console.err print streams
    * @param pf
    *   A partial function to apply matchers
    * @tparam T
    *   The return value type of the thunk code to execute
    * @tparam U
    *   The return value type of the partial function to return.
    * @return
    *   The return value of the partial function.
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

}
