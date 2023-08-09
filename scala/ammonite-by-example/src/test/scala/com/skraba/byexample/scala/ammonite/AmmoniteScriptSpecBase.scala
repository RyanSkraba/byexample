package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.reflect.io._
import scala.util.Properties

/** Testing ammonite scripts can be a bit tricky!
  *
  * This class provides a useful base for setting up a scripting environment.
  */
abstract class AmmoniteScriptSpecBase
    extends AnyFunSpecLike
    with BeforeAndAfterAll
    with Matchers {

  /** The path containing ammonite scripts. */
  val ScriptPath: File

  /** A temporary directory for playing with files. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Either create a new home directory reused across this suite, or use the
    * common one.
    */
  val HomeFolder: Path =
    if (ReuseAmmoniteHome) ReusableAmmoniteHome
    else Tmp / "ammonite.home"

  /** And delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try {
      Tmp.deleteRecursively()
      if (!ReuseAmmoniteHome && HomeFolder.exists)
        HomeFolder.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  /** A standalone helper method for running one specific script.
    *
    * @param args
    *   The arguments to apply to the ammonite script
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from
    *   the ammonite call
    * @tparam U
    *   If any, the type of output of the partial function
    * @return
    *   The output of the partial function
    */
  def withScript[U](args: String*)(
      pf: scala.PartialFunction[(Boolean, String, String), U]
  ): U = Streamable.closing(new ByteArrayInputStream(Array.empty[Byte])) { in =>
    Console.withIn(in) {
      withConsoleMatch(
        ammonite.AmmoniteMain.main0(
          List(
            "--silent",
            "--home",
            HomeFolder.toString,
            ScriptPath.toString
          ) ++ args,
          in,
          Console.out,
          Console.err
        )
      )(pf)
    }
  }

  /** A standalone helper method for running one specific script, with two
    * groups of arguments.
    *
    * @param args1
    *   First set of arguments to apply to the ammonite script
    * @param args2
    *   Second set of arguments to apply to the ammonite script
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from
    *   the ammonite call
    * @tparam U
    *   If any, the type of output of the partial function
    * @return
    *   The output of the partial function
    */
  def withScript2[U](args1: String*)(args2: String*)(
      pf: scala.PartialFunction[(Boolean, String, String), U]
  ): U = withScript(args1 ++ args2: _*)(pf)
}

object AmmoniteScriptSpecBase {

  /** Set to true to attempt to reuse the ammonite cache across tests */
  val ReuseAmmoniteHome: Boolean =
    sys.env.getOrElse("AMMONITESPEC_REUSE", "").nonEmpty

  lazy val ReusableAmmoniteHome: Directory =
    (Directory(Properties.tmpDir) / getClass.getSimpleName).createDirectory()

  /** Find the given script as a file in the classpath.
    *
    * @param script
    *   The name of the script to run.
    * @return
    *   The file pointing to that script in the current test environment.
    */
  def find(script: String): File = File(
    Paths.get(getClass.getResource(script).toURI).toFile
  )

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

  /** A helper method for running an ammonite script.
    *
    * @param args
    *   The arguments to apply to the ammonite executable, starting with the
    *   script name.
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from
    *   the ammonite call
    * @tparam U
    *   If any, the type of output of the partial function
    * @return
    *   The output of the partial function
    */
  def withAmmoniteMain0AndNoStdIn[U](tmp: Path, args: String*)(
      pf: scala.PartialFunction[(Boolean, String, String), U]
  ): U = Streamable.closing(new ByteArrayInputStream(Array.empty[Byte])) { in =>
    Console.withIn(in) {
      withConsoleMatch(
        ammonite.AmmoniteMain.main0(
          List("--silent", "--home", tmp.toString) ++ args,
          in,
          Console.out,
          Console.err
        )
      )(pf)
    }
  }
}
