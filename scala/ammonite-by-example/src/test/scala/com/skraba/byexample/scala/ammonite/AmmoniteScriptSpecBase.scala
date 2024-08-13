package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.nio.charset.StandardCharsets
import java.nio.file.Paths
import scala.reflect.io.{Directory, File, Path, Streamable}
import scala.util.Properties

/** Testing ammonite scripts can be a bit tricky!
  *
  * This class provides a useful base for setting up a scripting environment.
  */
abstract class AmmoniteScriptSpecBase extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** The path containing ammonite scripts. */
  val ScriptPath: File

  /** The filename of the script being run. */
  lazy val ScriptName: String = ScriptPath.name

  /** A temporary directory for playing with files. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Either create a new home directory reused across this suite, or use the common one.
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

  /** Creates a scenario in the temporary directory with some items, by default:
    *
    *   - `/tmp/tag/src/DCIM/Camera` for a camera directory
    *     - `image1.jpg` (fake images, containing their name as text)
    *     - `image2.jpg`
    *     - `image3.jpg`
    *   - `/tmp/tag/dst/` An empty directory to use as output.
    *
    * @param tag
    *   A string tag to use to uniquely identify the scenario
    * @param srcSubDir
    *   The subdirectory to create in the src directory
    * @param srcSubDirFiles
    *   The files to create in the src subdirectory
    * @return
    *   the source and destination directories.
    */
  def createSrcDst(
      tag: String,
      srcSubDir: String = "DCIM/Camera",
      srcSubDirFiles: Seq[String] = Seq("image1.jpg", "image2.jpg", "image3.jpg")
  ): (Directory, Directory) = {
    val src = (Tmp / tag / "src").createDirectory(failIfExists = false)
    val dst = (Tmp / tag / "dst").createDirectory(failIfExists = false)
    val fileDir = if (srcSubDir.isEmpty) src else (src / srcSubDir).createDirectory(failIfExists = false)
    srcSubDirFiles.foreach(f => (fileDir / f).createFile().writeAll(f))
    (src, dst)
  }

  /** A standalone helper method for running one specific script.
    *
    * @param args
    *   The arguments to apply to the ammonite script
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from the ammonite call
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

  /** A standalone helper method for running one specific script, with two groups of arguments.
    *
    * @param args1
    *   First set of arguments to apply to the ammonite script
    * @param args2
    *   Second set of arguments to apply to the ammonite script
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from the ammonite call
    * @tparam U
    *   If any, the type of output of the partial function
    * @return
    *   The output of the partial function
    */
  def withScript2[U](args1: String*)(args2: String*)(
      pf: scala.PartialFunction[(Boolean, String, String), U]
  ): U = withScript(args1 ++ args2: _*)(pf)

  /** A helper method for running an ammonite script with an assumed successful result.
    * @param replacements
    *   A list of pairs of strings to replace in the output.
    * @param task
    *   The task to run
    * @param args
    *   The arguments to apply to the ammonite script
    * @return
    *   The output of the script with all of the string replacements applied, as well as replacing the temporary
    *   directory with &lt;TMP&gt;.
    */
  def withTaskSuccess(replacements: (String, String)*)(task: String)(args: String*): String = {
    withScript2(task)(args: _*) { case (result, stdout, stderr) =>
      stderr shouldBe empty
      result shouldBe true
      replacements.foldLeft(stdout.replace(Tmp.toString, "<TMP>")) { (acc, r) =>
        acc.replace(r._1, r._2)
      }
    }
  }
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

  /** A helper method used to capture the console and apply it to a partial function.
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
    *   The arguments to apply to the ammonite executable, starting with the script name.
    * @param pf
    *   A partial function taking the result, the stdout and stderr strings from the ammonite call
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
