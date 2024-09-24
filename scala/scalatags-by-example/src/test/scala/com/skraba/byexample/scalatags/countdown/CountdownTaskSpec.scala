package com.skraba.byexample.scalatags.countdown

import com.skraba.byexample.scalatags.ScalatagsGoSpec.withScalatagsGoMatch
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File}

/** Unit tests for the [[CountdownTask]] CLI.
  */
class CountdownTaskSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** A resource in this maven project, discoverable on the classpath. */
  val TemplateFile: File = {
    val uri = Thread
      .currentThread()
      .getContextClassLoader
      .getResource("timer.svg")
    if (uri.getProtocol == "file") Some(File(uri.getFile)) else None
  }.getOrElse(fail("Can't find template resource 'timer.svg'"))

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  describe("CountdownTaskSpec simple test") {
    it("should run") {
      val tmpDir = (Tmp / "simple").createDirectory()

      withScalatagsGoMatch(
        CountdownTask.Cmd,
        "--dstDir",
        tmpDir.toString,
        "--warmup",
        "1",
        "--duration",
        "1",
        "--cooldown",
        "1",
        "--frameRate",
        "1",
        TemplateFile.toString
      ) { case (stdout, stderr) =>
        stderr shouldBe ""
        tmpDir.files.toSeq should have size 4
        for (f <- tmpDir.files) {
          f.name should fullyMatch regex raw"timer.0000[0123].00000.svg"
        }
      }
    }
  }

  describe("CountdownTaskSpec full test") {
    ignore("should run") {
      val tmpDir = (Tmp / "full").createDirectory()
      withScalatagsGoMatch(
        CountdownTask.Cmd,
        "--dstDir",
        tmpDir.toString,
        "--dstVideo",
        (tmpDir / "timer.mp4").toString,
        TemplateFile.toString
      ) { case (stdout, stderr) =>
        stderr shouldBe ""
        // Length = WarmUp + Duration + Cooldown (2 + 300 + 30 by default)
        // Frames = FrameRate * Length
        // Total = (Frames + 1) * 2 [two files per frame] + 1 video file
        tmpDir.files.toSeq should have size (30 * (2 + 300 + 30) * 2 + 2 + 1)
        (tmpDir / "timer.mp4")
      }
    }
  }
}
