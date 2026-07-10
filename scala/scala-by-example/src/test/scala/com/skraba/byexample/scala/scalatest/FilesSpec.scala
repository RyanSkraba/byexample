package com.skraba.byexample.scala.scalatest

import com.tinfoiled.docopt4s.testkit.TmpDir
import com.tinfoiled.docopt4s.FsPath._
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Path, Paths, StandardCopyOption, StandardOpenOption}
import scala.util.{Properties, Using}

/** Matchers and assertions on files. ScalaTest doesn't help much, but it's pretty easy to use existing methods and
  * tools.
  */
class FilesSpec extends AnyFunSpecLike with Matchers with TmpDir {

  // TODO: should this be added to docopts4s ?

  /** Create a temporary directory that will be shared and not deleted between runs. Be careful, this can have
    * unintended side effects.
    */
  val CachedTmp: Path = (Paths.get(Properties.tmpDir) / getClass.getSimpleName).createDirectory(failIfExists = false)

  /** If this is not None, retain the last [[Tmp]] here. */
  val SaveLastTmp: Option[Path] = Some(CachedTmp / "last")

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try {
      // Optionally save the last temp directory
      SaveLastTmp.map(last => {
        last.deleteRecursively()
        java.nio.file.Files.move(
          Tmp,
          last,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      })
      // Delete the temporary directory
      super.afterAll()
    } catch { case ex: Exception => ex.printStackTrace() }

  /** A resource in this maven project, discoverable on the classpath. */
  val SrcTestResource: Option[Path] = {
    val uri = Thread
      .currentThread()
      .getContextClassLoader
      .getResource(getClass.getPackageName.replace('.', '/') + s"/greeting.txt")
    if (uri.getProtocol == "file") Some(Paths.get(uri.getFile)) else None
  }

  describe("Filesystem operations") {
    val Basic = (Tmp / "basic").createDirectory()
    (Basic / "count").writeAll("1;one\n2;two\n")
    (Basic / "subdir1").createDirectory()
    (Basic / "subdir2").createDirectory()

    it("should list files in a directory") {
      Basic.files.map(_.name) should contain only "count"
      Basic.dirs.map(_.name) should contain allOf ("subdir1", "subdir2")
    }

    it("should create and append to a file") {
      Using.resource(Basic.resolve("newFile").bufferedWriter(StandardOpenOption.CREATE_NEW)) { out =>
        out.write("1;one\n2;two\n")
      }

      // There are some built in tests for existing files and file attributes
      Basic.resolve("noFile").exists shouldBe false

      // The file we created
      val myFile = Basic.resolve("newFile").toFile
      myFile.exists shouldBe true
      myFile.isFile shouldBe true
      myFile.canRead shouldBe true
      myFile.canWrite shouldBe true
      myFile.length shouldBe 12

      myFile should exist
      myFile shouldBe readable
      myFile shouldBe writable

      // Now append using the buffered writer
      Using.resource(myFile.toPath.bufferedWriter(StandardOpenOption.APPEND)) { out =>
        out.write("3;three\n4;four\n")
      }
      myFile.length shouldBe 27

      // Reading using a source
      val contents = Using.resource(scala.io.Source.fromFile(myFile.getAbsoluteFile.toString)) { out =>
        out.getLines().toList
      }
      contents.length shouldBe 4
      contents.head shouldBe "1;one"
    }

    it("should read from a file") {
      val count = Basic / "count"

      // Using a source
      val contents = Using.resource(scala.io.Source.fromFile(count.toFile)) { out =>
        out.getLines().toList
      }
      contents should contain inOrderOnly ("1;one", "2;two")

      // Other reads
      count.slurp() shouldBe "1;one\n2;two\n"
      count.safeSlurp() shouldBe Some("1;one\n2;two\n")
      (Basic / "noFile").safeSlurp() shouldBe None
      count.lines().toList should contain inOrderOnly ("1;one", "2;two")
    }

    it("should find a file in the resources") {
      SrcTestResource.value.slurp().trim shouldBe "Hello world!"
    }
  }
}
