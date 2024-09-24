package com.skraba.byexample.scala.scalatest

import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.file.StandardCopyOption
import scala.reflect.io.{Directory, File, Streamable}
import scala.util.Properties

/** Matchers and assertions on files. ScalaTest doesn't help much, but it's pretty easy to use existing methods and
  * tools.
  */
class FilesSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Create a temporary directory that will be shared and not deleted between runs. Be careful, this can have
    * unintended side effects.
    */
  val CachedTmp: Directory = (Directory(Properties.tmpDir) / getClass.getSimpleName).createDirectory()

  /** If this is not None, retain the last [[Tmp]] here. */
  val SaveLastTmp: Option[Directory] = Some(CachedTmp / "last").map(_.toDirectory)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try {
      // Optionally save the last temp directory
      SaveLastTmp.map(last => {
        last.deleteRecursively()
        java.nio.file.Files.move(
          Tmp.jfile.toPath,
          last.jfile.toPath,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      })
      // Delete the temporary directory
      Tmp.deleteRecursively()
    } catch { case ex: Exception => ex.printStackTrace() }

  /** A resource in this maven project, discoverable on the classpath. */
  val SrcTestResource: Option[File] = {
    val uri = Thread
      .currentThread()
      .getContextClassLoader
      .getResource(getClass.getPackageName.replace('.', '/') + s"/greeting.txt")
    if (uri.getProtocol == "file") Some(File(uri.getFile)) else None
  }

  describe("Filesystem operations") {
    val Basic = (Tmp / "basic").createDirectory()
    File(Basic / "count").writeAll("1;one\n2;two\n")
    (Basic / "subdir1").createDirectory()
    (Basic / "subdir2").createDirectory()

    it("should list files in a directory") {
      Basic.files.map(_.name).toSeq should contain only "count"
      Basic.dirs.map(_.name).toSeq should contain allOf ("subdir1", "subdir2")
    }

    it("should create and append to a file") {
      Streamable.closing(
        Basic.resolve("newFile").createFile(true).bufferedWriter()
      ) { out =>
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
      myFile.toFile.length shouldBe 12

      myFile.jfile should exist
      myFile.jfile shouldBe readable
      myFile.jfile shouldBe writable

      // Now append using the buffered writer
      Streamable.closing(
        myFile.bufferedWriter(append = true)
      ) { out =>
        out.write("3;three\n4;four\n")
      }
      myFile.toFile.length shouldBe 27

      // Reading using a source
      val contents = Streamable.closing(
        scala.io.Source.fromFile(myFile.toAbsolute.toString())
      ) { out =>
        out.getLines().toList
      }
      contents.length shouldBe 4
      contents.head shouldBe "1;one"
    }

    it("should read from a file") {
      val count = File(Basic / "count")

      // Using a source
      val contents = Streamable.closing(
        scala.io.Source.fromFile(count.jfile)
      ) { out =>
        out.getLines().toList
      }
      contents should contain inOrderOnly ("1;one", "2;two")

      // Other reads
      count.slurp() shouldBe "1;one\n2;two\n"
      count.safeSlurp() shouldBe Some("1;one\n2;two\n")
      File(Basic / "noFile").safeSlurp() shouldBe None
      count
        .lines()
        .toList should contain inOrderOnly ("1;one", "2;two")
    }

    it("should find a file in the resources") {
      SrcTestResource.value.slurp().trim shouldBe "Hello world!"
    }
  }
}
