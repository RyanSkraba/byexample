package com.skraba.byexample.scala.scalatest

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File, Path, Streamable}

/** Matchers and assertions on files. ScalaTest doesn't help much, but it's pretty easy to use existing methods and
  * tools.
  */
class FilesSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** Create a temporary directory to use for all tests. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** And delete it after the tests. */
  override protected def afterAll(): Unit =
    try {
      Tmp.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
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
  }
}
