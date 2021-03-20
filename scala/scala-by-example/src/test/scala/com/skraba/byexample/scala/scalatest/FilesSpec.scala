package com.skraba.byexample.scala.scalatest

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, Path, Streamable}

/** Matchers and assertions on files.  ScalaTest doesn't help much, but it's pretty easy to
  * use existing methods and tools.
  */
class FilesSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

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

  describe("File operations") {
    it("should create and append, and read from a file") {
      Streamable.closing(
        TempFolder.resolve("newFile").createFile(true).bufferedWriter()
      ) { out =>
        out.write("1;one\n2;two\n")
      }

      // There are some built in tests for existing files and file attributes
      TempFolder.resolve("noFile").exists shouldBe false

      val myFile = TempFolder.resolve("newFile")
      myFile.exists shouldBe true
      myFile.isFile shouldBe true
      myFile.canRead shouldBe true
      myFile.canWrite shouldBe true
      myFile.toFile.length shouldBe 12

      // Now append.
      Streamable.closing(
        TempFolder.resolve("newFile").toFile.bufferedWriter(append = true)
      ) { out =>
        out.write("3;three\n4;four\n")
      }
      myFile.toFile.length shouldBe 27

      // Reading
      val contents = Streamable.closing(
        scala.io.Source.fromFile(myFile.toAbsolute.toString())
      ) { out =>
        out.getLines().toList
      }
      contents.length shouldBe 4
      contents.head shouldBe "1;one"
    }
  }
}
