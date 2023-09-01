package com.skraba.byexample.scala.markd

import com.skraba.byexample.scala.markd.MarkdGo.InternalDocoptException
import com.skraba.byexample.scala.markd.MarkdGoSpec.withMarkdGo
import org.docopt.DocoptExitException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

/** Unit tests for [[MarkdGo]] */
class MarkdGoSpec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach
    with BeforeAndAfterAll {

  describe("MarkdGo docopt check") {
    it("should have less than 80 characters per string for readability") {
      for (line <- MarkdGo.Doc.split("\n")) {
        withClue("main" -> line) {
          line.length should be < 80
        }
      }
    }
  }

  describe("MarkdGo invalid command lines") {
    it("throws an exception with --version") {
      val t = intercept[DocoptExitException] {
        withMarkdGo("--version")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe MarkdGo.Version
    }

    it("throws an exception with --help") {
      val t = intercept[DocoptExitException] {
        withMarkdGo("--help")
      }
      t.getExitCode shouldBe 0
      t.getMessage shouldBe MarkdGo.Doc
    }

    it(s"throws an exception with unknown options") {
      for (
        args <- Seq(
          Seq("--garbage"),
          Seq("--debug", "--garbage"),
          Seq("--garbage", "--debug"),
          Seq("--garbage", "garbage")
        )
      ) withClue(s"Using: $args") {
        val t = intercept[DocoptExitException] {
          withMarkdGo(args: _*)
        }
        t.getExitCode shouldBe 1
        t.getMessage shouldBe null
      }
    }

    it("throws an exception with an unknown command") {
      val t = intercept[InternalDocoptException] {
        withMarkdGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe MarkdGo.Doc
    }
  }

  describe("MarkdGo beautify") {
    it("throws an exception if no files are specified") {
      val t = intercept[InternalDocoptException] {
        withMarkdGo("beautify")
      }
      t.getMessage shouldBe null
      t.docopt shouldBe MarkdGo.BeautifyTask.Doc
    }
  }

  describe("MarkdGo datecount") {
    it("throws an exception if no files are specified") {
      val t = intercept[InternalDocoptException] {
        withMarkdGo("datecount")
      }
      t.getMessage shouldBe null
      t.docopt shouldBe MarkdGo.DateCountdownTask.Doc
    }
  }

  for (task <- MarkdGo.Tasks) {
    describe(s"MarkdGo ${task.cmd} docopt check") {
      it("should have less than 80 characters per string for readability") {
        for (line <- task.doc.split("\n")) {
          withClue(task.cmd -> line) {
            line.length should be < 80
          }
        }
      }
    }

    describe(s"MarkdGo ${task.cmd} invalid command lines") {
      it("throws an exception if no files are specified") {
        val t = intercept[InternalDocoptException] {
          withMarkdGo(task.cmd)
        }
        t.getMessage shouldBe null
        t.docopt shouldBe task.doc
      }

      it("throws an exception with --version") {
        val t = intercept[DocoptExitException] {
          withMarkdGo(task.cmd, "--version")
        }
        t.getExitCode shouldBe 0
        t.getMessage shouldBe MarkdGo.Version
      }

      it("throws an exception with --help") {
        val t = intercept[DocoptExitException] {
          withMarkdGo(task.cmd, "--help")
        }
        t.getExitCode shouldBe 0
        t.getMessage shouldBe task.doc
      }

      it(s"throws an exception with unknown options") {
        for (
          args <- Seq(
            Seq(task.cmd, "--garbage"),
            Seq(task.cmd, "--debug", "--garbage"),
            Seq(task.cmd, "--garbage", "--debug"),
            Seq(task.cmd, "--garbage", "garbage")
          )
        ) withClue(s"Using: $args") {
          val t = intercept[InternalDocoptException] {
            withMarkdGo(args: _*)
          }
          t.docopt shouldBe task.doc
          t.getMessage shouldBe null
        }
      }
    }
  }
}

object MarkdGoSpec {

  import com.skraba.byexample.scala.scalatest.StdoutSpec.withConsoleMatch

  /** A helper method used to capture the console of a ScalaGo execution and
    * apply it to a partial function.
    * @param args
    *   String arguments to pass to the ScalaGo.go method
    * @param pf
    *   A partial function to apply matchers
    * @tparam T
    *   The return value type of the thunk code to execute
    * @tparam U
    *   The return value type of the partial function to return.
    * @return
    *   The return value of the partial function.
    */
  def withMarkdGoMatch[T, U](
      args: String*
  )(pf: scala.PartialFunction[(String, String), U]): U = {
    withConsoleMatch(MarkdGo.go(args: _*)) { case (_, stdout, stderr) =>
      pf(stdout, stderr)
    }
  }

  /** A helper method used to capture the console of a ScalaGo execution and
    * return the output.
    * @param args
    *   String arguments to pass to the ScalaGo.go method
    * @return
    *   A tuple of the stdout and stderr
    */
  def withMarkdGo(args: String*): (String, String) = {
    withMarkdGoMatch(args: _*) { case any => any }
  }
}
