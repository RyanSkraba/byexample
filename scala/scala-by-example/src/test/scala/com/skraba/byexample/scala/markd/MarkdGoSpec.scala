package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGoSpec
import org.docopt.DocoptExitException

/** Unit tests for [[MarkdGo]] */
class MarkdGoSpec extends DocoptCliGoSpec(MarkdGo) {

  describe(s"${Cli.Cli} docopt check") {
    it("should have less than 80 characters per string for readability.") {
      for (line <- Doc.split("\n")) {
        withClue("main" -> line) {
          line.length should be < 80
        }
      }
      for (
        task <- Cli.Tasks;
        line <- task.Doc.split("\n")
      ) {
        withClue(task.Cmd -> line) {
          line.length should be < 80
        }
      }
    }
  }

  describe(s"${Cli.Cli} command line") {

    itShouldThrowOnHelpAndVersionFlags()

    it("throw an exception like --help when run without a command") {
      val t = interceptGoDocoptEx("--debug")
      t.getMessage shouldBe "Missing command"
      t.docopt shouldBe Cli.Doc
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
          withGo(args: _*)
        }
        t.getExitCode shouldBe 1
        t.getMessage shouldBe null
      }
    }

    it("throws an exception with an unknown command") {
      val t = intercept[MarkdGo.InternalDocoptException] {
        withGo("garbage")
      }
      t.getMessage shouldBe "Unknown command: garbage"
      t.docopt shouldBe MarkdGo.Doc
    }
  }

  describe("MarkdGo beautify") {
    it("throws an exception if no files are specified") {
      val t = intercept[MarkdGo.InternalDocoptException] {
        withGo("beautify")
      }
      t.getMessage shouldBe null
      t.docopt shouldBe BeautifyTask.Doc
    }
  }

  describe("MarkdGo datecount") {
    it("throws an exception if no files are specified") {
      val t = intercept[MarkdGo.InternalDocoptException] {
        withGo("datecount")
      }
      t.getMessage shouldBe null
      t.docopt shouldBe DateCountdownTask.Doc
    }

    it("should process a Table") {
      val md = Table
        .parse("""# | Top T0      | Bottom T0 | No T0
           !---|---|---|---
           !a | T 2000-01-01   |
           !b | T-0 1999-01-01 |
           !c | T0 1999-01-01  |
           !d | T+0 1999-01-01 |
           !e | T-1 2000-01-01 | T-1 2000-01-01 | T-1 2000-01-01
           !f | T1 2000-01-01  | T1 2000-01-01  | T1 2000-01-01
           !g | T+1 2000-01-01 | T+1 2000-01-01 | T+1 2000-01-01
           !h | T-X 2000-01-02 | T-X 2000-01-02 | T-X 2000-01-02
           !i | T+X 1999-12-31 | T+X 1999-12-31 | T+X 1999-12-31
           !j |                | T 2000-01-01
           !k |                | T-0 1999-01-01
           !l |                | T0 1999-01-01
           !m |                | T+0 1999-01-01
           !""".stripMargin('!'))
        .get

      val cleaned = DateCountdownTask.process(md)
      cleaned.build().toString shouldBe
        """| # | Top T0         | Bottom T0      | No T0          |
          !|---|----------------|----------------|----------------|
          !| a | T 2000-01-01   |                |                |
          !| b | T 2000-01-01   |                |                |
          !| c | T 2000-01-01   |                |                |
          !| d | T 2000-01-01   |                |                |
          !| e | T-1 1999-12-31 | T-1 1999-12-31 | T-1 2000-01-01 |
          !| f | T+1 2000-01-02 | T+1 2000-01-02 | T+1 2000-01-03 |
          !| g | T+1 2000-01-02 | T+1 2000-01-02 | T+1 2000-01-03 |
          !| h | T+1 2000-01-02 | T+1 2000-01-02 | T 2000-01-02   |
          !| i | T-1 1999-12-31 | T-1 1999-12-31 | T-2 1999-12-31 |
          !| j |                | T 2000-01-01   |                |
          !| k |                | T 2000-01-01   |                |
          !| l |                | T 2000-01-01   |                |
          !| m |                | T 2000-01-01   |                |
          !""".stripMargin('!')

      DateCountdownTask.process(cleaned) shouldBe cleaned

      DateCountdownTask.process(
        Table
          .parse("""# | Top T0      | Bottom T0 | No T0
            !---|---|---|---
            !a | 2000-01-01 T   |
            !b | 1999-01-01 T-0 |
            !c | 1999-01-01 T0  |
            !d | 1999-01-01 T+0 |
            !e | 2000-01-01 T-1 | 2000-01-01 T-1 | 2000-01-01 T-1
            !f | 2000-01-01 T1  | 2000-01-01 T1  | 2000-01-01 T1
            !g | 2000-01-01 T+1 | 2000-01-01 T+1 | 2000-01-01 T+1
            !h | 2000-01-02 T-X | 2000-01-02 T-X | 2000-01-02 T-X
            !i | 1999-12-31 T+X | 1999-12-31 T+X | 1999-12-31 T+X
            !j |                | 2000-01-01 T
            !k |                | 1999-01-01 T-0
            !l |                | 1999-01-01 T0
            !m |                | 1999-01-01 T+0
            !""".stripMargin('!'))
          .get
      ) shouldBe cleaned
    }

    it("should retain some markdown around modified cells") {
      val md = Table
        .parse("""|Date |
            !|---|
            !| *T* 2000-01-01
            !| **T** 1999-01-01
            !| _T-1_ 1999-01-01
            !| __T0__ 1999-01-01
            !| (T-1) 1999-01-01
            !| [T+2] 1999-01-01
            !| T+3 *1999-01-01*
            !| T+4 **1999-01-01**
            !| T+5 _1999-01-01_
            !| T+6 __1999-01-01__
            !| T+7 (1999-01-01)
            !| T+8 [1999-01-01]
            !| T+9 [1999-01-01][l1]
            !| T+10 **[1999-01-01](http://l2)**
            !| T+11 1999-01-01 Lots of other stuff
            !| Lots of other stuff T+12 1999-01-01
            !| *T+X* 2000-01-02
            !| **T+X** 2000-01-03
            !| _T+X_ 2000-01-04
            !| __T+X__ 2000-01-05
            !| (T+X) 2000-01-06
            !| [T+X] 2000-01-07
            !""".stripMargin('!'))
        .get

      val cleaned = DateCountdownTask.process(md)
      cleaned.build().toString shouldBe
        """| Date                                |
          !|-------------------------------------|
          !| *T* 2000-01-01                      |
          !| **T** 2000-01-01                    |
          !| _T-1_ 1999-12-31                    |
          !| __T__ 2000-01-01                    |
          !| (T-1) 1999-12-31                    |
          !| [T+2] 2000-01-03                    |
          !| T+3 *2000-01-04*                    |
          !| T+4 **2000-01-05**                  |
          !| T+5 _2000-01-06_                    |
          !| T+6 __2000-01-07__                  |
          !| T+7 (2000-01-08)                    |
          !| T+8 [2000-01-09]                    |
          !| T+9 [2000-01-10][l1]                |
          !| T+10 **[2000-01-11](http://l2)**    |
          !| T+11 2000-01-12 Lots of other stuff |
          !| Lots of other stuff T+12 2000-01-13 |
          !| *T+1* 2000-01-02                    |
          !| **T+2** 2000-01-03                  |
          !| _T+3_ 2000-01-04                    |
          !| __T+4__ 2000-01-05                  |
          !| (T+5) 2000-01-06                    |
          !| [T+6] 2000-01-07                    |
          !""".stripMargin('!')

      DateCountdownTask.process(cleaned) shouldBe cleaned

      // TODO: Note that the T+10 isn't exactly what we're looking for.  The regex should be adjusted to include links.
      DateCountdownTask
        .process(
          Table
            .parse("""|Date |
             !|---|
             !| 2000-01-01 *T*
             !| 1999-01-01 **T**
             !| 1999-01-01 _T-1_
             !| 1999-01-01 __T0__
             !| 1999-01-01 (T-1)
             !| 1999-01-01 [T+2]
             !| *1999-01-01* T+3
             !| **1999-01-01** T+4
             !| _1999-01-01_ T+5
             !|  __1999-01-01__ T+6
             !|  (1999-01-01) T+7
             !| [1999-01-01] T+8
             !| [1999-01-01] T+9[l1]
             !| **[1999-01-01] T+10(http://l2)**
             !| 1999-01-01 T+11 Lots of other stuff
             !| Lots of other stuff 1999-01-01 T+12
             !| 2000-01-02 *T+X*
             !| 2000-01-03 **T+X**
             !| 2000-01-04 _T+X_
             !| 2000-01-05 __T+X__
             !| 2000-01-06 (T+X)
             !| 2000-01-07 [T+X]
             !""".stripMargin('!'))
            .get
        )
        .build()
        .toString shouldBe cleaned.build().toString
    }

    it("rearrange countdowns and dates") {
      val md = Table
        .parse("""|Date |
             !|---|
             !| *T* 2000-01-01
             !| [T+3) *1999-01-01_
             !""".stripMargin('!'))
        .get

      val cleaned = DateCountdownTask.process(md)
      cleaned.build().toString shouldBe
        """| Date               |
          !|--------------------|
          !| *T* 2000-01-01     |
          !| [T+3) *2000-01-04_ |
          !""".stripMargin('!')

      DateCountdownTask.process(cleaned) shouldBe cleaned

      DateCountdownTask
        .process(
          Table
            .parse("""|Date |
                 !|---|
                 !| *T* 2000-01-01
                 !| *1999-01-01_ [T+3)
                 !""".stripMargin('!'))
            .get
        )
        .build()
        .toString shouldBe cleaned.build().toString
    }
  }

  for (task <- MarkdGo.Tasks) {
    describe(s"MarkdGo ${task.Cmd} docopt check") {
      it("should have less than 80 characters per string for readability") {
        for (line <- task.Doc.split("\n")) {
          withClue(task.Cmd -> line) {
            line.length should be < 80
          }
        }
      }
    }

    describe(s"MarkdGo ${task.Cmd} invalid command lines") {
      it("throws an exception if no files are specified") {
        val t = intercept[MarkdGo.InternalDocoptException] {
          withGo(task.Cmd)
        }
        t.getMessage shouldBe null
        t.docopt shouldBe task.Doc
      }

      it("throws an exception with --version") {
        val t = intercept[DocoptExitException] {
          withGo(task.Cmd, "--version")
        }
        t.getExitCode shouldBe 0
        t.getMessage shouldBe MarkdGo.Version
      }

      it("throws an exception with --help") {
        val t = intercept[DocoptExitException] {
          withGo(task.Cmd, "--help")
        }
        t.getExitCode shouldBe 0
        t.getMessage shouldBe task.Doc
      }

      it(s"throws an exception with unknown options") {
        for (
          args <- Seq(
            Seq(task.Cmd, "--garbage"),
            Seq(task.Cmd, "--debug", "--garbage"),
            Seq(task.Cmd, "--garbage", "--debug"),
            Seq(task.Cmd, "--garbage", "garbage")
          )
        ) withClue(s"Using: $args") {
          val t = intercept[MarkdGo.InternalDocoptException] {
            withGo(args: _*)
          }
          t.docopt shouldBe task.Doc
          t.getMessage shouldBe null
        }
      }
    }
  }
}
