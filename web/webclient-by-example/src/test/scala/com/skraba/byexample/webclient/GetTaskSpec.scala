package com.skraba.byexample.webclient

import com.skraba.byexample.scalatra.{ScalatraGoServer, ServeJarResourceTask}
import com.skraba.docoptcli.DocoptCliGoSpec
import sttp.model.StatusCodes

/** Unit tests for [[GetTaskSpec]]. */
class GetTaskSpec extends DocoptCliGoSpec(WebClientGo, Some(GetTask)) with StatusCodes {

  val Srv = new ScalatraGoServer(Seq(ServeJarResourceTask.Cmd))

  override def afterAll(): Unit = {
    super.afterAll()
    Srv.shutdown()
  }

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    // TODO: Imcompatible flags, missing arguments
  }

  for (cmd <- Seq("--sttp", /* "--pekko" ,*/ "")) {
    describe(s"${Cli.Cli} $TaskCmd ${if (cmd.nonEmpty) s"with $cmd" else "with default"}") {
      it("should get a URI") {
        val args =
          if (cmd.nonEmpty) Seq(GetTask.Cmd, cmd, Srv.base.withWholePath("index.html"))
          else Seq(GetTask.Cmd, Srv.base.withWholePath("index.html"))
        withGoMatching(args: _*) { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout shouldBe
            """<html>
              |<body>
              |<h1>Hello world!</h1>
              |</body>
              |</html>
              |""".stripMargin
        }
      }
    }
  }
}
