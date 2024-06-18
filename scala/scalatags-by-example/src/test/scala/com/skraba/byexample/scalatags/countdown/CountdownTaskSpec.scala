package com.skraba.byexample.scalatags.countdown

import com.skraba.byexample.scalatags.ScalatagsGoSpec.withScalatagsGoMatch
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for the [[CountdownTask]] CLI.
  */
class CountdownTaskSpec extends AnyFunSpecLike with Matchers {

  describe("CountdownTaskSpec manual test") {
    it("should run") {
      withScalatagsGoMatch(
        CountdownTask.Cmd,
        "--dstDir",
        "/tmp/timerx",
        "/home/ryan.skraba/working/projects/byexample/scala/scalatags-by-example/src/test/resources/timer.svg"
      ) { case (stdout, stderr) =>
        stderr shouldBe ""
      }
    }
  }
}
