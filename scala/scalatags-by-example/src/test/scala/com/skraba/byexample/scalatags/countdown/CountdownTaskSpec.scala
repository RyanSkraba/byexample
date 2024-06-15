package com.skraba.byexample.scalatags.countdown

import com.skraba.byexample.scalatags.ScalatagsGoSpec.withScalatagsGoMatch
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for the [[CountdownTask]] CLI.
  */
class CountdownTaskSpec extends AnyFunSpecLike with Matchers {

  describe("CountdownTaskSpec manual test") {
    ignore("should run") {
      withScalatagsGoMatch(
        CountdownTask.Cmd,
        "--dstDir",
        "/tmp/timer",
        "timer.svg"
      ) { case (stdout, stderr) =>
        stderr shouldBe ""
      }
    }
  }
}
