package com.skraba.byexample.scala.git

import com.skraba.byexample.scala.git.CherryPickerReport
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[CherryPickerReport]]
  */
class CherryPickerReportSpec extends AnyFunSpecLike with Matchers {

  describe("Test") {
    ignore("works") {
      CherryPickerReport.fromGit(
        (os.home / "working" / "apache" / "avro").toString,
        "master",
        "branch-1.11"
      )
    }
  }
}
