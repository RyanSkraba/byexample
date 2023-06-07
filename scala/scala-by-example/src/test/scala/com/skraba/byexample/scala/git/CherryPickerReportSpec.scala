package com.skraba.byexample.scala.git

import com.skraba.byexample.scala.markd.Header
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import os.CommandResult

import scala.util.Try

/** Unit tests for [[CherryPickerReport]]
  */
class CherryPickerReportSpec extends AnyFunSpecLike with Matchers {

  describe("Test") {
    ignore("works") {
      val x: Either[CherryPickerReport, Try[CommandResult]] =
        CherryPickerReport.fromGit(
          (os.home / "working" / "apache" / "avro").toString,
          "master",
          "branch-1.11"
        )

      val y = CherryPickerReport.fromDoc(
        Header.parse(
          """| Project Info |                                            |
           |--------------|--------------------------------------------|
           | Issues       | https://issues.apache.org/jira/browse/AVRO |
           |""".stripMargin
        )
      )

      x.swap.map(y.update).map(_.toDoc).foreach(z => println(z))
    }
  }
}
