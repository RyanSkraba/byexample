package com.skraba.byexample.scalatools.gtd

import com.tinfoiled.markd.Markd
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Project]]. */
class ProjectSpec extends AnyFunSpecLike with Matchers {

  val gtd = GettingThingsDone(
    Markd(),
    Some(Markd.parse("""Projects | Title | Issue Reference | Issue Link | Pull Request Reference | Pull Request Link
      |---------|-------|-----------------|------------|------------------------|---------
      |avro     |       |                 |            |                        |
      |parquet  |       |                 |            | apache/parquet-java#   |
      |iceberg  |       | apache/iceberg# |            |                        |
      |""".stripMargin))
  )

  val Avro: Project = Project(
    "avro",
    "Avro",
    "AVRO-",
    "https://issues.apache.org/jira/browse/AVRO-",
    "apache/avro#",
    "https://github.com/apache/avro/pull/"
  )

  val Parquet: Project = Project(
    "parquet",
    "Parquet",
    "PARQUET-",
    "https://issues.apache.org/jira/browse/PARQUET-",
    "apache/parquet-java#",
    "https://github.com/apache/parquet-java/pull/"
  )

  val Iceberg: Project = Project(
    "iceberg",
    "Iceberg",
    "apache/iceberg#",
    "https://github.com/apache/iceberg/issues/",
    "apache/iceberg#",
    "https://github.com/apache/iceberg/pull/"
  )

  describe(s"Creating an Project") {
    it("should not supply defaults unless requested") {
      Project(parse = false, "avro") shouldBe Project("avro", "", "", "", "", "")
      Project(parse = false, "avro", "a", "b", "c", "d", "e") shouldBe Project("avro", "a", "b", "c", "d", "e")
      Project(parse = true, "avro", "a", "b", "c", "d", "e") shouldBe Project("avro", "a", "b", "c", "d", "e")
    }

    it("should choose all appropriate defaults if requested") {
      Project(parse = true, "avro") shouldBe Avro
    }

    it("should choose appropriate defaults when the issue link looks like a github reference") {
      Project(parse = true, "avro", issueRef = "apache/avro#") shouldBe Avro.copy(
        issueRef = "apache/avro#",
        issueLink = "https://github.com/apache/avro/issues/"
      )
    }

    it("should choose an apropriate title based on the tag") {
      Project(parse = true, "avro").title shouldBe "Avro"
      Project(parse = true, "Avro").title shouldBe "Avro"
      Project(parse = true, "AVRO").title shouldBe "Avro"
      Project(parse = true, "AVRO", title = "OVERRIDE").title shouldBe "OVERRIDE"
    }

    it("should choose an apropriate issueRef based on the tag") {
      Project(parse = true, "avro").issueRef shouldBe Avro.issueRef
      Project(parse = true, "Avro").issueRef shouldBe Avro.issueRef
      Project(parse = true, "AVRO").issueRef shouldBe Avro.issueRef
      Project(parse = true, "avro", issueRef = "OVERRIDE").issueRef shouldBe "OVERRIDE"
    }

    it("should choose an apropriate issueLink based on the tag and issueRef") {
      Project(parse = true, "avro").issueLink shouldBe Avro.issueLink
      Project(parse = true, "Avro").issueLink shouldBe Avro.issueLink
      Project(parse = true, "AVRO").issueLink shouldBe Avro.issueLink
      Project(parse = true, "avro", issueRef = "a").issueLink shouldBe Avro.issueLink
      Project(parse = true, "avro", issueRef = "a#").issueLink shouldBe Avro.issueLink
      Project(parse = true, "avro", issueRef = "a/b").issueLink shouldBe Avro.issueLink
      Project(parse = true, "avro", issueRef = "a/b#").issueLink shouldBe "https://github.com/a/b/issues/"
      Project(parse = true, "avro", issueRef = "a/b/c").issueLink shouldBe Avro.issueLink
      Project(parse = true, "avro", issueRef = "a/b/c#").issueLink shouldBe Avro.issueLink
      Project(parse = true, "avro", issueLink = "OVERRIDE").issueLink shouldBe "OVERRIDE"
      Project(parse = true, "avro", issueRef = "a/b#", issueLink = "OVERRIDE").issueLink shouldBe "OVERRIDE"
    }

    it("should choose an apropriate prRef based on the tag and issueRef") {
      Project(parse = true, "avro").prRef shouldBe Avro.prRef
      Project(parse = true, "Avro").prRef shouldBe Avro.prRef
      Project(parse = true, "AVRO").prRef shouldBe Avro.prRef
      Project(parse = true, "avro", issueRef = "a").prRef shouldBe Avro.prRef
      Project(parse = true, "avro", issueRef = "a#").prRef shouldBe Avro.prRef
      Project(parse = true, "avro", issueRef = "a/b").prRef shouldBe Avro.prRef
      Project(parse = true, "avro", issueRef = "a/b#").prRef shouldBe "a/b#"
      Project(parse = true, "avro", issueRef = "a/b/c").prRef shouldBe Avro.prRef
      Project(parse = true, "avro", issueRef = "a/b/c#").prRef shouldBe Avro.prRef
      Project(parse = true, "avro", issueRef = "a/b#", prRef = "OVERRIDE").prRef shouldBe "OVERRIDE"
    }

    it("should choose an apropriate prLink based on the tag and prRef") {
      Project(parse = true, "avro").prLink shouldBe Avro.prLink
      Project(parse = true, "Avro").prLink shouldBe Avro.prLink
      Project(parse = true, "AVRO").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", issueRef = "a").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", issueRef = "a#").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", issueRef = "a/b").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", issueRef = "a/b#").prLink shouldBe "https://github.com/a/b/pull/"
      Project(parse = true, "avro", issueRef = "a/b/c").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", issueRef = "a/b/c#").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", prRef = "a").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", prRef = "a#").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", prRef = "a/b").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", prRef = "a/b#").prLink shouldBe "https://github.com/a/b/pull/"
      Project(parse = true, "avro", prRef = "a/b/c").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", prRef = "a/b/c#").prLink shouldBe Avro.prLink
      Project(parse = true, "avro", prLink = "OVERRIDE").prLink shouldBe "OVERRIDE"
      Project(parse = true, "avro", issueRef = "a/b#", prLink = "OVERRIDE").prLink shouldBe "OVERRIDE"
      Project(parse = true, "avro", prRef = "a/b#", prLink = "OVERRIDE").prLink shouldBe "OVERRIDE"
    }

    for ((tmpl, replaced) <- Seq("ABC" -> "ABC123", "ABC-" -> "ABC-123", "ABC/%s#" -> "ABC/123#", "#%s#" -> "#123#"))
      it(s"should appropriately replace values: $tmpl -> $replaced") {
        Project(parse = true, "x", issueRef = tmpl).issueRefOf("123") shouldBe replaced
        Project(parse = true, "x", issueLink = tmpl).issueLinkOf("123") shouldBe replaced
        Project(parse = true, "x", prRef = tmpl).prRefOf("123") shouldBe replaced
        Project(parse = true, "x", prLink = tmpl).prLinkOf("123") shouldBe replaced
      }
  }

  describe(s"Creating an ASF style Project") {
    it("should choose appropriate defaults") {
      Project.asf("avro") shouldBe Avro
      Avro.issueLinkOf("123") shouldBe "https://issues.apache.org/jira/browse/AVRO-123"
      Avro.issueRefOf("123") shouldBe "AVRO-123"
      Avro.prLinkOf("123") shouldBe "https://github.com/apache/avro/pull/123"
      Avro.prRefOf("123") shouldBe "apache/avro#123"
    }

    it("should allow customizing the repo or issue") {
      Project.asf("parquet", "Parquet", "PARQUET", "parquet-java") shouldBe Parquet
      Parquet.issueLinkOf("123") shouldBe "https://issues.apache.org/jira/browse/PARQUET-123"
      Parquet.issueRefOf("123") shouldBe "PARQUET-123"
      Parquet.prLinkOf("123") shouldBe "https://github.com/apache/parquet-java/pull/123"
      Parquet.prRefOf("123") shouldBe "apache/parquet-java#123"
    }
  }

  describe(s"Read a project list from a GettingThingsDone configuration") {
    it("should parse the lines with the defaults") {
      val prjs = Project.fromCfg(gtd)
      prjs should have size 3
      prjs.head shouldBe Avro
      prjs(1) shouldBe Parquet
      prjs(2) shouldBe Iceberg
    }
  }

}
