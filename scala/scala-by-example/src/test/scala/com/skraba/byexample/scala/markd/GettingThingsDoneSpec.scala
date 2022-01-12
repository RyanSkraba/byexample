package com.skraba.byexample.scala.markd

import com.skraba.byexample.scala.markd.GettingThingsDone.{
  DoneToDo,
  H1Weekly,
  nextWeekStart
}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate}

/** Unit tests for [[GettingThingsDone]]
  */
class GettingThingsDoneSpec extends AnyFunSpecLike with Matchers {

  /** Next monday, which is the default week start when initializing the statuses. */
  val defaultNextWeekStart: String = nextWeekStart(None)

  /** The default day of the week. */
  val todayDayOfWeek: String =
    LocalDate.now.getDayOfWeek.toString.take(3).toLowerCase.capitalize

  /** Prepend a comment with a given text to the contents of the header. */
  def preComment(text: String = "Hi")(h: Header): Header =
    h.copyMds(Comment(text) +: h.mds)

  describe(s"Updating the $H1Weekly section") {

    /** Some text to be parsed into an status document. */
    val original =
      s"""
         |# H1 One
         |1
         |# H1 Two
         |2
         |# H1 Three
         |3""".stripMargin

    it("should add itself to an empty document") {
      val empty = GettingThingsDone("")
      val updated = empty.updateH1Weekly(preComment("empty"))
      updated.doc shouldBe Header(0, "", Header(1, H1Weekly, Comment("empty")))
    }

    it(s"should add itself document where the section doesn't exist") {
      val existing = GettingThingsDone(original)
      val updated = existing.updateH1Weekly(preComment("existing"))
      updated.doc shouldBe Header(
        0,
        "",
        Header(1, "H1 One", Paragraph("1")),
        Header(1, "H1 Two", Paragraph("2")),
        Header(1, "H1 Three", Paragraph("3")),
        Header(1, H1Weekly, Comment("existing"))
      )
    }

    describe("should update an existing section") {
      it("at the start") {
        val existing =
          GettingThingsDone(original.replace("H1 One", H1Weekly))
        val updated = existing.updateH1Weekly(preComment("un"))
        updated.doc shouldBe Header(
          0,
          "",
          Header(1, H1Weekly, Comment("un"), Paragraph("1")),
          Header(1, "H1 Two", Paragraph("2")),
          Header(1, "H1 Three", Paragraph("3"))
        )
      }

      it("in the middle") {
        val existing =
          GettingThingsDone(original.replace("H1 Two", H1Weekly))
        val updated = existing.updateH1Weekly(preComment("deux"))
        updated.doc shouldBe Header(
          0,
          "",
          Header(1, "H1 One", Paragraph("1")),
          Header(1, H1Weekly, Comment("deux"), Paragraph("2")),
          Header(1, "H1 Three", Paragraph("3"))
        )
      }

      it("at the end") {
        val existing =
          GettingThingsDone(original.replace("H1 Three", H1Weekly))
        val updated = existing.updateH1Weekly(preComment("trois"))
        updated.doc shouldBe Header(
          0,
          "",
          Header(1, "H1 One", Paragraph("1")),
          Header(1, "H1 Two", Paragraph("2")),
          Header(1, H1Weekly, Comment("trois"), Paragraph("3"))
        )
      }
    }

    it("should add the latest week to an empty document") {
      val empty = GettingThingsDone("")
      val updated = empty.updateTopWeek(preComment("empty"))
      updated.doc shouldBe Header(
        0,
        "",
        Header(1, H1Weekly, Header(2, defaultNextWeekStart, Comment("empty")))
      )
    }

    it("should add the latest week where one doesn't exist") {
      val empty = GettingThingsDone(s"# $H1Weekly")
      val updated = empty.updateTopWeek(preComment("existing"))
      updated.doc shouldBe Header(
        0,
        "",
        Header(
          1,
          H1Weekly,
          Header(2, defaultNextWeekStart, Comment("existing"))
        )
      )
    }

    it("should add the update the latest week where it exists") {
      val existing =
        GettingThingsDone(
          original.replace("H1 Two", s"$H1Weekly\n## Top week\n## Next week")
        )
      existing.doc shouldBe Header(
        0,
        "",
        Header(1, "H1 One", Paragraph("1")),
        Header(
          1,
          H1Weekly,
          Header(2, "Top week"),
          Header(2, "Next week", Paragraph("2"))
        ),
        Header(1, "H1 Three", Paragraph("3"))
      )

      val updated = existing.updateTopWeek(preComment("update"))
      updated.doc shouldBe Header(
        0,
        "",
        Header(1, "H1 One", Paragraph("1")),
        Header(
          1,
          H1Weekly,
          Header(2, "Top week", Comment("update")),
          Header(2, "Next week", Paragraph("2"))
        ),
        Header(1, "H1 Three", Paragraph("3"))
      )

    }
  }

  describe(s"Updating statistics") {

    val withWeeklyStats =
      s"""Weekly Status
         !==============================================================================
         !
         !Top week
         !------------------------------------------------------------------------------
         !
         !| Stats  | Mon | Tue | Wed | Thu | Fri  | Sat | Sun   |
         !|--------|-----|-----|-----|-----|------|-----|-------|
         !| unread | 1   | 22  |     | 333 | 4444 |     | 55555 |
         !""".stripMargin('!')

    it("should add itself to an empty document") {
      val empty = GettingThingsDone("")
      val updated = empty.updateTopWeekStats("unread", "123", Some("Sun"))
      updated.doc.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !$defaultNextWeekStart
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
           !|--------|-----|-----|-----|-----|-----|-----|-----|
           !| unread |     |     |     |     |     |     | 123 |
           !""".stripMargin('!')
    }

    it("should update an empty cell in an existing document") {
      val existing = GettingThingsDone(withWeeklyStats)
      val updated = existing.updateTopWeekStats("unread", "123", Some("Wed"))
      updated.doc.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !Top week
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed | Thu | Fri  | Sat | Sun   |
           !|--------|-----|-----|-----|-----|------|-----|-------|
           !| unread | 1   | 22  | 123 | 333 | 4444 |     | 55555 |
           !""".stripMargin('!')
    }

    it("should update an existing cell in an existing document") {
      val existing = GettingThingsDone(withWeeklyStats)
      val updated = existing.updateTopWeekStats("unread", "123", Some("Wed"))
      updated.doc.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !Top week
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed | Thu | Fri  | Sat | Sun   |
           !|--------|-----|-----|-----|-----|------|-----|-------|
           !| unread | 1   | 22  | 123 | 333 | 4444 |     | 55555 |
           !""".stripMargin('!')
    }

    it("should add itself to an empty document on today's day") {
      val empty = GettingThingsDone("")
      val updated = empty.updateTopWeekStats("unread", "987")
      updated.doc.build().toString() should include("| 987 |")
      // Calling without a day should be the same as calling with today's day of the week.
      updated shouldBe empty.updateTopWeekStats(
        "unread",
        "987",
        Some(todayDayOfWeek)
      )
    }

    it("should add itself to an existing document on today's day") {
      val existing = GettingThingsDone(withWeeklyStats)
      val updated = existing.updateTopWeekStats("unread", "987")
      updated.doc.build().toString() should include("| 987 |")
      // Calling without a day should be the same as calling with today's day of the week.
      updated shouldBe existing.updateTopWeekStats(
        "unread",
        "987",
        Some(todayDayOfWeek)
      )
    }

    it("should add a new row in an existing document") {
      val existing = GettingThingsDone(withWeeklyStats)
      val updated = existing.updateTopWeekStats("read", "99999", Some("Wed"))
      updated.doc.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !Top week
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed   | Thu | Fri  | Sat | Sun   |
           !|--------|-----|-----|-------|-----|------|-----|-------|
           !| unread | 1   | 22  |       | 333 | 4444 |     | 55555 |
           !| read   |     |     | 99999 |     |      |     |       |
           !""".stripMargin('!')
    }

  }

  describe(s"Updating tasks to do") {

    val withWeeklyToDo =
      s"""Weekly Status
         !==============================================================================
         !
         !Top week
         !------------------------------------------------------------------------------
         !
         !| To Do  | Notes      |
         !|--------|------------|
         !| Baking | Make bread |
         !""".stripMargin('!')

    it("should add itself to an empty document") {
      val empty = GettingThingsDone("")
      val updated = empty.updateTopWeekToDo("Baking", "Make bread")
      updated.doc.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !$defaultNextWeekStart
           !------------------------------------------------------------------------------
           !
           !| To Do  | Notes      |
           !|--------|------------|
           !| Baking | Make bread |
           !""".stripMargin('!')
    }

    it("should add itself in an existing document") {
      val existing = GettingThingsDone(withWeeklyToDo)
      val updated =
        existing.updateTopWeekToDo("Baking", "Make muffins", DoneToDo)
      updated.doc.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !Top week
           !------------------------------------------------------------------------------
           !
           !| To Do    | Notes        |
           !|----------|--------------|
           !| Baking   | Make bread   |
           !| 🟢Baking | Make muffins |
           !""".stripMargin('!')
    }
  }

  describe("Utility for calculating a new week") {

    it("should ignore suffixes") {
      nextWeekStart(
        Some("2021/09/05 Extra")
      ) shouldBe "2021/09/06"
    }

    it(s"should calculate from today if None") {
      val today =
        Some(LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy/MM/dd")))
      nextWeekStart(None) shouldBe nextWeekStart(today)
    }

    it(s"should allow the week start to be selectable") {
      nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.SUNDAY
      ) shouldBe "2021/09/05"
      nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.MONDAY
      ) shouldBe "2021/09/06"
      nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.TUESDAY
      ) shouldBe "2021/09/07"
      nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.WEDNESDAY
      ) shouldBe "2021/09/08"
      nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.THURSDAY
      ) shouldBe "2021/09/02"
      nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.FRIDAY
      ) shouldBe "2021/09/03"
      nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.SATURDAY
      ) shouldBe "2021/09/04"
    }

    for (
      date <- Seq(
        "2021/08/30",
        "2021/08/31",
        "2021/09/01",
        "2021/09/02",
        "2021/09/03",
        "2021/09/04",
        "2021/09/05"
      )
    ) {
      it(s"should return next 2021/09/07 from a $date") {
        nextWeekStart(Some(date)) shouldBe "2021/09/06"
      }
    }
  }

}
