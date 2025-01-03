package com.skraba.byexample.scala.ammonite.gtd

import com.skraba.byexample.scala.ammonite.gtd.GettingThingsDone._
import com.skraba.byexample.scala.markd.{Comment, Header, Paragraph}
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate}

/** Unit tests for [[GettingThingsDone]]
  */
class GettingThingsDoneSpec extends AnyFunSpecLike with Matchers {

  /** Next monday, which is the default week start when initializing the statuses.
    */
  val defaultNextWeekStart: String = nextWeekStart(None)

  /** The default day of the week. */
  val todayDayOfWeek: String = LocalDate.now.getDayOfWeek.toString.take(3).toLowerCase.capitalize

  /** A function to prepend a comment with a given text to the contents of the header.
    */
  def preComment(text: String = "Hi")(h: Header): Header =
    h.copyMds(Comment(text) +: h.mds)

  /** Some text to be parsed into an status document. */
  val original =
    s"""
       |# H1 One
       |1
       |# H1 Two
       |2
       |# H1 Three
       |3""".stripMargin

  describe(s"Creating a clean document") {

    it("should be readable and self-describing") {
      GettingThingsDone().h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !<!--
           !| To Do      | Notes 🟢🔵🔶🟥⤴️🕒                                           |
           !|------------|--------------------------------------------------------------|
           !| 🟢Tech     | **Did the thing** and some notes                             |
           !| 🔵Finance  | **Thing was did** Done by someone else, or worked around     |
           !| 🔶Health   | **Ready to take** or paused, or to think about               |
           !| 🟥Personal | **Won't do** and here's why                                  |
           !| ⤴️Personal | **Read Getting Things Done Chapter 4/12** Moved to later     |
           !| 🕒Proj     | **[PROJ-1234]**:[org/proj#4321] Fix all the things `WAITING` |
           !| Pro        | **Another task** With some [details][YYYYMMDD-1]             |
           !-->
           !
           !$defaultNextWeekStart
           !------------------------------------------------------------------------------
           !
           !| Stats   | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
           !|---------|-----|-----|-----|-----|-----|-----|-----|
           !| pushups |     |     |     |     |     |     |     |
           !
           !* Something I did this week
           !""".stripMargin('!')
    }
  }

  describe(s"Creating a document with a config section") {

    it("should be readable and self-formatting") {
      GettingThingsDone(s"""Weekly Status
           !==============================================================================
           !
           !<!-- $CommentConfig
           !# This comment text will be beautified
           !
           !| AAA | BBB |
           !|---|---|
           !|1|2|
           !-->
           !
           !# This out-of-comment text will also be beautified
           !Hello
           !<!--
           !# This comment text will not be beautified
           !-->
           !""".stripMargin('!')).h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !<!-- $CommentConfig
           !
           !This comment text will be beautified
           !==============================================================================
           !
           !| AAA | BBB |
           !|-----|-----|
           !| 1   | 2   |
           !
           !-->
           !
           !This out-of-comment text will also be beautified
           !==============================================================================
           !
           !Hello
           !
           !<!--
           !# This comment text will not be beautified
           !-->
           !""".stripMargin('!')
    }
  }

  describe(s"Updating a top-level section") {

    it("should add itself to an empty document") {
      val empty = GettingThingsDone("")
      empty.h0 shouldBe Header(0, "")

      val updated = empty.updateHeader1("New")(preComment("empty"))
      updated.h0 shouldBe Header(
        0,
        "",
        Header(1, "New", Comment("empty"))
      )
    }

    it(s"should add itself document where the section doesn't exist") {
      val updated =
        GettingThingsDone(original).updateHeader1("New")(preComment("add"))
      updated.h0 shouldBe Header(
        0,
        "",
        Header(1, "H1 One", Paragraph("1")),
        Header(1, "H1 Two", Paragraph("2")),
        Header(1, "H1 Three", Paragraph("3")),
        Header(1, "New", Comment("add"))
      )
    }

    describe("should update an existing section") {
      it("at the start") {
        val updated =
          GettingThingsDone(original).updateHeader1("H1 One")(preComment("one"))
        updated.h0 shouldBe Header(
          0,
          "",
          Header(1, "H1 One", Comment("one"), Paragraph("1")),
          Header(1, "H1 Two", Paragraph("2")),
          Header(1, "H1 Three", Paragraph("3"))
        )
      }

      it("in the middle") {
        val updated =
          GettingThingsDone(original).updateHeader1("H1 Two")(preComment("two"))
        updated.h0 shouldBe Header(
          0,
          "",
          Header(1, "H1 One", Paragraph("1")),
          Header(1, "H1 Two", Comment("two"), Paragraph("2")),
          Header(1, "H1 Three", Paragraph("3"))
        )
      }

      it("at the end") {
        val updated = GettingThingsDone(original).updateHeader1("H1 Three")(
          preComment("three")
        )
        updated.h0 shouldBe Header(
          0,
          "",
          Header(1, "H1 One", Paragraph("1")),
          Header(1, "H1 Two", Paragraph("2")),
          Header(1, "H1 Three", Comment("three"), Paragraph("3"))
        )
      }
    }
  }

  describe(s"Updating the $H1Weeklies section") {

    it("should add itself to an empty document") {
      val empty = GettingThingsDone("")
      empty.weeklies shouldBe None

      val updated = empty.updateWeeklies(preComment("empty"))
      updated.weeklies.value shouldBe Header(1, H1Weeklies, Comment("empty"))
      updated.h0 shouldBe Header(
        0,
        "",
        Header(1, H1Weeklies, Comment("empty"))
      )
    }

    it(s"should add itself document where the section doesn't exist") {
      val existing = GettingThingsDone(original)
      existing.weeklies shouldBe None

      val updated = existing.updateWeeklies(preComment("existing"))
      updated.weeklies.value shouldBe Header(1, H1Weeklies, Comment("existing"))
      updated.h0 shouldBe Header(
        0,
        "",
        Header(1, "H1 One", Paragraph("1")),
        Header(1, "H1 Two", Paragraph("2")),
        Header(1, "H1 Three", Paragraph("3")),
        Header(1, H1Weeklies, Comment("existing"))
      )
    }

    describe("should update an existing section") {
      it("at the start") {
        val existing =
          GettingThingsDone(original.replace("H1 One", H1Weeklies))
        val updated = existing.updateWeeklies(preComment("un"))
        updated.h0 shouldBe Header(
          0,
          "",
          Header(1, H1Weeklies, Comment("un"), Paragraph("1")),
          Header(1, "H1 Two", Paragraph("2")),
          Header(1, "H1 Three", Paragraph("3"))
        )
      }

      it("in the middle") {
        val existing =
          GettingThingsDone(original.replace("H1 Two", H1Weeklies))
        val updated = existing.updateWeeklies(preComment("deux"))
        updated.h0 shouldBe Header(
          0,
          "",
          Header(1, "H1 One", Paragraph("1")),
          Header(1, H1Weeklies, Comment("deux"), Paragraph("2")),
          Header(1, "H1 Three", Paragraph("3"))
        )
      }

      it("at the end") {
        val existing =
          GettingThingsDone(original.replace("H1 Three", H1Weeklies))
        val updated = existing.updateWeeklies(preComment("trois"))
        updated.h0 shouldBe Header(
          0,
          "",
          Header(1, "H1 One", Paragraph("1")),
          Header(1, "H1 Two", Paragraph("2")),
          Header(1, H1Weeklies, Comment("trois"), Paragraph("3"))
        )
      }
    }

    it("should add the latest week to an empty document") {
      val empty = GettingThingsDone("")
      empty.topWeek shouldBe None

      val updated = empty.updateTopWeek(preComment("empty"))
      updated.weeklies.value shouldBe Header(
        1,
        H1Weeklies,
        Header(2, defaultNextWeekStart, Comment("empty"))
      )
      updated.topWeek.value shouldBe Header(
        2,
        defaultNextWeekStart,
        Comment("empty")
      )
      updated.h0 shouldBe Header(
        0,
        "",
        Header(1, H1Weeklies, Header(2, defaultNextWeekStart, Comment("empty")))
      )
    }

    it("should add the latest week where one doesn't exist") {
      val empty = GettingThingsDone(s"# $H1Weeklies")
      val updated = empty.updateTopWeek(preComment("existing"))
      updated.h0 shouldBe Header(
        0,
        "",
        Header(
          1,
          H1Weeklies,
          Header(2, defaultNextWeekStart, Comment("existing"))
        )
      )
    }

    it("should add the update the latest week where it exists") {
      val existing =
        GettingThingsDone(
          original.replace("H1 Two", s"$H1Weeklies\n## Top week\n## Next week")
        )
      existing.weeklies.value shouldBe Header(
        1,
        H1Weeklies,
        Header(2, "Top week"),
        Header(2, "Next week", Paragraph("2"))
      )
      existing.topWeek.value shouldBe Header(2, "Top week")
      existing.h0 shouldBe Header(
        0,
        "",
        Header(1, "H1 One", Paragraph("1")),
        Header(
          1,
          H1Weeklies,
          Header(2, "Top week"),
          Header(2, "Next week", Paragraph("2"))
        ),
        Header(1, "H1 Three", Paragraph("3"))
      )

      val updated = existing.updateTopWeek(preComment("update"))
      updated.weeklies.value shouldBe Header(
        1,
        H1Weeklies,
        Header(2, "Top week", Comment("update")),
        Header(2, "Next week", Paragraph("2"))
      )
      updated.topWeek.value shouldBe Header(2, "Top week", Comment("update"))
      updated.h0 shouldBe Header(
        0,
        "",
        Header(1, "H1 One", Paragraph("1")),
        Header(
          1,
          H1Weeklies,
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
      updated.h0.build().toString() shouldBe
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
      updated.h0.build().toString() shouldBe
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

    it("should add a column if unknown (not an error)") {
      val existing = GettingThingsDone(withWeeklyStats)
      val updated = existing.updateTopWeekStats("unread", "Vacation", Some(""))
      updated.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !Top week
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed | Thu | Fri  | Sat | Sun   |          |
           !|--------|-----|-----|-----|-----|------|-----|-------|----------|
           !| unread | 1   | 22  |     | 333 | 4444 |     | 55555 | Vacation |
           !""".stripMargin('!')
    }

    it("should update an existing cell in an existing document") {
      val existing = GettingThingsDone(withWeeklyStats)
      val updated = existing.updateTopWeekStats("unread", "123", Some("Wed"))
      updated.topWeek.value.mds.headOption.value.build().toString() shouldBe
        s"""| Stats  | Mon | Tue | Wed | Thu | Fri  | Sat | Sun   |
           !|--------|-----|-----|-----|-----|------|-----|-------|
           !| unread | 1   | 22  | 123 | 333 | 4444 |     | 55555 |
           !""".stripMargin('!')
    }

    it("should add itself to an empty document on today's day") {
      val empty = GettingThingsDone("")
      val updated = empty.updateTopWeekStats("unread", "987")
      updated.h0.build().toString() should include("| 987 |")
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
      updated.h0.build().toString() should include("| 987 |")
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
      updated.topWeek.value.mds.headOption.value.build().toString() shouldBe
        s"""| Stats  | Mon | Tue | Wed   | Thu | Fri  | Sat | Sun   |
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
         !| To Do  | Notes 🟢🔵🔶🟥⤴️🕒 |
         !|--------|-------------------|
         !| Baking | Make bread        |
         !""".stripMargin('!')

    it("should add itself to an empty document") {
      val empty = GettingThingsDone("")
      val updated = empty.addTopWeekToDo("Baking", "Make bread")
      updated.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !$defaultNextWeekStart
           !------------------------------------------------------------------------------
           !
           !| To Do  | Notes 🟢🔵🔶🟥⤴️🕒 |
           !|--------|--------------------|
           !| Baking | Make bread         |
           !""".stripMargin('!')
    }

    it("should add a task to an existing document") {
      val existing = GettingThingsDone(withWeeklyToDo)
      val updated =
        existing.addTopWeekToDo("Cuisine", "Make muffins", state = DoneToDo)
      updated.topWeek.value.mds.headOption.value.build().toString() shouldBe
        s"""| To Do     | Notes 🟢🔵🔶🟥⤴️🕒 |
           !|-----------|--------------------|
           !| Baking    | Make bread         |
           !| 🟢Cuisine | Make muffins       |
           !""".stripMargin('!')

      // These are all equivalent
      val updated2 = existing.updateTopWeekToDo(
        1,
        Some("Cuisine"),
        Some("Make muffins"),
        Some(DoneToDo)
      )
      updated2 shouldBe updated
      val updated3 = existing.updateTopWeekToDo(
        10,
        Some("Cuisine"),
        Some("Make muffins"),
        Some(DoneToDo)
      )
      updated3 shouldBe updated
      val updated4 = existing.updateTopWeekToDo(
        Int.MaxValue,
        Some("Cuisine"),
        Some("Make muffins"),
        Some(DoneToDo)
      )
      updated4 shouldBe updated
    }

    it("should update a task in an existing document") {
      val updated = GettingThingsDone(withWeeklyToDo).updateTopWeekToDo(
        0,
        Some("Cuisine"),
        Some("Make muffins"),
        Some(DoneToDo)
      )
      updated.topWeek.value.mds.headOption.value.build().toString() shouldBe
        s"""| To Do     | Notes 🟢🔵🔶🟥⤴️🕒 |
           !|-----------|--------------------|
           !| 🟢Cuisine | Make muffins       |
           !""".stripMargin('!')
    }

    it("should update a task category in an existing document") {
      val updated = GettingThingsDone(withWeeklyToDo).updateTopWeekToDo(
        0,
        category = Some("Cuisine")
      )
      updated.topWeek.value.mds.headOption.value.build().toString() shouldBe
        s"""| To Do   | Notes 🟢🔵🔶🟥⤴️🕒 |
           !|---------|--------------------|
           !| Cuisine | Make bread         |
           !""".stripMargin('!')
    }

    it("should update a task note in an existing document") {
      val updated = GettingThingsDone(withWeeklyToDo).updateTopWeekToDo(
        0,
        notes = Some("Make muffins")
      )
      updated.topWeek.value.mds.headOption.value.build().toString() shouldBe
        s"""| To Do  | Notes 🟢🔵🔶🟥⤴️🕒 |
           !|--------|--------------------|
           !| Baking | Make muffins       |
           !""".stripMargin('!')
    }

    it("should update a task state in an existing document") {
      val updated = GettingThingsDone(withWeeklyToDo).updateTopWeekToDo(
        0,
        state = Some(DoneToDo)
      )
      updated.topWeek.value.mds.headOption.value.build().toString() shouldBe
        s"""| To Do    | Notes 🟢🔵🔶🟥⤴️🕒 |
           !|----------|--------------------|
           !| 🟢Baking | Make bread         |
           !""".stripMargin('!')

      // It should remove the task state too
      updated.updateTopWeekToDo(
        0,
        state = Some(NoToDoState)
      ) shouldBe GettingThingsDone(withWeeklyToDo)
    }
  }

  describe("Updating a new week") {

    it("should create a new week in an empty document") {
      val empty = GettingThingsDone("")
      val updated = empty.addWeek(None)
      updated.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !$defaultNextWeekStart
           !------------------------------------------------------------------------------
           !""".stripMargin('!')
    }

    it("should roll over and clean the stats") {
      val gtd = GettingThingsDone(s"""Weekly Status
           !==============================================================================
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
           !|--------|-----|-----|-----|-----|-----|-----|-----|
           !| read   |     | 12  | 13  |     | 20  |     | 30  |
           !| unread | 1   | 2   |     | 13  | 2   |     | 3   |
           !""".stripMargin('!'))
      val updated = gtd.addWeek(None)
      updated.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !2022-02-21
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
           !|--------|-----|-----|-----|-----|-----|-----|-----|
           !| read   |     |     |     |     |     |     |     |
           !| unread |     |     |     |     |     |     |     |
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !| Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
           !|--------|-----|-----|-----|-----|-----|-----|-----|
           !| read   |     | 12  | 13  |     | 20  |     | 30  |
           !| unread | 1   | 2   |     | 13  | 2   |     | 3   |
           !""".stripMargin('!')
    }

    it("should roll over and update todo tasks") {
      val gtd = GettingThingsDone(s"""Weekly Status
           !==============================================================================
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !| To Do  | Notes |
           !|--------|-------------------|
           !| ${DoneToDo.txt}A  | 1 |
           !| ${DoneSimpleToDo.txt}B  | 2 |
           !| ${MaybeToDo.txt}C  | 3 |
           !| ${StoppedToDo.txt}D  | 4 |
           !| ${LaterToDo.txt}E  | 5 |
           !| ${WaitingToDo.txt}F  | 6 |
           !| G | 7 |
           !""".stripMargin('!'))

      val updated = gtd.addWeek(None)
      updated.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !2022-02-21
           !------------------------------------------------------------------------------
           !
           !| To Do | Notes |
           !|-------|-------|
           !| 🔶C   | 3     |
           !| ⤴️E   | 5     |
           !| 🕒F   | 6     |
           !| G     | 7     |
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !| To Do | Notes |
           !|-------|-------|
           !| 🟢A   | 1     |
           !| 🔵B   | 2     |
           !| ⤴️C   | 3     |
           !| 🟥D   | 4     |
           !| ⤴️E   | 5     |
           !| 🕒F   | 6     |
           !| G     | 7     |
           !""".stripMargin('!')
    }

    it("should roll over all text without modification") {
      val gtd = GettingThingsDone(s"""Weekly Status
           !==============================================================================
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !* See this [example][20220214-1].
           !
           ![20220214-1]: https://example.com/
           !""".stripMargin('!'))

      val updated = gtd.addWeek(None)
      updated.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !2022-02-21
           !------------------------------------------------------------------------------
           !
           !* See this [example][20220214-1].
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !* See this [example][20220214-1].
           !
           ![20220214-1]: https://example.com/
           !""".stripMargin('!')
    }

    it("should roll over all text to a requested date") {
      val gtd = GettingThingsDone(s"""Weekly Status
           !==============================================================================
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !* My status
           !""".stripMargin('!'))

      // These should not change the document at all
      gtd.addWeek(Some("000")) shouldBe gtd
      gtd.addWeek(Some("2022-02-12")) shouldBe gtd
      gtd.addWeek(Some("2022-02-12 Extra")) shouldBe gtd
      gtd.addWeek(Some("2022-02-14")) shouldBe gtd
      gtd.addWeek(Some("2022-02-14 More")) shouldBe gtd
      gtd.addWeek(Some("2022-02-15")) shouldBe gtd
      gtd.addWeek(Some("2022-02-20")) shouldBe gtd

      val updated = gtd.addWeek(Some("2022-02-21"))
      updated.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !2022-02-21
           !------------------------------------------------------------------------------
           !
           !* My status
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !* My status
           !""".stripMargin('!')

      gtd.addWeek(Some("2022-02-21")) shouldBe updated
      gtd.addWeek(Some("2022-02-21 Stuff")) shouldBe updated
      gtd.addWeek(Some("2022-02-22")) shouldBe updated
      gtd.addWeek(Some("2022-02-27 Stuff")) shouldBe updated

      val updated2 = gtd.addWeek(Some("2022-04"))
      updated2.h0.build().toString() shouldBe
        s"""Weekly Status
           !==============================================================================
           !
           !2022-03-28
           !------------------------------------------------------------------------------
           !
           !* My status
           !
           !2022-03-21
           !------------------------------------------------------------------------------
           !
           !* My status
           !
           !2022-03-14
           !------------------------------------------------------------------------------
           !
           !* My status
           !
           !2022-03-07
           !------------------------------------------------------------------------------
           !
           !* My status
           !
           !2022-02-28
           !------------------------------------------------------------------------------
           !
           !* My status
           !
           !2022-02-21
           !------------------------------------------------------------------------------
           !
           !* My status
           !
           !2022-02-14
           !------------------------------------------------------------------------------
           !
           !* My status
           !""".stripMargin('!')
    }
  }

  describe("Extracting statistics") {

    def stringify(
        in: Seq[(LocalDate, String, String)]
    ): Seq[(String, String, String)] =
      in.map { case (d, stat, value) => (d.format(Pattern), stat, value) }

    val withWeeklyStats = GettingThingsDone(s"""Weekly Status
         !==============================================================================
         !
         !2022-02-28
         !------------------------------------------------------------------------------
         !
         !| Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
         !|--------|-----|-----|-----|-----|-----|-----|-----|
         !| read   | 30  | 12  | 13  |     | 20  |     | 30  |
         !| unread | 52  | 51  | 50  | 58  | 46  | 57  | 38  |
         !
         !2022-02-21
         !------------------------------------------------------------------------------
         !
         !| Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
         !|--------|-----|-----|-----|-----|-----|-----|-----|
         !| unread | 14  | 24  | 31  |     | 59  | 60  | 72  |
         !
         !2022-02-14
         !------------------------------------------------------------------------------
         !
         !| Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
         !|--------|-----|-----|-----|-----|-----|-----|-----|
         !| read   |     | 12  | 13  |     | 20  |     | 30  |
         !| unread | 1   |  2  |     | 13  | 2   |     | 3   |
         !""".stripMargin('!'))

    it("should ignore when no statistic is found") {
      withWeeklyStats.extractStats("none") shouldBe empty
    }

    it("should find all the read and unread statistics") {
      stringify(withWeeklyStats.extractStats()) shouldBe Seq(
        ("2022-02-14", "unread", "1"),
        ("2022-02-15", "read", "12"),
        ("2022-02-15", "unread", "2"),
        ("2022-02-16", "read", "13"),
        ("2022-02-17", "unread", "13"),
        ("2022-02-18", "read", "20"),
        ("2022-02-18", "unread", "2"),
        ("2022-02-20", "read", "30"),
        ("2022-02-20", "unread", "3"),
        ("2022-02-21", "unread", "14"),
        ("2022-02-22", "unread", "24"),
        ("2022-02-23", "unread", "31"),
        ("2022-02-25", "unread", "59"),
        ("2022-02-26", "unread", "60"),
        ("2022-02-27", "unread", "72"),
        ("2022-02-28", "read", "30"),
        ("2022-02-28", "unread", "52"),
        ("2022-03-01", "read", "12"),
        ("2022-03-01", "unread", "51"),
        ("2022-03-02", "read", "13"),
        ("2022-03-02", "unread", "50"),
        ("2022-03-03", "unread", "58"),
        ("2022-03-04", "read", "20"),
        ("2022-03-04", "unread", "46"),
        ("2022-03-05", "unread", "57"),
        ("2022-03-06", "read", "30"),
        ("2022-03-06", "unread", "38")
      )
      stringify(withWeeklyStats.extractStats("unread")) shouldBe Seq(
        ("2022-02-14", "unread", "1"),
        ("2022-02-15", "unread", "2"),
        ("2022-02-17", "unread", "13"),
        ("2022-02-18", "unread", "2"),
        ("2022-02-20", "unread", "3"),
        ("2022-02-21", "unread", "14"),
        ("2022-02-22", "unread", "24"),
        ("2022-02-23", "unread", "31"),
        ("2022-02-25", "unread", "59"),
        ("2022-02-26", "unread", "60"),
        ("2022-02-27", "unread", "72"),
        ("2022-02-28", "unread", "52"),
        ("2022-03-01", "unread", "51"),
        ("2022-03-02", "unread", "50"),
        ("2022-03-03", "unread", "58"),
        ("2022-03-04", "unread", "46"),
        ("2022-03-05", "unread", "57"),
        ("2022-03-06", "unread", "38")
      )
      stringify(withWeeklyStats.extractStats("read")) shouldBe Seq(
        ("2022-02-15", "read", "12"),
        ("2022-02-16", "read", "13"),
        ("2022-02-18", "read", "20"),
        ("2022-02-20", "read", "30"),
        ("2022-02-28", "read", "30"),
        ("2022-03-01", "read", "12"),
        ("2022-03-02", "read", "13"),
        ("2022-03-04", "read", "20"),
        ("2022-03-06", "read", "30")
      )
    }

    describe("should find all the read statistics") {

      it("after a date") {
        stringify(
          withWeeklyStats
            .extractStats(
              "read",
              from = Some(LocalDate.parse("2022-02-18", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-18", "read", "20"),
          ("2022-02-20", "read", "30"),
          ("2022-02-28", "read", "30"),
          ("2022-03-01", "read", "12"),
          ("2022-03-02", "read", "13"),
          ("2022-03-04", "read", "20"),
          ("2022-03-06", "read", "30")
        )
      }

      it("before a date") {
        stringify(
          withWeeklyStats
            .extractStats(
              "read",
              to = Some(LocalDate.parse("2022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-15", "read", "12"),
          ("2022-02-16", "read", "13"),
          ("2022-02-18", "read", "20"),
          ("2022-02-20", "read", "30"),
          ("2022-02-28", "read", "30"),
          ("2022-03-01", "read", "12"),
          ("2022-03-02", "read", "13")
        )
      }

      it("between dates") {
        stringify(
          withWeeklyStats
            .extractStats(
              "read",
              from = Some(LocalDate.parse("2022-02-18", Pattern)),
              to = Some(LocalDate.parse("2022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-18", "read", "20"),
          ("2022-02-20", "read", "30"),
          ("2022-02-28", "read", "30"),
          ("2022-03-01", "read", "12"),
          ("2022-03-02", "read", "13")
        )
      }

      it("between large bounds") {
        stringify(
          withWeeklyStats
            .extractStats(
              "read",
              from = Some(LocalDate.parse("1022-02-18", Pattern)),
              to = Some(LocalDate.parse("3022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-15", "read", "12"),
          ("2022-02-16", "read", "13"),
          ("2022-02-18", "read", "20"),
          ("2022-02-20", "read", "30"),
          ("2022-02-28", "read", "30"),
          ("2022-03-01", "read", "12"),
          ("2022-03-02", "read", "13"),
          ("2022-03-04", "read", "20"),
          ("2022-03-06", "read", "30")
        )
      }
    }

    describe("should find all the read and unread statistics") {

      it("after a date") {
        stringify(
          withWeeklyStats
            .extractStats(
              from = Some(LocalDate.parse("2022-02-18", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-18", "read", "20"),
          ("2022-02-18", "unread", "2"),
          ("2022-02-20", "read", "30"),
          ("2022-02-20", "unread", "3"),
          ("2022-02-21", "unread", "14"),
          ("2022-02-22", "unread", "24"),
          ("2022-02-23", "unread", "31"),
          ("2022-02-25", "unread", "59"),
          ("2022-02-26", "unread", "60"),
          ("2022-02-27", "unread", "72"),
          ("2022-02-28", "read", "30"),
          ("2022-02-28", "unread", "52"),
          ("2022-03-01", "read", "12"),
          ("2022-03-01", "unread", "51"),
          ("2022-03-02", "read", "13"),
          ("2022-03-02", "unread", "50"),
          ("2022-03-03", "unread", "58"),
          ("2022-03-04", "read", "20"),
          ("2022-03-04", "unread", "46"),
          ("2022-03-05", "unread", "57"),
          ("2022-03-06", "read", "30"),
          ("2022-03-06", "unread", "38")
        )
      }

      it("before a date") {
        stringify(
          withWeeklyStats
            .extractStats(
              to = Some(LocalDate.parse("2022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-14", "unread", "1"),
          ("2022-02-15", "read", "12"),
          ("2022-02-15", "unread", "2"),
          ("2022-02-16", "read", "13"),
          ("2022-02-17", "unread", "13"),
          ("2022-02-18", "read", "20"),
          ("2022-02-18", "unread", "2"),
          ("2022-02-20", "read", "30"),
          ("2022-02-20", "unread", "3"),
          ("2022-02-21", "unread", "14"),
          ("2022-02-22", "unread", "24"),
          ("2022-02-23", "unread", "31"),
          ("2022-02-25", "unread", "59"),
          ("2022-02-26", "unread", "60"),
          ("2022-02-27", "unread", "72"),
          ("2022-02-28", "read", "30"),
          ("2022-02-28", "unread", "52"),
          ("2022-03-01", "read", "12"),
          ("2022-03-01", "unread", "51"),
          ("2022-03-02", "read", "13"),
          ("2022-03-02", "unread", "50")
        )
      }

      it("between dates") {
        stringify(
          withWeeklyStats
            .extractStats(
              from = Some(LocalDate.parse("2022-02-18", Pattern)),
              to = Some(LocalDate.parse("2022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-18", "read", "20"),
          ("2022-02-18", "unread", "2"),
          ("2022-02-20", "read", "30"),
          ("2022-02-20", "unread", "3"),
          ("2022-02-21", "unread", "14"),
          ("2022-02-22", "unread", "24"),
          ("2022-02-23", "unread", "31"),
          ("2022-02-25", "unread", "59"),
          ("2022-02-26", "unread", "60"),
          ("2022-02-27", "unread", "72"),
          ("2022-02-28", "read", "30"),
          ("2022-02-28", "unread", "52"),
          ("2022-03-01", "read", "12"),
          ("2022-03-01", "unread", "51"),
          ("2022-03-02", "read", "13"),
          ("2022-03-02", "unread", "50")
        )
      }

      it("between large bounds") {
        stringify(
          withWeeklyStats
            .extractStats(
              from = Some(LocalDate.parse("1022-02-18", Pattern)),
              to = Some(LocalDate.parse("3022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-14", "unread", "1"),
          ("2022-02-15", "read", "12"),
          ("2022-02-15", "unread", "2"),
          ("2022-02-16", "read", "13"),
          ("2022-02-17", "unread", "13"),
          ("2022-02-18", "read", "20"),
          ("2022-02-18", "unread", "2"),
          ("2022-02-20", "read", "30"),
          ("2022-02-20", "unread", "3"),
          ("2022-02-21", "unread", "14"),
          ("2022-02-22", "unread", "24"),
          ("2022-02-23", "unread", "31"),
          ("2022-02-25", "unread", "59"),
          ("2022-02-26", "unread", "60"),
          ("2022-02-27", "unread", "72"),
          ("2022-02-28", "read", "30"),
          ("2022-02-28", "unread", "52"),
          ("2022-03-01", "read", "12"),
          ("2022-03-01", "unread", "51"),
          ("2022-03-02", "read", "13"),
          ("2022-03-02", "unread", "50"),
          ("2022-03-03", "unread", "58"),
          ("2022-03-04", "read", "20"),
          ("2022-03-04", "unread", "46"),
          ("2022-03-05", "unread", "57"),
          ("2022-03-06", "read", "30"),
          ("2022-03-06", "unread", "38")
        )
      }
    }
  }

  describe("Extracting to do tasks") {

    def stringify(
        in: Seq[(LocalDate, ToDoState, String, String)]
    ): Seq[(String, ToDoState, String, String)] =
      in.map { case (d, state, category, notes) =>
        (d.format(Pattern), state, category, notes)
      }

    val withWeeklyToDo = GettingThingsDone(s"""Weekly Status
         !==============================================================================
         !
         ! <!-- 🟢🔵🔶🟥⤴️🕒 -->
         !
         !2022-02-28
         !------------------------------------------------------------------------------
         !
         !| To Do   | Notes |
         !|---------|-------|
         !| 🟥A     | 1.2   |
         !| 🟢B Fri | 2.2   |
         !
         !2022-02-21
         !------------------------------------------------------------------------------
         !
         !| To Do | Notes |
         !|-------|-------|
         !| 🟢A   | 1.1   |
         !| 🟥B   | 2.1   |
         !
         !2022-02-14
         !------------------------------------------------------------------------------
         !
         !| To Do   | Notes |
         !|---------|-------|
         !| 🟢A Mon | 1   |
         !| 🔵B Tue | 2   |
         !| ⤴️C Wed | 3   |
         !| 🟥D Thu | 4   |
         !| ⤴️E Fri | 5   |
         !| 🕒F Sat | 6   |
         !| G Sun   | 7   |
         !""".stripMargin('!'))

    it("should ignore when no statistic is found") {
      GettingThingsDone().extractToDo() shouldBe empty
    }

    it("should ignore empty lines") {
      stringify(GettingThingsDone(s"""Weekly Status
         !==============================================================================
         !
         !2022-02-14
         !------------------------------------------------------------------------------
         !
         !| To Do   | Notes |
         !|---------|-------|
         !|         |       |
         !| A       | B     |
         !|         |       |
         !| C       |       |
         !|         |       |
         !|         | D     |
         !|         |       |
         !""".stripMargin('!')).extractToDo()) shouldBe Seq(
        ("2022-02-14", NoToDoState, "A", "B"),
        ("2022-02-14", NoToDoState, "C", ""),
        ("2022-02-14", NoToDoState, "", "D")
      )
    }

    it("should find all the tasks") {
      stringify(withWeeklyToDo.extractToDo()) shouldBe Seq(
        ("2022-02-14", DoneToDo, "A", "1"),
        ("2022-02-15", DoneSimpleToDo, "B", "2"),
        ("2022-02-16", LaterToDo, "C", "3"),
        ("2022-02-17", StoppedToDo, "D", "4"),
        ("2022-02-18", LaterToDo, "E", "5"),
        ("2022-02-19", WaitingToDo, "F", "6"),
        ("2022-02-20", NoToDoState, "G", "7"),
        ("2022-02-21", DoneToDo, "A", "1.1"),
        ("2022-02-21", StoppedToDo, "B", "2.1"),
        ("2022-02-28", StoppedToDo, "A", "1.2"),
        ("2022-03-04", DoneToDo, "B", "2.2")
      )
    }

    it("should find only completed tasks") {
      stringify(
        withWeeklyToDo.extractToDo(completed = Some(true))
      ) shouldBe Seq(
        ("2022-02-14", DoneToDo, "A", "1"),
        ("2022-02-15", DoneSimpleToDo, "B", "2"),
        ("2022-02-17", StoppedToDo, "D", "4"),
        ("2022-02-21", DoneToDo, "A", "1.1"),
        ("2022-02-21", StoppedToDo, "B", "2.1"),
        ("2022-02-28", StoppedToDo, "A", "1.2"),
        ("2022-03-04", DoneToDo, "B", "2.2")
      )
    }

    describe("should find all the tasks") {

      it("after a date") {
        stringify(
          withWeeklyToDo
            .extractToDo(
              from = Some(LocalDate.parse("2022-02-18", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-18", LaterToDo, "E", "5"),
          ("2022-02-19", WaitingToDo, "F", "6"),
          ("2022-02-20", NoToDoState, "G", "7"),
          ("2022-02-21", DoneToDo, "A", "1.1"),
          ("2022-02-21", StoppedToDo, "B", "2.1"),
          ("2022-02-28", StoppedToDo, "A", "1.2"),
          ("2022-03-04", DoneToDo, "B", "2.2")
        )
      }

      it("before a date") {
        stringify(
          withWeeklyToDo
            .extractToDo(
              to = Some(LocalDate.parse("2022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-14", DoneToDo, "A", "1"),
          ("2022-02-15", DoneSimpleToDo, "B", "2"),
          ("2022-02-16", LaterToDo, "C", "3"),
          ("2022-02-17", StoppedToDo, "D", "4"),
          ("2022-02-18", LaterToDo, "E", "5"),
          ("2022-02-19", WaitingToDo, "F", "6"),
          ("2022-02-20", NoToDoState, "G", "7"),
          ("2022-02-21", DoneToDo, "A", "1.1"),
          ("2022-02-21", StoppedToDo, "B", "2.1"),
          ("2022-02-28", StoppedToDo, "A", "1.2")
        )
      }

      it("between dates") {
        stringify(
          withWeeklyToDo
            .extractToDo(
              from = Some(LocalDate.parse("2022-02-18", Pattern)),
              to = Some(LocalDate.parse("2022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-18", LaterToDo, "E", "5"),
          ("2022-02-19", WaitingToDo, "F", "6"),
          ("2022-02-20", NoToDoState, "G", "7"),
          ("2022-02-21", DoneToDo, "A", "1.1"),
          ("2022-02-21", StoppedToDo, "B", "2.1"),
          ("2022-02-28", StoppedToDo, "A", "1.2")
        )
      }

      it("between large bounds") {
        stringify(
          withWeeklyToDo
            .extractToDo(
              from = Some(LocalDate.parse("1022-02-18", Pattern)),
              to = Some(LocalDate.parse("3022-03-02", Pattern))
            )
        ) shouldBe Seq(
          ("2022-02-14", DoneToDo, "A", "1"),
          ("2022-02-15", DoneSimpleToDo, "B", "2"),
          ("2022-02-16", LaterToDo, "C", "3"),
          ("2022-02-17", StoppedToDo, "D", "4"),
          ("2022-02-18", LaterToDo, "E", "5"),
          ("2022-02-19", WaitingToDo, "F", "6"),
          ("2022-02-20", NoToDoState, "G", "7"),
          ("2022-02-21", DoneToDo, "A", "1.1"),
          ("2022-02-21", StoppedToDo, "B", "2.1"),
          ("2022-02-28", StoppedToDo, "A", "1.2"),
          ("2022-03-04", DoneToDo, "B", "2.2")
        )
      }
    }
  }

  describe("The nextWeekStart(...) utility for calculating a new week") {

    it("should ignore suffixes") {
      nextWeekStart(
        Some("2021-09-05 Extra")
      ) shouldBe "2021-09-06"
    }

    it(s"should calculate from today if None") {
      val today =
        Some(LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy-MM-dd")))
      nextWeekStart(None) shouldBe nextWeekStart(today)
    }

    it(s"should allow the week start to be selectable") {
      nextWeekStart(
        Some("2021-09-01"),
        dow = DayOfWeek.SUNDAY
      ) shouldBe "2021-09-05"
      nextWeekStart(
        Some("2021-09-01"),
        dow = DayOfWeek.MONDAY
      ) shouldBe "2021-09-06"
      nextWeekStart(
        Some("2021-09-01"),
        dow = DayOfWeek.TUESDAY
      ) shouldBe "2021-09-07"
      nextWeekStart(
        Some("2021-09-01"),
        dow = DayOfWeek.WEDNESDAY
      ) shouldBe "2021-09-08"
      nextWeekStart(
        Some("2021-09-01"),
        dow = DayOfWeek.THURSDAY
      ) shouldBe "2021-09-02"
      nextWeekStart(
        Some("2021-09-01"),
        dow = DayOfWeek.FRIDAY
      ) shouldBe "2021-09-03"
      nextWeekStart(
        Some("2021-09-01"),
        dow = DayOfWeek.SATURDAY
      ) shouldBe "2021-09-04"
    }

    it(s"should work with epoch days as well") {
      nextWeekStartByEpoch(
        Some(18871L),
        dow = DayOfWeek.SUNDAY
      ) shouldBe 18875L
      nextWeekStartByEpoch(
        Some(18871L),
        dow = DayOfWeek.MONDAY
      ) shouldBe 18876L
      nextWeekStartByEpoch(
        Some(18871L),
        dow = DayOfWeek.TUESDAY
      ) shouldBe 18877L
      nextWeekStartByEpoch(
        Some(18871L),
        dow = DayOfWeek.WEDNESDAY
      ) shouldBe 18878L
      nextWeekStartByEpoch(
        Some(18871L),
        dow = DayOfWeek.THURSDAY
      ) shouldBe 18872L
      nextWeekStartByEpoch(
        Some(18871L),
        dow = DayOfWeek.FRIDAY
      ) shouldBe 18873L
      nextWeekStartByEpoch(
        Some(18871L),
        dow = DayOfWeek.SATURDAY
      ) shouldBe 18874L
    }

    for (
      date <- Seq(
        "2021-08-30",
        "2021-08-31",
        "2021-09-01",
        "2021-09-02",
        "2021-09-03",
        "2021-09-04",
        "2021-09-05"
      )
    ) {
      it(s"should return next 2021-09-07 from a $date") {
        nextWeekStart(Some(date)) shouldBe "2021-09-06"
      }
    }
  }

}
