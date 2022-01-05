package com.skraba.byexample.scala.markd

import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, Instant, LocalDate}
import scala.reflect.internal.util.ChromeTrace.EventType

/** Unit tests for [[GettingThingsDone]]
  */
class GettingThingsDoneSpec extends AnyFunSpecLike with Matchers {

  describe("Updating the weekly status") {}

  describe("Utility for calculating a new week") {

    it(s"should ignore suffixes") {
      GettingThingsDone.nextWeekStart(
        Some("2021/09/05 Extra")
      ) shouldBe "2021/09/06"
    }

    it(s"should calculate from today if None") {
      val today =
        Some(LocalDate.now().format(DateTimeFormatter.ofPattern("yyyy/MM/dd")))
      GettingThingsDone.nextWeekStart(None) shouldBe GettingThingsDone
        .nextWeekStart(today)
    }

    it(s"should allow the week start to be selectable") {
      GettingThingsDone.nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.SUNDAY
      ) shouldBe "2021/09/05"
      GettingThingsDone.nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.MONDAY
      ) shouldBe "2021/09/06"
      GettingThingsDone.nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.TUESDAY
      ) shouldBe "2021/09/07"
      GettingThingsDone.nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.WEDNESDAY
      ) shouldBe "2021/09/08"
      GettingThingsDone.nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.THURSDAY
      ) shouldBe "2021/09/02"
      GettingThingsDone.nextWeekStart(
        Some("2021/09/01"),
        dow = DayOfWeek.FRIDAY
      ) shouldBe "2021/09/03"
      GettingThingsDone.nextWeekStart(
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
        GettingThingsDone.nextWeekStart(Some(date)) shouldBe "2021/09/06"
      }
    }
  }

}
