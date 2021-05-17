package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour080PatternMatchingSpec extends AnyFunSpecLike with Matchers {

  describe("Pattern Matching") {

    it("is simple") {
      val x: Int = 0
      val y = x match {
        case 0 => "zero"
        case 1 => "one"
        case 2 => "two"
        case _ => "many"
      }
      y shouldBe "zero"

      def matchTest(x: Int): String =
        x match {
          case 1 => "one"
          case 2 => "two"
          case _ => "other"
        }

      matchTest(3) shouldBe "other"
      matchTest(1) shouldBe "one"
      matchTest _ shouldBe a[_ => _]
    }

    describe("works well with case classes") {

      abstract class Notification
      case class Email(sender: String, title: String, body: String)
          extends Notification
      case class SMS(caller: String, message: String) extends Notification
      case class VoiceRecording(contactName: String, link: String)
          extends Notification

      def showNotification(notification: Notification): String = {
        notification match {
          case Email(sender, title, _) =>
            s"You got an email from $sender with title: $title"
          case SMS(number, message) =>
            s"You got an SMS from $number! Message: $message"
          case VoiceRecording(name, link) =>
            s"You received a Voice Recording from $name! Click the link to hear it: $link"
        }
      }

      it("should match the simple notifications") {
        val someSms = SMS("12345", "Are you there?")
        val someVoiceRecording =
          VoiceRecording("Tom", "voicerecording.org/id/123")
        showNotification(
          someSms
        ) shouldBe "You got an SMS from 12345! Message: Are you there?"
        showNotification(
          someVoiceRecording
        ) shouldBe "You received a Voice Recording from Tom! Click the link to hear it: voicerecording.org/id/123"
      }

      it("should match with pattern guards") {
        def showImportantNotification(
            notification: Notification,
            importantPeopleInfo: Seq[String]
        ): String = {
          notification match {
            case Email(sender, _, _) if importantPeopleInfo.contains(sender) =>
              "You got an email from special someone!"
            case SMS(number, _) if importantPeopleInfo.contains(number) =>
              "You got an SMS from special someone!"
            case other =>
              showNotification(
                other
              ) // nothing special, delegate to our original showNotification function
          }
        }

        val importantPeopleInfo = Seq("867-5309", "jenny@gmail.com")

        val someSms = SMS("867-5309", "Are you there?")
        val someVoiceRecording =
          VoiceRecording("Tom", "voicerecording.org/id/123")
        val importantEmail =
          Email("jenny@gmail.com", "Drinks tonight?", "I'm free after 5!")
        val importantSms = SMS("867-5309", "I'm here! Where are you?")

        showImportantNotification(
          someSms,
          importantPeopleInfo
        ) shouldBe "You got an SMS from special someone!"
        showImportantNotification(
          someVoiceRecording,
          importantPeopleInfo
        ) shouldBe "You received a Voice Recording from Tom! Click the link to hear it: voicerecording.org/id/123"
        showImportantNotification(
          importantEmail,
          importantPeopleInfo
        ) shouldBe "You got an email from special someone!"
        showImportantNotification(
          importantSms,
          importantPeopleInfo
        ) shouldBe "You got an SMS from special someone!"
      }
    }

    it("can match on types only") {
      abstract class Device
      case class Phone(model: String) extends Device {
        def screenOff = "Turning screen off"
      }
      case class Computer(model: String) extends Device {
        def screenSaverOn = "Turning screen saver on..."
      }
      case class HotWaterHeater(model: String) extends Device {
        def vacationModeOn = "Lowering temperature"
      }

      def goIdle(device: Device) =
        device match {
          case p: Phone    => p.screenOff
          case c: Computer => c.screenSaverOn
        }

      goIdle(Phone("Nexus5")) shouldBe "Turning screen off"
      goIdle(Computer("Dell 1234")) shouldBe "Turning screen saver on..."

      intercept[MatchError] {
        // This throws the unmatched error.
        goIdle(HotWaterHeater("ABC123")) shouldBe "Turning energy saver on..."
      }
    }

    it("can match on sealed classes") {
      sealed abstract class Furniture
      case class Couch() extends Furniture
      case class Chair() extends Furniture

      def findPlaceToSit(piece: Furniture): String =
        piece match {
          case a: Couch => "Lie on the couch"
          case b: Chair => "Sit on the chair"
        }

      findPlaceToSit(Couch()) shouldBe "Lie on the couch"
      findPlaceToSit(Chair()) shouldBe "Sit on the chair"
    }
  }
}
