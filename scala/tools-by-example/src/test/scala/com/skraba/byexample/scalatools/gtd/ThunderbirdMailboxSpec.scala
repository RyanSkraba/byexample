package com.skraba.byexample.scalatools.gtd

import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.WithTmpDir
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

/** Unit tests for [[ThunderbirdMailbox]]. */
class ThunderbirdMailboxSpec extends AnyFunSpecLike with Matchers with WithTmpDir {

  (Tmp / ".thunderbird" / "profile1" / "a1" / "b1/").createDirectory(failIfExists = false)
  (Tmp / ".thunderbird" / "profile2" / "a1" / "b1/").createDirectory(failIfExists = false)
  (Tmp / ".thunderbird" / "profile2" / "a1" / "c1/").createDirectory(failIfExists = false)

  (Tmp / ".thunderbird" / "profile1" / "a1" / "b1/" / "Unique").writeAll("""
      |ABC123=numNewMsgs
      |(^ABC123=46E)
      |""".stripMargin)

  (Tmp / ".thunderbird" / "profile1" / "a1" / "b1/" / "Duplicate").writeAll("""
      |Tests a tag with only numbers, plus multiple values to only find the last one
      |00=numNewMsgs
      |(^00=000)
      |(^00=010)
      |ignored
      |(^ABC123=46E)
      |(^00=100)
      |(^00=001)
      |""".stripMargin)

  (Tmp / ".thunderbird" / "profile2" / "a1" / "b1/" / "Duplicate").writeAll("""
      |ignoreA001=numNewMsgs
      |ignore
      |(^A001=46e)
      |ignore
      |""".stripMargin)

  (Tmp / ".thunderbird" / "profile2" / "a1" / "c1/" / "Duplicate").writeAll("""
      |Also the order doesn't seem to matter
      |(^XXX=1000) XXX=numNewMsgs
      |""".stripMargin)

  describe(s"Getting messages from a Thunderbird mailbox") {

    it("should fail when it can't find a mailbox") {
      intercept[RuntimeException] {
        ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Nox")
      } should have message s"'Nox' can't be found in $Tmp/.thunderbird"
    }

    // None of these refiners are sufficient to find the mailbox file
    for (refiner <- Seq("", "b", "/a1/", "/.thunder", "ofi", "1/Duplic"))
      it(s"should fail when it finds duplicate mailboxes with refiner: '$refiner'") {
        intercept[RuntimeException] {
          ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", refiner)
        } should have message
          s"""More than one 'Duplicate' found in $Tmp/.thunderbird:
           |  $Tmp/.thunderbird/profile1/a1/b1/Duplicate
           |  $Tmp/.thunderbird/profile2/a1/b1/Duplicate
           |  $Tmp/.thunderbird/profile2/a1/c1/Duplicate
           |""".stripMargin.trim
      }

    for (refiner <- Seq("b1", "/b1/", "1/b", "a1/b1"))
      it(s"should fail when it finds duplicate mailboxes with refiner: '$refiner'") {
        intercept[RuntimeException] {
          ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", refiner)
        } should have message
          s"""More than one 'Duplicate' found in $Tmp/.thunderbird:
           |  $Tmp/.thunderbird/profile1/a1/b1/Duplicate
           |  $Tmp/.thunderbird/profile2/a1/b1/Duplicate
           |""".stripMargin.trim
      }

    // None of these refiners help find the Duplicate mailbox
    for (refiner <- Seq("a1/b2", "xxx"))
      it(s"should fail when it can't find a mailbox with refiner: '$refiner'") {
        intercept[RuntimeException] {
          ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", refiner)
        } should have message
          s"'Duplicate' can't be found in $Tmp/.thunderbird"
      }

    it("should find the 'Unique' mailbox without a refiner") {
      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Unique") shouldBe 1134
    }

    it("should find the 'Duplicate' mailbox with a refiner") {
      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", "profile1") shouldBe 1
      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", "profile1/a1") shouldBe 1

      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(
        Tmp / ".thunderbird",
        "Duplicate",
        "profile2/a1/b1"
      ) shouldBe 1134
      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", "2/a1/b") shouldBe 1134

      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", "/c") shouldBe 4096
      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", "1/c") shouldBe 4096
      ThunderbirdMailbox.getNumberOfMessagesFromMailbox(Tmp / ".thunderbird", "Duplicate", "c1/D") shouldBe 4096
    }
  }
}
