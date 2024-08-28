package com.skraba.byexample.scala.ammonite.validator

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.Directory

/** Test the [[Validator]]. */
class ValidatorSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** A temporary directory for playing with files. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** And delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try {
      Tmp.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

}
