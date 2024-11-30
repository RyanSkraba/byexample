package com.skraba.byexample.scala.hack

import org.scalatest.Assertions

import java.nio.charset.StandardCharsets
import java.security.Key
import java.util.Base64
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, KeyGenerator}
import scala.io.Source
import scala.reflect.io.{File, Path}

/** Helpful utilities for https://adventofcode.com/
  *
  * Specifically, we have been asked to not submit input to repos, so this provides a way to encrypt / decrypt inputs
  * and answers using an environment variable containing a AES 256 key.
  *
  * The solutions tests should be ignored if the key isn't present.
  */
class AdventUtils {

  /** The environment variable that contains the AES-256 key. */
  val AdventOfCodeKey = "ADVENT_OF_CODE_KEY"
  val AdventOfCodeEncrypted =
    s"!! Set $AdventOfCodeKey to decrypt (https://adventofcode.com/about)"

  /** @return
    *   the [[Key]] being used to decrypt inputs and solutions.
    */
  def getOrCreateKey(): Key = {
    val keyEnv = sys.env.get(AdventOfCodeKey).filter(_.nonEmpty).getOrElse {
      val gen = KeyGenerator.getInstance("AES")
      gen.init(256)
      val key = gen.generateKey
      val encoded = Base64.getEncoder.encodeToString(key.getEncoded)
      println(s"Using temporary key: $encoded")
      encoded
    }
    new SecretKeySpec(Base64.getDecoder.decode(keyEnv), "AES")
  }

  /** Causes a test to be canceled (but not failed) if the key isn't present. */
  def requireAdventOfCodeKey(): Unit = {
    val filteredEnv = sys.env.filter(_._1 == AdventOfCodeKey)
    Assertions.assume(
      filteredEnv.contains(AdventOfCodeKey),
      "(missing key for running solutions)"
    )
    Assertions.assume(
      filteredEnv.get(AdventOfCodeKey).exists(_.nonEmpty),
      "(\"\" key for running solutions)"
    )
  }

  /** A helper method for testing and getting the encrypted answer. This can be used temporarily but shouldn't be
    * committed to a repo.
    */
  def decryptLongDoNotSubmit(in: Long): Long = {
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, getOrCreateKey())
    val encrypted =
      Base64.getEncoder.encodeToString(cipher.doFinal(in.toString.getBytes))
    println(s"Encrypted $in: decryptLong(\"$encrypted\")")
    in
  }

  /** A helper method for testing and getting the encrypted answer. This can be used temporarily but shouldn't be
    * committed to a repo.
    */
  def decryptDoNotSubmit(in: String): String = {
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, getOrCreateKey())
    val encrypted =
      Base64.getEncoder.encodeToString(cipher.doFinal(in.getBytes))
    println(s"Encrypted $in: decrypt(\"$encrypted\")")
    in
  }

  /** @return the long value encrypted in the string */
  def decryptLong(in: String): Long = {
    decrypt(in).toLong
  }

  /** @return the long value encrypted in the string */
  def decrypt(in: String): String = {
    requireAdventOfCodeKey()
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.DECRYPT_MODE, getOrCreateKey())
    new String(
      cipher.doFinal(Base64.getDecoder.decode(in)),
      StandardCharsets.UTF_8
    )
  }

  /** Get the puzzle input corresponding to the filename, in the right Advent of Code directory.
    *
    * If the file isn't encrypted but the [[AdventOfCodeKey]] is set in the environment, overwrite it with an encrypted
    * text.
    */
  def puzzleInput(name: String): Array[String] = {
    val in = Source
      .fromResource(getClass.getPackageName.replace('.', '/') + s"/$name")
      .getLines()
      .toArray

    // If the file exists but was encrypted, then attempt to decrypt it with the environment variable.
    if (in.headOption.exists(_.startsWith(AdventOfCodeEncrypted))) {
      requireAdventOfCodeKey()
      val cipher = Cipher.getInstance("AES")
      cipher.init(Cipher.DECRYPT_MODE, getOrCreateKey())
      new String(
        cipher.doFinal(Base64.getDecoder.decode(in.drop(1).mkString)),
        StandardCharsets.UTF_8
      ).split("\n")
    } else {

      // It's not encrypted, but we'll try to overwrite it if we can find it in the filesystem
      val uri = Thread
        .currentThread()
        .getContextClassLoader
        .getResource(getClass.getPackageName.replace('.', '/') + s"/$name")
      if (sys.env.contains(AdventOfCodeKey) && uri.getProtocol == "file") {
        uri.getFile
          .split("/target/")
          .headOption
          .map(File(_))
          .map(_ / "src/test/resources")
          .map(_ / getClass.getPackageName.replace('.', '/'))
          .map(_ / name)
          .foreach { out =>
            val cipher = Cipher.getInstance("AES")
            cipher.init(Cipher.ENCRYPT_MODE, getOrCreateKey())
            out.toFile.writeAll(
              AdventOfCodeEncrypted + "\n",
              Base64.getEncoder.encodeToString(
                cipher.doFinal(in.map(_ + "\n").flatMap(_.getBytes))
              )
            )
          }
      }
      // Return the unencrypted input
      in
    }
  }
}

object AdventUtils {

  /** Create a whole new year of Advent of code! */
  def main(args: Array[String]): Unit = {
    import java.time.LocalDate
    val year = args.headOption.getOrElse(LocalDate.now().getYear.toString)

    // If we can find this class' source, we can write code next to it in subpackages
    val uri = Thread
      .currentThread()
      .getContextClassLoader
      .getResource(getClass.getName.replace('.', '/') + ".class")

    if (uri.getProtocol == "file") {

      // The ./src/test/ directory to add files to
      val root: Option[Path] = uri.getFile.split("/target/").headOption.map(File(_)).map(_ / "src/test")

      // Find the most recent Day0Input.txt file and copy it to the new year
      root.toSeq
        .flatMap(_.walk)
        .filter(_.name == "Day0Input.txt")
        .filter(_.isFile)
        .map(_.toFile)
        .sortBy(_.toString())
        .lastOption
        .map(_.slurp())
        .foreach { basic =>
          root
            .map(_ / "resources" / getClass.getPackageName.replace('.', '/') / s"advent$year")
            .foreach { dst =>
              dst.toDirectory.createDirectory(failIfExists = true)
              for (day <- 0 to 25)
                (dst / s"Day${day}Input.txt").toFile.writeAll(basic)
            }
        }

      // Find the most recent AdventOfCodeDay0Spec.scala file and copy it to the new year
      val day0code = root.toSeq
        .flatMap(_.walk)
        .filter(_.name == "AdventOfCodeDay0Spec.scala")
        .filter(_.isFile)
        .map(_.toFile)
        .sortBy(_.toString())
        .lastOption

      day0code
        .map(_.slurp())
        .foreach { basic =>
          root
            .map(_ / "scala" / getClass.getPackageName.replace('.', '/') / s"advent$year")
            .foreach { dst =>
              dst.toDirectory.createDirectory(failIfExists = true)
              for (day <- 0 to 25)
                (dst / s"AdventOfCodeDay${day}Spec.scala").toFile.writeAll(
                  basic
                    .replaceAll("advent\\d\\d\\d\\d", s"advent$year")
                    .replaceAll("Advent of Code \\d\\d\\d\\d Day 0", s"Advent of Code $year Day $day")
                    .replaceAll("ZERO", day.toString)
                    .replaceAll("Day0", s"Day$day")
                )
            }
        }

      // Find the most recent AdventUtils.scala file and copy it to the new year
      day0code
        .map(_.parent / "AdventUtils.scala")
        .filter(_.exists)
        .map(_.toFile.slurp())
        .foreach { basic =>
          root
            .map(_ / "scala" / getClass.getPackageName.replace('.', '/') / s"advent$year" / "AdventUtils.scala")
            .foreach { dst => dst.toFile.writeAll(basic.replaceAll("advent\\d\\d\\d\\d", s"advent$year")) }
        }

    } else {
      println("Can't find sources.")
    }
  }
}
