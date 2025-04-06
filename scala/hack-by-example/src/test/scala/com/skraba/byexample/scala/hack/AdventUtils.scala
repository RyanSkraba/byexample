package com.skraba.byexample.scala.hack

import com.skraba.byexample.scala.hack.AdventUtils.SrcTestScalaRoot
import org.scalatest.Assertions

import java.nio.charset.StandardCharsets
import java.security.Key
import java.util.Base64
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, KeyGenerator}
import scala.io.Source
import scala.reflect.io.{Directory, File}

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
  val AdventOfCodeEncrypted = s"!! Set $AdventOfCodeKey to decrypt (https://adventofcode.com/about)"

  /** @return
    *   the [[Key]] being used to decrypt inputs and solutions.
    */
  def getOrCreateKey(): Key = {
    val keyEnv = sys.env.get(AdventOfCodeKey).filter(_.nonEmpty).getOrElse {
      val gen = KeyGenerator.getInstance("AES")
      gen.init(256)
      val key = gen.generateKey
      val encoded = Base64.getEncoder.encodeToString(key.getEncoded)
      println(s"Using temporary key: $AdventOfCodeKey=$encoded")
      encoded
    }
    new SecretKeySpec(Base64.getDecoder.decode(keyEnv), "AES")
  }

  /** Causes a test to be canceled (but not failed) if the key isn't present. */
  def requireAdventOfCodeKey(): Unit = {
    val filteredEnv = sys.env.filter(_._1 == AdventOfCodeKey)
    Assertions.assume(filteredEnv.contains(AdventOfCodeKey), "(missing key for running solutions)")
    Assertions.assume(filteredEnv.get(AdventOfCodeKey).exists(_.nonEmpty), "(\"\" key for running solutions)")
  }

  /** A helper method for testing and getting the encrypted answer. This can be used temporarily but shouldn't be
    * committed to a repo.
    */
  def decryptDoNotSubmit(in: Long): Long =
    decryptDoNotSubmit(in.toString, "decryptLong").toLong

  /** A helper method for testing and getting the encrypted answer. This can be used temporarily but shouldn't be
    * committed to a repo.
    */
  def decryptDoNotSubmit(in: Long, rewrite: Boolean): Long =
    decryptDoNotSubmit(in.toString, "decryptLong", rewrite).toLong

  /** A helper method for testing and getting the encrypted answer. This can be used temporarily but shouldn't be
    * committed to a repo.
    */
  def decryptDoNotSubmit(in: String, method: String = "decrypt", rewrite: Boolean = true): String = {
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, getOrCreateKey())
    val encrypted = Base64.getEncoder.encodeToString(cipher.doFinal(in.getBytes))

    if (SrcTestScalaRoot.isEmpty)
      println(s"Encrypted $in: $method(\"$encrypted\")")

    // Rewrite the test files with the encrypted value.
    if (rewrite)
      SrcTestScalaRoot.foreach { root =>
        root.walk.toSeq
          .filterNot(_.name == "AdventUtils.scala")
          .filter(_.name.endsWith(".scala"))
          .map(_.toFile)
          .foreach { f =>
            val scalaCode = f.slurp()
            if (scalaCode.contains("decryptDoNotSubmit")) {

              s"decryptDoNotSubmit\\((\"?)\\Q$in\\E\\1L?\\)".r.findFirstMatchIn(scalaCode) match {
                case Some(m) =>
                  println(s"${f.name}: Replacing with $method(\"$encrypted\") in ${f.name}")
                  val out = scalaCode.patch(m.start, s"""$method("$encrypted")""", m.end - m.start)
                  f.writeAll(out)
                case _ =>
              }

            }
          }
      }

    in
  }

  /** @return the long value encrypted in the string */
  def decryptLong(in: String): Long = decrypt(in).toLong

  /** @return the long value encrypted in the string */
  def decrypt(in: String): String = {
    requireAdventOfCodeKey()
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.DECRYPT_MODE, getOrCreateKey())
    new String(cipher.doFinal(Base64.getDecoder.decode(in)), StandardCharsets.UTF_8)
  }

  /** Get the puzzle input corresponding to the filename, in the right Advent of Code directory.
    *
    * If the file isn't encrypted but the [[AdventOfCodeKey]] is set in the environment, overwrite it with an encrypted
    * text.
    */
  def puzzleInput(name: String): IndexedSeq[String] = {
    val in = Source.fromResource(getClass.getPackageName.replace('.', '/') + s"/$name").getLines().toIndexedSeq

    // If the file exists but was encrypted, then attempt to decrypt it with the environment variable.
    if (in.headOption.exists(_.startsWith(AdventOfCodeEncrypted))) {
      decrypt(in.dropWhile(_.startsWith("!!")).mkString).split("\n")
    } else {

      // It's not encrypted, but we'll try to overwrite it if we can find it in the filesystem
      if (sys.env.contains(AdventOfCodeKey))
        AdventUtils.SrcTestResourcesRoot.foreach { root =>
          Option(root)
            .map(_ / name)
            .foreach { out =>
              val cipher = Cipher.getInstance("AES")
              cipher.init(Cipher.ENCRYPT_MODE, getOrCreateKey())
              out.toFile.writeAll(
                AdventOfCodeEncrypted + "\n",
                Base64.getEncoder.encodeToString(cipher.doFinal(in.toArray.map(_ + "\n").flatMap(_.getBytes)))
              )
            }
        }

      // Return the unencrypted input
      in
    }
  }

  /** A standard way to strip input blocks in tests. */
  def trimSplit(input: String): IndexedSeq[String] = {
    if (input == null) IndexedSeq.empty
    else input.trim.stripMargin.split("\n").toIndexedSeq
  }
}

object AdventUtils {

  lazy val SrcTest: Option[Directory] =
    Option(Thread.currentThread().getContextClassLoader.getResource(getClass.getName.replace('.', '/') + ".class"))
      .filter(_.getProtocol == "file")
      .flatMap(_.getFile.split("/target/").headOption.map(File(_)).map(_ / "src/test").map(_.toDirectory))

  lazy val SrcTestResourcesRoot: Option[Directory] =
    SrcTest.map(_ / "resources" / getClass.getPackageName.replace('.', '/')).map(_.toDirectory)

  lazy val SrcTestScalaRoot: Option[Directory] =
    SrcTest.map(_ / "scala" / getClass.getPackageName.replace('.', '/')).map(_.toDirectory)

  /** Create a whole new year of Advent of code! */
  def main(args: Array[String]): Unit = {
    import java.time.LocalDate
    val year = args.headOption.getOrElse(LocalDate.now().getYear.toString)

    if (SrcTest.isEmpty) {
      println("Can't find sources.")
    }

    // If we can find this class' source, we can write code next to it in subpackages
    SrcTestResourcesRoot.foreach { rsrcRoot =>
      // Find the most recent Day0Input.txt file and copy it to the new year
      rsrcRoot.walk.toSeq
        .filter(_.name == "Day0Input.txt")
        .filter(_.isFile)
        .map(_.toFile)
        .sortBy(_.toString())
        .lastOption
        .map(_.slurp())
        .foreach { basic =>
          val dst = (rsrcRoot / s"advent$year").toDirectory
          dst.createDirectory(force = true)
          for (day <- 0 to 25 if !(dst / s"Day${day}Input.txt").exists)
            (dst / s"Day${day}Input.txt").toFile.writeAll(basic)
        }
    }

    // If we can find this class' source, we can write code next to it in subpackages
    SrcTestScalaRoot.foreach { mainRoot =>
      // Find the most recent AdventOfCodeDay0Spec.scala file and copy it to the new year
      val day0code = mainRoot.walk.toSeq
        .filter(_.name == "AdventOfCodeDay0Spec.scala")
        .filter(_.isFile)
        .map(_.toFile)
        .sortBy(_.toString())
        .lastOption

      day0code
        .map(_.slurp())
        .foreach { basic =>
          val dst = (mainRoot / s"advent$year").toDirectory
          dst.createDirectory(force = true)
          for (day <- 0 to 25 if !(dst / s"AdventOfCodeDay${day}Spec.scala").exists)
            (dst / s"AdventOfCodeDay${day}Spec.scala").toFile.writeAll(
              basic
                .replaceAll(
                  "https://adventofcode.com/\\d\\d\\d\\d/day/\\d+",
                  s"https://adventofcode.com/$year/day/$day"
                )
                .replaceAll("advent\\d\\d\\d\\d", s"advent$year")
                .replaceAll("Advent of Code \\d\\d\\d\\d Day 0", s"Advent of Code $year Day $day")
                .replaceAll("ZERO", day.toString)
                .replaceAll("Day0", s"Day$day")
            )
        }

      // Find the most recent AdventUtils.scala file and copy it to the new year
      day0code
        .map(_.parent / "AdventUtils.scala")
        .filter(_.exists)
        .map(_.toFile.slurp())
        .foreach { basic =>
          val dst = (mainRoot / s"advent$year" / "AdventUtils.scala").toFile
          dst.writeAll(basic.replaceAll("advent\\d\\d\\d\\d", s"advent$year"))
        }
    }
  }
}
