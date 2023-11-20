package com.skraba.byexample.scala.hack

import org.scalatest.Assertions

import java.nio.charset.StandardCharsets
import java.security.Key
import java.util.Base64
import javax.crypto.spec.SecretKeySpec
import javax.crypto.{Cipher, KeyGenerator}
import scala.io.Source
import scala.reflect.io.File

/** Helpful utilities for https://adventofcode.com/
  *
  * Specifically, we have been asked to not submit input to repos, so this
  * provides a way to encrypt / decrypt inputs and answers using an environment
  * variable containing a AES 256 key.
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
    val keyEnv = sys.env.getOrElse(
      AdventOfCodeKey, {
        val gen = KeyGenerator.getInstance("AES")
        gen.init(256)
        val key = gen.generateKey
        val encoded = Base64.getEncoder.encodeToString(key.getEncoded)
        println(s"Using temporary key: $encoded")
        encoded
      }
    )
    new SecretKeySpec(Base64.getDecoder.decode(keyEnv), "AES")
  }

  /** A helper method for testing and getting the encrypted answer. This can be
    * used temporarily but shouldn't be committed to a repo.
    */
  def decryptLongDoNotSubmit(in: Long): Long = {
    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.ENCRYPT_MODE, getOrCreateKey())
    cipher.update(in.toString.getBytes)
    val encrypted = Base64.getEncoder.encodeToString(cipher.doFinal())
    println(s"Encrypted $in: $encrypted")
    in
  }

  /** @return the long value encrypted in the string */
  def decryptLong(in: String): Long = {
    Assertions.assume(sys.env.contains(AdventOfCodeKey))

    val cipher = Cipher.getInstance("AES")
    cipher.init(Cipher.DECRYPT_MODE, getOrCreateKey())
    cipher.update(Base64.getDecoder.decode(in))
    new String(cipher.doFinal(), StandardCharsets.UTF_8).toLong
  }

  /** Get the puzzle input corresponding to the filename, in the right Advent of
    * Code directory.
    *
    * If the file isn't encrypted but the [[AdventOfCodeKey]] is set in the
    * environment, overwrite it with an encrypted text.
    */
  def puzzleInput(name: String): Array[String] = {
    val in = Source
      .fromResource(getClass.getPackageName.replace('.', '/') + s"/$name")
      .getLines()
      .toArray

    // If the file exists but was encrypted, then attempt to decrypt it with the environment variable.
    if (in.headOption.exists(_.startsWith(AdventOfCodeEncrypted))) {
      // Cancel this specific test if the environment isn't set.
      Assertions.assume(sys.env.contains(AdventOfCodeKey))
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
