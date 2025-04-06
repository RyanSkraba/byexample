package com.skraba.byexample.scala.hack.advent2024

object AdventUtils extends com.skraba.byexample.scala.hack.AdventUtils {
  implicit class SafeStringSplitter(val str: String) extends AnyVal {
    def trimSplit: IndexedSeq[String] = AdventUtils.trimSplit(str)
  }
}
