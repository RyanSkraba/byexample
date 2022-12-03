package com.skraba.byexample.scala.hack.advent2022

import scala.io.Source

/** Helpful utilities for the advent code
  */
object AdventUtils {

  def puzzleInput(name: String): Array[String] =
    Source
      .fromResource(getClass.getPackageName.replace('.', '/') + s"/$name")
      .getLines()
      .toArray

}
