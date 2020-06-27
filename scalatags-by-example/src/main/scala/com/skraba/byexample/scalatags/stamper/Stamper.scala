package com.skraba.byexample.scalatags.stamper

import scalatags.Text.implicits._
import scalatags.Text.svgAttrs.{height, width}
import scalatags.Text.svgTags.rect

/**
  * A Stamper draws SVG files by repeating an SVG element at different X/Y positions on the document.
  *
  * Create a Stamper with the SVG tag (configured without an x or y attribute).  A copy of the tag is "stamped" at the
  * origin, and every movement adds another to the list.  A complete history of tags can be obtained using [[asTags]].
  *
  * @param x           The x position for the first element added to the history.
  * @param y           The y position for the first element added to the history.
  * @param dx          Number of pixels to move in the x direction for a step.
  * @param dy          Number of pixels to move in the y direction for a step.
  * @param stamp       The SVG tag to reuse when moving.
  * @param history     The tags that have already been added during movement.
  * @param checkpoints A list of named checkpoint positions during movement.
  */
case class Stamper(
    x: Double = 0,
    y: Double = 0,
    dx: Double = 1,
    dy: Double = 1,
    stamp: Tag = rect(width := 1, height := 1),
    history: List[Tag] = Nil,
    checkpoints: Map[String, (Double, Double)] = Map()
) {

  /**
    * Move a relative number of pixels and add a copy of the stamp.  This does not take into account [[dx]] or [[dy]].
    *
    * @param dx    The number of pixels to move right.
    * @param dy    The number of pixels to move left.
    * @param stamp The stamp to use from now on (unchanged by default)
    * @return An instance of the stamper at the new position coordinates.
    */
  def mvRel(dx: Double = 0, dy: Double = 0, stamp: Tag = stamp): Stamper = {
    mvAbs(this.x + dx, this.y + dy, stamp)
  }

  /**
    * Move to an abolute position and add a copy of the stamp.  This does not take into account [[dx]] or [[dy]].
    *
    * @param x     The new x position to add a stamp.
    * @param y     The new y position to add a stamp.
    * @param stamp The stamp to use from now on (unchanged by default)
    * @return An instance of the stamper at the new position coordinates.
    */
  def mvAbs(x: Double = 0, y: Double = 0, stamp: Tag = stamp): Stamper = {
    copy(x = x, y = y, stamp = stamp, history = asTags)
  }

  /**
    * Move one unit north (up).
    *
    * @param stamp The stamp to use from now on (unchanged by default)
    * @return An instance of the stamper at the new position.
    */
  def n(stamp: Tag = stamp): Stamper = mvRel(dy = -dy, stamp = stamp)

  /**
    * Move one unit south (down).
    *
    * @param stamp The stamp to use from now on (unchanged by default)
    * @return An instance of the stamper at the new position.
    */
  def s(stamp: Tag = stamp): Stamper = mvRel(dy = +dy, stamp = stamp)

  /**
    * Move one unit east (right).
    *
    * @param stamp The stamp to use from now on (unchanged by default)
    * @return An instance of the stamper at the new position.
    */
  def e(stamp: Tag = stamp): Stamper = mvRel(dx = +dx, stamp = stamp)

  /**
    * Move one unit west (left).
    *
    * @param stamp The stamp to use from now on (unchanged by default)
    * @return An instance of the stamper at the new position.
    */
  def w(stamp: Tag = stamp): Stamper = mvRel(dx = -dx, stamp = stamp)

  /** Move one unit northeast. */
  def ne(stamp: Tag = stamp): Stamper = mvRel(dx = +dx, dy = -dy, stamp = stamp)

  /** Move one unit southeast. */
  def se(stamp: Tag = stamp): Stamper = mvRel(dx = +dx, dy = +dy, stamp = stamp)

  /** Move one unit northwest. */
  def nw(stamp: Tag = stamp): Stamper = mvRel(dx = -dx, dy = -dy, stamp = stamp)

  /** Move one unit southwest. */
  def sw(stamp: Tag = stamp): Stamper = mvRel(dx = -dx, dy = +dy, stamp = stamp)

  def save(name: String): Stamper =
    copy(checkpoints = checkpoints + (name -> (x, y)))

  def asTags: List[Tag] = {
    stamp(
      scalatags.Text.svgAttrs.x := x,
      scalatags.Text.svgAttrs.y := y
    ) :: history
  }

}
