package com.skraba.byexample.scalatags.stamper

import com.skraba.byexample.scalatags.Svg
import scalatags.Text.implicits._
import scalatags.Text.svgTags.{g, rect, use}

import scala.util.Random

/** Draws SVG files by repeating an SVG tag at different X/Y positions on the document.
  *
  * Whenever the Stamper is moved, it leaves a copy of the tag at the current position before moving to the new position
  * (as long as the pen is "down").
  *
  * @param x
  *   The x position for the first element added to the history.
  * @param y
  *   The y position for the first element added to the history.
  * @param stepX
  *   Number of pixels to move in the x direction for a step.
  * @param stepY
  *   Number of pixels to move in the y direction for a step.
  * @param penDown
  *   True to leave a trail as the stamper is moved.
  * @param tag
  *   The SVG tag to reuse when moving.
  * @param history
  *   The tags that have already been added during movement.
  * @param checkpoints
  *   A list of named checkpoint positions during movement.
  */
case class Stamper(
    x: Double = 0,
    y: Double = 0,
    stepX: Double = 1,
    stepY: Double = 1,
    penDown: Boolean = true,
    tag: Tag = rect(
      scalatags.Text.svgAttrs.width := 1,
      scalatags.Text.svgAttrs.height := 1
    ),
    history: List[Tag] = Nil,
    checkpoints: Map[String, (Double, Double)] = Map()
) {

  /** Move a relative number of pixels and add a copy of the stamp. This does not take into account [[stepX]] or
    * [[stepY]].
    *
    * @param dx
    *   The number of pixels to move right.
    * @param dy
    *   The number of pixels to move left.
    * @param tag
    *   The tag to use from now on (unchanged by default)
    * @return
    *   An instance of the stamper at the new position coordinates.
    */
  def stamp(dx: Double = 0, dy: Double = 0, tag: Tag = tag): Stamper = {
    stampAt(this.x + dx, this.y + dy, tag)
  }

  /** Move to an abolute position and add a copy of the stamp. This does not take into account [[stepX]] or [[stepY]].
    *
    * @param x
    *   The new x position to add a stamp.
    * @param y
    *   The new y position to add a stamp.
    * @param tag
    *   The stamp to use from now on (unchanged by default)
    * @return
    *   An instance of the stamper at the new position coordinates.
    */
  def stampAt(x: Double = 0, y: Double = 0, tag: Tag = tag): Stamper = {
    copy(x = x, y = y, tag = tag, history = appendCurrentToHistory)
  }

  /** Move one unit north (up).
    *
    * @param tag
    *   The stamp to use from now on (unchanged by default)
    * @return
    *   An instance of the stamper at the new position.
    */
  def n(tag: Tag = tag): Stamper = stamp(dy = -stepY, tag = tag)

  /** Move one unit south (down).
    *
    * @param tag
    *   The stamp to use from now on (unchanged by default)
    * @return
    *   An instance of the stamper at the new position.
    */
  def s(tag: Tag = tag): Stamper = stamp(dy = +stepY, tag = tag)

  /** Move one unit east (right).
    *
    * @param tag
    *   The stamp to use from now on (unchanged by default)
    * @return
    *   An instance of the stamper at the new position.
    */
  def e(tag: Tag = tag): Stamper = stamp(dx = +stepX, tag = tag)

  /** Move one unit west (left).
    *
    * @param tag
    *   The stamp to use from now on (unchanged by default)
    * @return
    *   An instance of the stamper at the new position.
    */
  def w(tag: Tag = tag): Stamper = stamp(dx = -stepX, tag = tag)

  /** Move one unit northeast. */
  def ne(tag: Tag = tag): Stamper = stamp(dx = +stepX, dy = -stepY, tag = tag)

  /** Move one unit southeast. */
  def se(tag: Tag = tag): Stamper = stamp(dx = +stepX, dy = +stepY, tag = tag)

  /** Move one unit northwest. */
  def nw(tag: Tag = tag): Stamper = stamp(dx = -stepX, dy = -stepY, tag = tag)

  /** Move one unit southwest. */
  def sw(tag: Tag = tag): Stamper = stamp(dx = -stepX, dy = +stepY, tag = tag)

  /** Save the current position of the stamper so it can be recalled later.
    * @param name
    *   The name to associate with the current position.
    */
  def save(name: String): Stamper =
    copy(checkpoints = checkpoints + (name -> (x, y)))

  /** Using the position and [[stepX]] and [[stepY]] values, add the two dimension array of tags into the history of
    * this stamper.
    * @param pattern
    *   A two dimensional array of tags to be added to the history. The tag at (0,0) will be added at position [[x]],
    *   [[y]].
    */
  def draw(pattern: Iterable[Iterable[Option[Tag]]]): Stamper = {
    val newTags: Iterable[Tag] = pattern
      .map(_.zipWithIndex)
      .zipWithIndex
      .flatMap { case (row: Iterable[(Option[Tag], Int)], rowIndex: Int) =>
        row.map {
          case (Some(tag), columnIndex) => Some(tag, rowIndex, columnIndex)
          case _                        => None
        }
      }
      .flatten
      .map { case (tag: Tag, rowIndex: Int, columnIndex: Int) =>
        tag(
          scalatags.Text.svgAttrs.x := x + stepX * columnIndex,
          scalatags.Text.svgAttrs.y := y + stepY * rowIndex
        )
      }

    copy(history = newTags.toList ++ history)
  }

  /** Using the position and [[stepX]] and [[stepY]] values, add a two dimension array of tags into the history of this
    * stamper.
    *
    * @param pattern
    *   A multiline string of characters representing a two dimension of tags.
    * @param withTag
    *   A function mapping a character at a position in the string to a tag.
    */
  def draw(
      pattern: String,
      withTag: Char => Option[Tag] = useStampTag
  ): Stamper = {
    draw(pattern.split("\\n").toIterable.map(_.map(withTag).toIterable))
  }

  /** Clone the entire history of this stamper relative to the existing position.
    *
    * The returned stamper will have two elements in the history: the original grouped history and a clone at the new
    * position. The existing [[x]], [[y]] position will not be changed but the [[stepX]] and [[stepY]] parameters will
    * be increased by the relative locations.
    *
    * @param id
    *   The SVG element id to use for the original history.
    * @param dx
    *   Where to position the clone horizontally.
    * @param dy
    *   Where to position the clone vertically.
    */
  def clone(
      id: String = "id" + Random.alphanumeric.take(10).mkString,
      dx: Double = stepX,
      dy: Double = stepY
  ): Stamper = {
    copy(
      stepX = stepX + dx,
      stepY = stepY + dy,
      history = List(
        g(scalatags.Text.svgAttrs.id := s"$id")(history: _*),
        use(
          scalatags.Text.svgAttrs.xLinkHref := s"#$id",
          Svg.attrTranslate(dx, dy)
        )
      )
    )
  }

  /** Clone the entire history of this stamper to the south and the east without adding any additional tags or changing
    * the position. [[stepX]] and [[stepY]] are used to reposition the clones, and the resulting stamper will have
    * double these values.
    *
    * @param id
    *   The SVG element id to use for the original history.
    */
  def clone4(
      id: String = "id" + Random.alphanumeric.take(10).mkString,
      dx: Double = stepX,
      dy: Double = stepY
  ): Stamper = {
    clone(id, 0, dy).clone(id + "_2", dx, 0)
  }

  /** Helper method that maps any non-space character to the stamp tag. */
  private[this] def useStampTag(c: Char): Option[Tag] =
    if (c == ' ') None else Some(tag)

  /** Helper method to add the tag to the history at the current position. */
  private[this] def appendCurrentToHistory: List[Tag] = {
    if (penDown)
      tag(
        scalatags.Text.svgAttrs.x := x,
        scalatags.Text.svgAttrs.y := y
      ) :: history
    else history
  }
}
