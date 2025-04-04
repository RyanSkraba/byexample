package com.skraba.byexample.lanterna.progress;

import com.googlecode.lanterna.TerminalPosition;
import com.googlecode.lanterna.TextCharacter;
import com.googlecode.lanterna.graphics.TextGraphics;

/** Monitors progress on a task by printing a progress bar. */
public class BarProgress implements ProgressMonitor {

  private final TextGraphics graphics;
  private final TerminalPosition position;
  private TerminalPosition endPosition;
  private final TextCharacter fill;
  private final TextCharacter fill2;
  private final TextCharacter clear;
  private int barSize;
  private final boolean overCount;
  private int tickCount;
  private int tickTotal;

  /**
   * @param position Starting point of the bar.
   * @param barSize The length of the bar to be drawn.
   * @param overCount Whether to count higher than 100% (by overdrawing on the same bar).
   */
  public BarProgress(
      TextGraphics graphics,
      TerminalPosition position,
      int barSize,
      TextCharacter fill,
      TextCharacter fill2,
      TextCharacter clear,
      boolean overCount) {
    this.graphics = graphics;
    this.position = position;
    this.endPosition = position.withRelativeColumn(barSize - 1);
    this.barSize = barSize;
    this.fill = fill;
    this.fill2 = fill2;
    this.clear = clear;
    this.overCount = overCount;
  }

  public BarProgress(
      TextGraphics graphics,
      TerminalPosition position,
      int barSize,
      TextCharacter fill,
      TextCharacter fill2,
      TextCharacter clear) {
    this(graphics, position, barSize, fill, fill2, clear, true);
  }

  public BarProgress(
      TextGraphics graphics,
      TerminalPosition position,
      int barSize,
      TextCharacter fill,
      TextCharacter clear) {
    this(graphics, position, barSize, fill, fill, clear, false);
  }

  /**
   * @param position Starting point of the bar.
   * @param barSize The length of the bar to be drawn.
   * @param overCount Whether to count higher than 100% (by overdrawing on the same bar).
   */
  public BarProgress(
      TextGraphics graphics, TerminalPosition position, int barSize, boolean overCount) {
    this(
        graphics,
        position,
        barSize,
        TextCharacter.fromCharacter('=')[0],
        TextCharacter.fromCharacter('-')[0],
        TextCharacter.fromCharacter(' ')[0],
        overCount);
  }

  public BarProgress(TextGraphics graphics, TerminalPosition position, int barSize) {
    this(graphics, position, barSize, false);
  }

  @Override
  public void tick(int tickCount, int tickTotal) {
    this.tickCount = tickCount;
    this.tickTotal = tickTotal;
    if (tickTotal == 0) {
      tickCount = 1;
      tickTotal = 1;
    }
    // The progress in number of characters.
    int size = barSize * tickCount / tickTotal;
    TextCharacter charBar = fill;
    TextCharacter charClear = clear;
    if (overCount && size > barSize) {
      charBar = size / barSize % 2 == 0 ? fill : fill2;
      charClear = size / barSize % 2 == 0 ? fill2 : fill;
      size = size % barSize;
    }
    drawBar(graphics, position, size, endPosition, charBar, charClear);
  }

  public static void drawBar(
      TextGraphics graphics,
      TerminalPosition start,
      int size,
      TerminalPosition end,
      TextCharacter fill,
      TextCharacter rest) {
    TerminalPosition barEnd = start.withRelativeColumn(Math.max(0, size - 1));
    if (size <= 0) {
      graphics.drawLine(start, end, rest);
    } else if (barEnd.getColumn() >= end.getColumn()) {
      graphics.drawLine(start, end, fill);
    } else {
      graphics.drawLine(start, barEnd, fill);
      graphics.drawLine(barEnd.withRelativeColumn(1), end, rest);
    }
  }

  public void setBarSize(int barSize) {
    this.endPosition = position.withRelativeColumn(barSize - 1);
    this.barSize = barSize;
    tick(tickCount, tickTotal);
  }
}
