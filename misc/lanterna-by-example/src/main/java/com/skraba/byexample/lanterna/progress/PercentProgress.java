package com.skraba.byexample.lanterna.progress;

import com.googlecode.lanterna.TerminalPosition;
import com.googlecode.lanterna.graphics.TextGraphics;
import com.googlecode.lanterna.terminal.Terminal;
import java.io.IOException;

/** Monitors progress on a task by printing the completed count and percentage. */
public class PercentProgress implements ProgressMonitor {

  private final Terminal term;
  private final TextGraphics graphics;
  private final TerminalPosition position;
  private TerminalPosition lastPosition;

  public PercentProgress(Terminal term, TerminalPosition position) throws IOException {
    this.term = term;
    this.graphics = term.newTextGraphics();
    this.position = position;
    this.lastPosition = position;
  }

  @Override
  public void tick(int tickCount, int tickTotal) throws IOException {
    double percent = tickTotal == 0 ? 100 : 100d * tickCount / tickTotal;
    graphics.putString(
        position,
        String.valueOf(tickCount) + " / " + tickTotal + " (" + Math.round(percent) + "%)");
    while (term.getCursorPosition().getColumn() < lastPosition.getColumn()) {
      term.putCharacter(' ');
    }
    term.flush();
    lastPosition = term.getCursorPosition();
  }
}
