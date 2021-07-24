package com.skraba.byexample.lanterna;

import com.googlecode.lanterna.SGR;
import com.googlecode.lanterna.TerminalPosition;
import com.googlecode.lanterna.TerminalSize;
import com.googlecode.lanterna.TextCharacter;
import com.googlecode.lanterna.TextColor;
import com.googlecode.lanterna.TextColor.ANSI;
import com.googlecode.lanterna.TextColor.RGB;
import com.googlecode.lanterna.graphics.TextGraphics;
import com.googlecode.lanterna.input.KeyStroke;
import com.googlecode.lanterna.input.KeyType;
import com.googlecode.lanterna.terminal.DefaultTerminalFactory;
import com.googlecode.lanterna.terminal.Terminal;
import com.googlecode.lanterna.terminal.TerminalResizeListener;
import com.skraba.byexample.lanterna.progress.BarProgress;
import com.skraba.byexample.lanterna.progress.PercentProgress;
import com.skraba.byexample.lanterna.progress.ProgressMonitor;
import com.skraba.byexample.misc.ClrsCc;

import org.docopt.Docopt;
import org.docopt.DocoptExitException;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Random;

/** Some examples of rich console applications using the Lanterna library. */
public class LanternaGo {

  public static final String VERSION = "0.0.1-SNAPSHOT";

  public static final String DOC =
      String.join(
          "\n",
          "Runs rich console examples using Lanterna.",
          "",
          "Usage:",
          "  LanternaGo [--speed=SPEED] [--private] [hacker|listener]",
          "",
          "Options:",
          "  -h --help      Show this screen.",
          "  --version      Show version.",
          "  --speed=SPEED  The number of The times to say hello [default: 100]",
          "  --private      Use a private terminal",
          "",
          "Commands:",
          "   hacker  Outputs a non-interactive hollywood hacking scenario.",
          " listener  Demonstrates listening to keystrokes in a private window.");

  /**
   * Runs the tool. This does not handle any docopt exception automatically while parsing the
   * command line.
   *
   * @param args command-line arguments as described in DOC .
   */
  public static void go(String... args)
      throws DocoptExitException, InternalDocoptException, IOException, InterruptedException {
    Map<String, Object> opts = new Docopt(DOC).withVersion(VERSION).withExit(false).parse(args);

    int speed = Integer.parseInt((String) opts.get("--speed"));
    boolean privateTerm = opts.get("--private").equals(true);
    try (Terminal term = new DefaultTerminalFactory().createTerminal()) {
      if (privateTerm) {
        term.enterPrivateMode();
        term.clearScreen();
        // This *almost* looks the same -- but it appears that there are differences between
        // newlines and setting cursor positions between the two.
        term.setCursorPosition(0, 0);
      }
      try {
        if (opts.get("hacker").equals(true)) {
          goHollywoodHacking(term, speed);
        } else {
          goListeners(term);
        }
      } finally {
        if (privateTerm) term.exitPrivateMode();
      }
    }
  }

  /** Main entry point to the application. */
  public static void main(String[] args) {
    // All of the command is executed in the go method, and this wraps DocOpt and exceptions for
    // console feedback.
    try {
      go(args);
    } catch (DocoptExitException ex) {
      if (ex.getMessage() != null)
        (ex.getExitCode() == 0 ? System.out : System.err).println(ex.getMessage());
      System.exit(ex.getExitCode());
    } catch (InternalDocoptException ex) {
      System.out.println(ex.getDocopt());
      if (ex.getMessage() != null) {
        System.out.println();
        System.out.println(ex.getMessage());
      }
      System.exit(1);
    } catch (Exception ex) {
      System.err.println(DOC);
      System.err.println();
      ex.printStackTrace();
      System.exit(1);
    }
  }

  /** {@link org.docopt.DocoptExitException} constructors are protected. */
  public static class InternalDocoptException extends RuntimeException {

    private final String docopt;

    public InternalDocoptException(String message, Throwable cause, String docopt) {
      super(message, cause);
      this.docopt = docopt;
    }

    public String getDocopt() {
      return docopt;
    }
  }

  /**
   * Demonstrate the low level operations on a {@link Terminal}.
   *
   * @param term The terminal to use.
   * @param sleep Used to control the speed of the story, smaller is faster.
   * @throws IOException If a problem occurs during terminal IO.
   * @throws InterruptedException If a problem occurs while the thread is animating the response.
   */
  private static void goHollywoodHacking(final Terminal term, final int sleep)
      throws IOException, InterruptedException {

    final Random rnd = new Random(0L);
    term.setCursorVisible(false);

    // Remember this for later.
    final TerminalSize size = term.getTerminalSize();

    // Simple text is put to the screen as a String or by character.
    term.putString("Connecting");
    term.flush();
    // Animate the simplest progress with twenty periods.
    for (int i = 0; i < 20; i++) {
      term.putCharacter('.');
      term.flush();
      Thread.sleep(sleep);
    }

    // If we're running in interactive mode, this will advance/scroll the terminal.
    // In a private window, it will move the cursor down the screen.  Right now,
    // we are only adding text, but later we'll want to move the cursor around.
    term.putString("\n\n");
    term.flush();

    // SGR is used to "style" characters written to the screen.
    term.enableSGR(SGR.BOLD);
    term.putString("Lanterna Global Sequence Launcher 1.0\n");
    term.putString("=====================================\n\n");
    term.disableSGR(SGR.BOLD);
    term.putString("Attention!\n");
    term.putString("Unauthorized access is prohibited.\n\n");

    // Simulate typing the login in a single line.
    term.setForegroundColor(ANSI.CYAN);
    term.enableSGR(SGR.BOLD);
    term.putString("     Login: ");
    term.disableSGR(SGR.BOLD);
    term.flush();
    term.setForegroundColor(ANSI.YELLOW);
    for (char c : "Administrator".toCharArray()) {
      term.putCharacter(c);
      term.flush();
      Thread.sleep(sleep + rnd.nextInt(sleep));
    }

    // Because we want to support both private and interactive mode, we add a newline
    // (which scrolls the window) and then note the new position.
    term.putCharacter('\n');

    // Simulate typing the login in a single line.
    term.setForegroundColor(ANSI.CYAN);
    term.enableSGR(SGR.BOLD);
    term.putString("     Password: ");
    term.disableSGR(SGR.BOLD);
    term.flush();
    term.setForegroundColor(ANSI.YELLOW);
    for (char c : "joshua".toCharArray()) {
      term.putCharacter(c);
      term.flush();
      Thread.sleep(sleep + rnd.nextInt(sleep));
      // Overwrite the displayed text cause it's more secure.
      term.setCursorPosition(term.getCursorPosition().withRelativeColumn(-1));
      term.putCharacter('*');
    }

    // Again, advance three rows.  The first is empty, the second will have our
    // HACKER banner and the third will scan through numbers while detecting the
    // security token.
    term.putString("\n\n\n");

    // You can get and adjust the position of the cursor. Note that moving the cursor
    // down doesn't scroll the existing contents, but printing a newline does.
    TerminalPosition pos = term.getCursorPosition();

    // On the third line, print the prompt for the secure token.
    term.setCursorPosition(pos.withRelativeColumn(5));
    term.setForegroundColor(ANSI.CYAN);
    term.enableSGR(SGR.BOLD);
    term.putString("Secure token: ");
    term.disableSGR(SGR.BOLD);
    term.flush();

    // These positions are where we'll draw the hacker banner and the security scan.
    TerminalPosition hackingPosition = pos.withRelativeRow(-1);
    TerminalPosition secureTokenPosition = term.getCursorPosition();
    Thread.sleep(5L * sleep);

    // But start the hacking!  Here's the contents of the banner.
    char[] hacking = new char[size.getColumns()];
    Arrays.fill(hacking, ' ');
    String hackingMsg = "HACKING ENABLED";
    System.arraycopy(
        hackingMsg.toCharArray(),
        0,
        hacking,
        (hacking.length - hackingMsg.length()) / 2,
        hackingMsg.length());

    // Number of characters in the key, and whether they've been hacked yet.
    boolean[] stable = new boolean[20];
    int hackAttemptsPer = 5;

    for (int i = stable.length * hackAttemptsPer; --i >= 0; ) {
      // The flickering red hack message to increase efficiency.
      int redRange = (int) (255 * i / (stable.length * hackAttemptsPer - 1d));
      term.enableSGR(SGR.BOLD);
      term.enableSGR(SGR.REVERSE);
      term.setCursorPosition(hackingPosition);
      for (char c : hacking) {
        term.setForegroundColor(new RGB(255 - (redRange <= 0 ? 0 : rnd.nextInt(redRange)), 0, 0));
        term.putCharacter(c);
      }

      term.disableSGR(SGR.REVERSE);
      term.setForegroundColor(ANSI.YELLOW);

      // Just solve one digit every once in a while in any order.  That's how all of this works.
      if (i % hackAttemptsPer == 0) {
        int newStable = rnd.nextInt(stable.length);
        while (stable[newStable]) {
          newStable = (newStable + 1) % stable.length;
        }
        term.setCursorPosition(secureTokenPosition.withRelativeColumn(newStable));
        term.putCharacter((char) ('0' + rnd.nextInt(10)));
        stable[newStable] = true;
      }

      term.disableSGR(SGR.BOLD);
      for (int j = 0; j < stable.length; j++) {
        if (!stable[j]) {
          term.setCursorPosition(secureTokenPosition.withRelativeColumn(j));
          term.putCharacter((char) ('0' + rnd.nextInt(10)));
        }
      }
      term.flush();
      Thread.sleep(sleep);
    }

    // The hacking succeeded, so brag a bit with a green banner.
    hackingMsg = "HACKING SUCCESSFUL";
    System.arraycopy(
        hackingMsg.toCharArray(),
        0,
        hacking,
        (hacking.length - hackingMsg.length()) / 2,
        hackingMsg.length());
    term.enableSGR(SGR.BOLD);
    term.enableSGR(SGR.REVERSE);
    term.setCursorPosition(hackingPosition);
    term.setForegroundColor(ANSI.GREEN);
    for (char c : hacking) {
      term.putCharacter(c);
    }
    term.flush();

    term.resetColorAndSGR();
    term.putString("\n\n\nWelcome!");
    term.flush();
    term.setCursorVisible(true);
    Thread.sleep(5L * sleep);
  }

  /**
   * Demonstrate using {@link TextGraphics} and listeners on a {@link Terminal}.
   *
   * @param term The terminal to use.
   * @throws IOException If a problem occurs during terminal IO.
   */
  private static void goListeners(final Terminal term) throws IOException {

    // Setup for private, fullscreen mode.
    term.enterPrivateMode();
    term.clearScreen();
    term.setCursorVisible(false);

    // A bit simpler to use then putting characters to the terminal.
    final TextGraphics textGraphics = term.newTextGraphics();
    final TerminalSize terminalSize = term.getTerminalSize();

    int tickCount = 0;
    int tickTotal = 57;

    textGraphics.setForegroundColor(
        new TextColor.RGB(ClrsCc.WHITE[0], ClrsCc.WHITE[1], ClrsCc.WHITE[2]));
    textGraphics.setBackgroundColor(TextColor.ANSI.DEFAULT);

    // Create a bunch of progress bars to put on the screen.
    int row = 0;
    final List<ProgressMonitor> progress = new ArrayList<>();
    // Set up a percentage progress.
    {
      String msg = "Processing file:";
      textGraphics.putString(0, row, msg, SGR.BOLD);
      term.flush();
      progress.add(new PercentProgress(term, new TerminalPosition(msg.length() + 1, row)));
    }
    row++;
    // Set up the progress bar.
    {
      textGraphics.putString(0, row, "[", SGR.BOLD);
      textGraphics.putString(terminalSize.getColumns() - 1, row, "]", SGR.BOLD);
      progress.add(
          new BarProgress(
              term.newTextGraphics(), new TerminalPosition(1, row), terminalSize.getColumns() - 2));
    }
    row++;
    // Set up the progress bar with overcount
    {
      textGraphics.putString(0, row, "<", SGR.BOLD);
      textGraphics.putString(terminalSize.getColumns() - 1, row, ">", SGR.BOLD);
      progress.add(
          new BarProgress(
              term.newTextGraphics(),
              new TerminalPosition(1, row),
              terminalSize.getColumns() - 2,
              true));
    }
    row++;
    // Set up the progress bar with overcount and colours
    {
      textGraphics.putString(0, row, "[", SGR.BOLD);
      textGraphics.putString(terminalSize.getColumns() - 1, row, "]", SGR.BOLD);
      progress.add(
          new BarProgress(
              term.newTextGraphics(),
              new TerminalPosition(1, row),
              terminalSize.getColumns() - 2,
              new TextCharacter(' ', TextColor.ANSI.DEFAULT, TextColor.ANSI.BLUE),
              new TextCharacter(' ', TextColor.ANSI.DEFAULT, TextColor.ANSI.CYAN),
              new TextCharacter(' ', TextColor.ANSI.DEFAULT, TextColor.ANSI.DEFAULT)));
    }
    row++;

    // Set up a resize listener on a different thread.
    term.addResizeListener(
        new TerminalResizeListener() {
          @Override
          public void onResized(Terminal terminal, TerminalSize newSize) {
            ((BarProgress) progress.get(1)).setBarSize(newSize.getColumns() - 2);
            textGraphics.putString(newSize.getColumns() - 1, 1, "]", SGR.BOLD);
            ((BarProgress) progress.get(2)).setBarSize(newSize.getColumns() - 2);
            textGraphics.putString(newSize.getColumns() - 1, 2, ">", SGR.BOLD);
            ((BarProgress) progress.get(3)).setBarSize(newSize.getColumns() - 2);
            textGraphics.putString(newSize.getColumns() - 1, 3, "]", SGR.BOLD);
          }
        });

    // Do a blocking read on keyboard input and update the progress.
    // Left and right control the progress, up and down the expected number.
    // Shift permits you to go out of bounds.
    // Control jumps by increments of 10.
    KeyStroke keyStroke = null;
    do {
      for (ProgressMonitor p : progress) {
        p.tick(tickCount, tickTotal);
      }

      keyStroke = term.readInput();
      if (keyStroke != null) {
        textGraphics.putString(0, row + 1, keyStroke.toString());
      }

      boolean changeTotal =
          keyStroke.getKeyType() == KeyType.ArrowUp || keyStroke.getKeyType() == KeyType.ArrowDown;
      boolean decrement =
          keyStroke.getKeyType() == KeyType.ArrowUp || keyStroke.getKeyType() == KeyType.ArrowLeft;
      int value = (keyStroke.isCtrlDown() ? 10 : 1) * (decrement ? -1 : 1);
      boolean enforceMinMax = !keyStroke.isShiftDown();

      if (changeTotal) {
        tickTotal += value;
        if (enforceMinMax) {
          tickTotal = Math.max(1, tickTotal);
        }
      } else {
        tickCount += value;
        if (enforceMinMax) {
          tickCount = Math.max(0, Math.min(tickTotal, tickCount));
        }
      }
    } while (keyStroke.getKeyType() != KeyType.Escape);
  }
}
