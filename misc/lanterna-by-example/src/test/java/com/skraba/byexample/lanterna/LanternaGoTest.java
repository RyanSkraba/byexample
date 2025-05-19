package com.skraba.byexample.lanterna;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.tinfoiled.docopt4s.shaded.docoptjava.DocoptExitException;
import org.junit.jupiter.api.Test;

/** TODO: How can the colourful, rich text examples be tested? */
public class LanternaGoTest {

  @Test
  public void testReadability() {
    for (String line : LanternaGo.DOC.split("\n")) {
      assertThat("Check DOC line length: " + line, line.length(), lessThan(80));
    }
  }

  @Test
  public void testBasicExceptions() {
    // --version and --help don't run the tool but drop out with "successful" exit
    DocoptExitException ex =
        assertThrows(DocoptExitException.class, () -> LanternaGo.go("--version"));
    assertThat(ex.getExitCode(), is(0));
    assertThat(ex.getMessage(), is(LanternaGo.VERSION));
    ex = assertThrows(DocoptExitException.class, () -> LanternaGo.go("--help"));
    assertThat(ex.getExitCode(), is(0));
    assertThat(ex.getMessage(), is(LanternaGo.DOC));

    // Unknown arguments are failures.
    ex = assertThrows(DocoptExitException.class, () -> LanternaGo.go("--garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> LanternaGo.go("--debug", "--garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> LanternaGo.go("--garbage", "--debug"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> LanternaGo.go("--garbage", "garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
  }
}
