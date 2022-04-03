package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Objects;
import org.junit.jupiter.api.Test;
import org.opentest4j.AssertionFailedError;

/** Basic tests and assertions. */
class BasicTest {

  @Test
  void testFailure() {
    // There are better ways to test exceptions!  This example just shows one control flow example
    try {
      Objects.requireNonNull(null);
      fail("We should never arrive here");
    } catch (NullPointerException ignored) {
    }

    // By itself, fail throws an AssertionError
    AssertionFailedError ex =
        assertThrows(AssertionFailedError.class, () -> fail("We arrived here"));
    assertThat(ex.getMessage(), is("We arrived here"));

    // Assertions also throw exceptions
    AssertionError ex2 = assertThrows(AssertionError.class, () -> assertThat("X", is("Y")));
    assertThat(ex2.getMessage(), containsString("Expected: is \"Y\""));
    assertThat(ex2.getMessage(), containsString("but: was \"X\""));
  }
}
