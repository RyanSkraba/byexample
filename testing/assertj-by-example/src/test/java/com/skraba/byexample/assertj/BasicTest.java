package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.fail;

import java.util.Objects;
import org.junit.jupiter.api.Test;

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
    assertThatThrownBy(() -> fail("We arrived here"))
        .isInstanceOf(AssertionError.class)
        .hasMessage("We arrived here");

    // Assertions also throw exceptions
    assertThatThrownBy(() -> assertThat("X").isEqualTo("Y"))
        .isInstanceOf(AssertionError.class)
        .hasMessageContaining("expected: \"Y\"")
        .hasMessageContaining("but was: \"X\"");
  }
}
