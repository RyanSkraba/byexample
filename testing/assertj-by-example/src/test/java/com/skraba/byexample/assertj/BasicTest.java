package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.assertj.core.api.Assertions.fail;

import java.util.Arrays;
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

  @Test
  void testPrimitiveByte() {
    byte value = 0x12;
    Byte boxed = value;

    // Careful with casting
    assertThat(value).isNotEqualTo(0x12).isEqualTo((byte) 0x12).isGreaterThan((byte) 0x00);
    assertThat(boxed).isNotEqualTo(0x12).isEqualTo((byte) 0x12).isGreaterThan((byte) 0x00);
  }

  @Test
  void testPrimitiveByteArray() {
    byte[] value = {0x12, 0x34, (byte) 0xFF};

    // Array equality in Java is same instance unless you use the helper method.
    //noinspection ArrayEquals
    assertThat(value.equals(new byte[] {0x12, 0x34, (byte) 0xFF})).isFalse();
    assertThat(Arrays.equals(value, new byte[] {0x12, 0x34, (byte) 0xFF})).isTrue();

    // Arrays contents are checked with the matcher
    assertThat(value)
        .isEqualTo(new byte[] {0x12, 0x34, (byte) 0xFF})
        .contains(0x12)
        .contains(0x12, 0x34, 0xFF);
  }
}
