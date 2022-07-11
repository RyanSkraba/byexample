package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.fail;

import java.util.Arrays;
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

  @Test
  void testPrimitiveByte() {
    byte value = 0x12;
    Byte boxed = value;

    // Careful with casting
    assertThat(value, not(0x12));
    assertThat(boxed, not(0x12));
    assertThat(value, is((byte) 0x12));
    assertThat(boxed, is((byte) 0x12));

    // You can't compare a byte with an int
    // assertThat(value, greaterThan(0x00));
    assertThat(value, greaterThan((byte) 0x00));
    assertThat(boxed, greaterThan((byte) 0x00));
    assertThat((int) value, greaterThan(0x00));
    assertThat(boxed.intValue(), greaterThan(0x00));
    assertThat(value, greaterThan((byte) 0x00));
    assertThat(boxed, greaterThan((byte) 0x00));
    assertThat((byte) 0x00, lessThan(value));
    assertThat((byte) 0x00, lessThan(boxed));
  }

  @Test
  void testPrimitiveByteArray() {
    byte[] value = {0x12, 0x34, (byte) 0xFF};

    // Array equality in Java is same instance unless you use the helper method.
    //noinspection ArrayEquals
    boolean arraysDontEqual = value.equals(new byte[] {0x12, 0x34, (byte) 0xFF});
    boolean arraysWithEquals = Arrays.equals(value, new byte[] {0x12, 0x34, (byte) 0xFF});
    assertThat(arraysDontEqual, is(false));
    assertThat(arraysWithEquals, is(true));

    // Arrays contents are checked with the matcher
    assertThat(value, is(new byte[] {0x12, 0x34, (byte) 0xFF}));
  }
}
