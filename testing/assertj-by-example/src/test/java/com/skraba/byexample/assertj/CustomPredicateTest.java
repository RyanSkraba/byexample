package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import org.assertj.core.api.Condition;
import org.junit.jupiter.api.Test;

/** Simple assertions using a custom predicate. */
class CustomPredicateTest {

  @Test
  void testBasicStringCondition() {
    assertThat("ABC-1234").is(new CountDashes(1));
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat("ABC-1234").is(new CountDashes(2)))
        .withMessageEndingWith("to be with 2 dashes");
  }

  @Test
  void testBasicStringConditionAsLambda() {
    // expressed as a lambda, requires a description
    assertThat("ABC-1234").is(new Condition<>(v -> v.length() == 8, "size is 8"));
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(
            () -> assertThat("ABCD-1234").is(new Condition<>(v -> v.length() == 8, "size is 8")))
        .withMessageEndingWith("to be size is 8");
  }

  @Test
  void testBasicStringPredicateAsLambda() {
    assertThat("ABC-1234").matches(v -> v.length() == 8, "size is 8");
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat("ABCD-1234").matches(v -> v.length() == 8, "size is 8"))
        .withMessageEndingWith("to match 'size is 8' predicate.");
  }

  /** Condition counting the number of dashes in the string. */
  public static class CountDashes extends Condition<String> {

    private final int expected;

    CountDashes(int expected) {
      super("with " + expected + " dashes");
      this.expected = expected;
    }

    @Override
    public boolean matches(String value) {
      return value.replaceAll("-", "").length() == value.length() - expected;
    }
  }
}
