package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import org.assertj.core.api.Condition;
import org.junit.jupiter.api.Test;

/** Custom assertions using {@link Condition}, and lambda */
class CustomPredicateTest {

  String abc1234 = "ABC-1234";

  @Test
  void testCustomConditions() {
    // is, has, satisfies are all aliases to check a Condition. The description is in the custom
    // Condition.
    var singleDash = new CountDashes(1);
    var doubleDash = new CountDashes(2);
    assertThat(abc1234)
        .is(singleDash)
        .has(singleDash)
        .satisfies(singleDash)
        .isNot(doubleDash)
        .doesNotHave(doubleDash);
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat("ABC-1234").is(doubleDash))
        .withMessageEndingWith("to be with 2 dashes");
  }

  @Test
  void testBasicStringConditionAsLambda() {
    // expressed as a lambda in a Condition, requires a description.
    assertThat(abc1234).is(new Condition<>(v -> v.length() == 8, "size is 8"));
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(
            () -> assertThat("ABCD-1234").is(new Condition<>(v -> v.length() == 8, "size is 8")))
        .withMessageEndingWith("to be size is 8");
  }

  @Test
  void testBasicStringPredicateAsLambda() {
    // expressed as a lambda predicate (returning true for success), requires a description.
    assertThat(abc1234).matches(v -> v.length() == 8, "size is 8");
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat("ABCD-1234").matches(v -> v.length() == 8, "size is 8"))
        .withMessageEndingWith("to match 'size is 8' predicate.");
  }

  /** A custom Condition counting the number of dashes in the string. */
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
