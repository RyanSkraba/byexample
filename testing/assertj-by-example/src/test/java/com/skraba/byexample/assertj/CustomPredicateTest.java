package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;

import org.assertj.core.api.Condition;
import org.junit.jupiter.api.Test;

/** Simple assertions using a custom predicate. */
class CustomPredicateTest {

  @Test
  void testBasicString() {
    String issue = "ABC-1234";
    assertThat(issue)
        .is(new Size8Condition())
        // expressed as a lambda, requires a description
        .is(new Condition<>(v -> v.length() == 8, "size is 8"));
  }

  /** This condition can be reused in all tests. */
  public static class Size8Condition extends Condition<String> {
    @Override
    public boolean matches(String value) {
      return value.length() == 8;
    }
  }
}
