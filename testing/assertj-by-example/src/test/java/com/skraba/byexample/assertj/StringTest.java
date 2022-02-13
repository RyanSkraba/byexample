package com.skraba.byexample.assertj;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/** Simple assertions on string primitives. */
class StringTest {

  @Test
  void testBasicString() {
    String issue = "ABC-1234";
    Assertions.assertThat(issue)
        .isNotBlank()
        .isEqualTo("ABC-1234")
        .contains("C-1")
        .startsWith("ABC")
        .endsWith("1234")
        .matches("[A-Z]+-\\d+");
  }
}
