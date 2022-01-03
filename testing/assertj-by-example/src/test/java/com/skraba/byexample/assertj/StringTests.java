package com.skraba.byexample.assertj;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/** Simple assertions on string primitives. */
class StringTests {

  @Test
  void testBasicString() {
    String jira = "JIRA-1234";
    Assertions.assertThat(jira)
        .isNotBlank()
        .isEqualTo("JIRA-1234")
        .contains("A-1")
        .startsWith("JIRA")
        .endsWith("1234")
        .matches("[A-Z]+-\\\\d+");
  }
}
