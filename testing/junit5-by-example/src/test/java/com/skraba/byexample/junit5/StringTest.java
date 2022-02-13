package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import org.junit.jupiter.api.Test;

/** Simple assertions on string primitives. */
class StringTest {

  @Test
  void testBasicString() {
    String issue = "ABC-1234";
    assertThat(issue, not(emptyOrNullString()));
    assertThat(issue, is("ABC-1234"));
    assertThat(issue, containsString("C-1"));
    assertThat(issue, startsWith("ABC"));
    assertThat(issue, endsWith("1234"));
    assertThat(issue, matchesRegex("[A-Z]+-\\d+"));
  }
}
