package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.matchesRegex;
import static org.hamcrest.Matchers.startsWith;

import org.junit.jupiter.api.Test;

/** Simple assertions on string primitives. */
class StringTests {

  @Test
  void testBasicString() {
    String jira = "JIRA-1234";
    assertThat(jira, is("JIRA-1234"));
    assertThat(jira, containsString("A-1"));
    assertThat(jira, startsWith("JIRA"));
    assertThat(jira, matchesRegex("[A-Z]+-\\\\d+"));
  }
}
