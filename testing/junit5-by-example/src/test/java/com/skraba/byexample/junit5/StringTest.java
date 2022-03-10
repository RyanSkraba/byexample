package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;

import java.nio.CharBuffer;
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

  @Test
  void testCharSequence() {
    // Note that all CharSequence with the same contents aren't necessarily equal!
    CharSequence issue = CharBuffer.wrap("ABC-1234");

    // The CharBuffer is not equal to the "same" String
    assertThat(issue, not("ABC-1234"));

    // The workaround is to wrap matchers with hasToString(...)
    assertThat(issue, hasToString("ABC-1234"));
    assertThat(issue, hasToString(not(emptyOrNullString())));
    assertThat(issue, hasToString(is("ABC-1234")));
    assertThat(issue, hasToString(containsString("C-1")));
    assertThat(issue, hasToString(startsWith("ABC")));
    assertThat(issue, hasToString(endsWith("1234")));
    assertThat(issue, hasToString(matchesRegex("[A-Z]+-\\d+")));
  }
}
