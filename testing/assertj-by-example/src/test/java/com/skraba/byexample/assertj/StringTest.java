package com.skraba.byexample.assertj;

import java.nio.CharBuffer;
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

  @Test
  void testCharSequence() {
    // Note that all CharSequence with the same contents aren't necessarily equal!
    CharSequence issue = CharBuffer.wrap("ABC-1234");

    // The CharBuffer is not equal to the "same" String
    Assertions.assertThat(issue).isNotEqualTo("ABC-1234");

    // Unless you specify the Comparator explicitly.
    Assertions.assertThat(issue)
        .usingComparator(CharSequence::compare)
        .isNotBlank()
        .isEqualTo("ABC-1234")
        .contains("C-1")
        .startsWith("ABC")
        .endsWith("1234")
        .matches("[A-Z]+-\\d+");
  }
}
