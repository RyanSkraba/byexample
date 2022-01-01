package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.*;

import org.junit.jupiter.api.Test;

/** Simple assertions on primitive types. */
class PrimitiveTest {

  @Test
  void testBasicString() {
    // Strings
    String greeting = "Hello world";
    assertThat(greeting).isNotBlank().contains("lo").startsWith("Hello").endsWith("world");

    assertThat("JIRA-1234").matches("[A-Z]+-\\d+");
  }

  @Test
  void testBasicDouble() {
    double pi = 3.14;
    assertThat(3.14).isNotCloseTo(Math.PI, within(0.001));
    assertThat(3.14).isEqualTo(Math.PI, within(0.002));
  }
}
