package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.within;

import org.junit.jupiter.api.Test;

/** Simple assertions on numeric primitives. */
class NumericTests {

  @Test
  void testBasicDouble() {
    double piApprox = 355d / 113d;
    assertThat(piApprox)
        .isNotCloseTo(Math.PI, within(0.0000002))
        .isCloseTo(Math.PI, within(0.0000003));
  }
}
