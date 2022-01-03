package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.not;

import org.junit.jupiter.api.Test;

/** Simple assertions on numeric primitives. */
class NumericTests {

  @Test
  void testBasicDouble() {
    double piApprox = 355d / 113d;
    assertThat(piApprox, not(closeTo(Math.PI, 0.0000003)));
    assertThat(piApprox, closeTo(Math.PI, 0.0000003));
  }
}
