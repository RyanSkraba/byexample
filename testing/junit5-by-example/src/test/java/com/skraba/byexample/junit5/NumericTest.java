package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notANumber;

import org.junit.jupiter.api.Test;

/** Simple assertions on numeric primitives. */
class NumericTest {

  @Test
  void testBasicDouble() {
    double piApprox = 355d / 113d;
    assertThat(piApprox, not(closeTo(Math.PI, 0.0000002)));
    assertThat(piApprox, closeTo(Math.PI, 0.0000003));
  }

  @Test
  void testBasicDoubleNaN() {
    double canonicalNaN = Double.NaN;
    assertThat(canonicalNaN, notANumber());

    //noinspection ConstantConditions
    assertThat(canonicalNaN == canonicalNaN, is(false));
    //noinspection ConstantConditions
    assertThat(Double.valueOf(canonicalNaN).equals(canonicalNaN), is(true));

    long longCanonicalNaN = Double.doubleToLongBits(canonicalNaN);
    assertThat(longCanonicalNaN, is(Double.doubleToRawLongBits(canonicalNaN)));

    assertThat(canonicalNaN, equalTo(canonicalNaN));
    assertThat(canonicalNaN, is(Double.longBitsToDouble(longCanonicalNaN)));
    assertThat(Double.doubleToLongBits(canonicalNaN), is(longCanonicalNaN));

    // There are other NaNs
    double otherNaN = Double.longBitsToDouble(longCanonicalNaN + 1);
    assertThat(otherNaN, notANumber());

    assertThat(otherNaN, is(canonicalNaN));
    assertThat(Double.doubleToLongBits(otherNaN), is(Double.doubleToLongBits(canonicalNaN)));
    assertThat(Double.doubleToRawLongBits(otherNaN), not(Double.doubleToRawLongBits(canonicalNaN)));
  }
}
