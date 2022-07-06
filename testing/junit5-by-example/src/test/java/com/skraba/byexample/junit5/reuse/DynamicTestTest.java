package com.skraba.byexample.junit5.reuse;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

import java.util.stream.Stream;
import org.junit.jupiter.api.DynamicNode;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.function.Executable;

/** Using {@link org.junit.jupiter.api.DynamicTest} to automatically create test cases. */
public class DynamicTestTest {

  private void testGreaterThanZero(Long l) {
    assertThat(l, greaterThan(0L));
  }

  private Executable lambdaTestGreaterThanZero(Long l) {
    return () -> testGreaterThanZero(1_000_000L);
  }

  @TestFactory
  public Stream<DynamicNode> testSimple() {
    return Stream.of(
        dynamicTest("One million", () -> testGreaterThanZero(1_000_000L)),
        dynamicTest("One billion", () -> testGreaterThanZero(1_000_000L)));
  }

  @TestFactory
  public Stream<DynamicNode> testSimpleLambda() {
    return Stream.of(
        dynamicTest("One million", lambdaTestGreaterThanZero(1_000_000L)),
        dynamicTest("One billion", lambdaTestGreaterThanZero(1_000_000L)));
  }

  @TestFactory
  public Stream<DynamicTest> testStream() {
    return DynamicTest.stream(
        Stream.of(1L, 2L, 3L, 4L), value -> "Testing " + value, this::testGreaterThanZero);
  }
}
