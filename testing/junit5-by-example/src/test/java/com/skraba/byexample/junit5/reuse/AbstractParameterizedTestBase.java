package com.skraba.byexample.junit5.reuse;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.stream.Stream;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/** An example of a parameterized test that contains reusable logic. */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
abstract class AbstractParameterizedTestBase {

  public abstract Stream<Integer> getNumbers();

  @ParameterizedTest
  @MethodSource("getNumbers")
  void testEven(Integer num) {
    assertThat(num % 2, is(0));
  }
}
