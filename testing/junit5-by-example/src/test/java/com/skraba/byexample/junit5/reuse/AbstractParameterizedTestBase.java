package com.skraba.byexample.junit5.reuse;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.junit.jupiter.api.Assumptions.assumeTrue;

import java.util.stream.Stream;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

/**
 * An example of a parameterized test that contains reusable logic.
 *
 * <p>In this case, the parent abstract class has two concrete static inner classes that will be
 * evaluated (although one inner class is designed to be skipped automatically).
 *
 * <p>Only the {@link #getNumbers()} method needs to be configured in a concrete class.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
abstract class AbstractParameterizedTestBase {

  /** @return the list of numbers to test. */
  public abstract Stream<Integer> getNumbers();

  @BeforeAll
  void setup() {
    assumeTrue(!this.getClass().getSimpleName().contains("DoNotRun"));
  }

  @ParameterizedTest
  @MethodSource("getNumbers")
  void testEven(Integer num) {
    assertThat(num % 2, is(0));
  }

  @ParameterizedTest
  @MethodSource("getNumbers")
  void testLessThanSixFigs(Integer num) {
    assertThat(num, lessThan(100_000));
  }

  /** A concrete inner class that executes tests. */
  @Nested
  static class InnerTest extends AbstractParameterizedTestBase {

    @Override
    public Stream<Integer> getNumbers() {
      return Stream.of(0, 2, 4, 6, 8);
    }
  }

  /** A concrete inner class that will not be run, because the setup method checks an assumption. */
  @Nested
  static class InnerDoNotRunTest extends AbstractParameterizedTestBase {

    @Override
    public Stream<Integer> getNumbers() {
      return Stream.of(1, 3, 5, 7);
    }
  }
}
