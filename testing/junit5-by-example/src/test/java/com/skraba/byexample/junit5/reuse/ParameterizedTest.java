package com.skraba.byexample.junit5.reuse;

import java.util.stream.Stream;
import org.junit.jupiter.api.Nested;

/** Some basic parameterized tests that reuse {@link AbstractParameterizedTestBase}. */
class ParameterizedTest {

  @Nested
  class A extends AbstractParameterizedTestBase {

    @Override
    public Stream<Integer> getNumbers() {
      return Stream.of(0, 2, 4, 6, 8);
    }
  }

  @Nested
  class B extends AbstractParameterizedTestBase {

    @Override
    public Stream<Integer> getNumbers() {
      return Stream.of(1000, 1002, 1004);
    }
  }

  @Nested
  class C extends A {

    @Override
    public Stream<Integer> getNumbers() {
      return super.getNumbers().map(num -> num * 2);
    }
  }
}
