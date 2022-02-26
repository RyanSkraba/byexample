package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.as;
import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;
import org.assertj.core.api.InstanceOfAssertFactories;
import org.junit.jupiter.api.Test;

/** Simple assertions on optional values. */
class OptionalTest {

  @Test
  void testBasic() {

    // Checking for absence
    Optional<String> none = Optional.empty();
    assertThat(none).isEmpty();

    // Non-optional tests for comparison
    String issue = "ABC-1234";
    assertThat(issue)
        .isNotBlank()
        .isEqualTo("ABC-1234")
        .contains("C-1")
        .startsWith("ABC")
        .endsWith("1234")
        .matches("[A-Z]+-\\d+");


    // Checking for presence
    Optional<String> opt = Optional.of(issue);
    assertThat(opt).contains("ABC-1234");
    assertThat(opt).get().isEqualTo("ABC-1234");

    // This can't compile because of the way Generics work...
    // assertThat(opt).get().startsWith("ABC");

    // These factories are the workaround.
    assertThat(opt).get(as(InstanceOfAssertFactories.STRING)).startsWith("ABC");

    // And conditions on the contained value can be written like this.
    assertThat(opt)
        .hasValueSatisfying(
            i ->
                assertThat(i)
                    .isNotBlank()
                    .isEqualTo("ABC-1234")
                    .contains("C-1")
                    .startsWith("ABC")
                    .endsWith("1234")
                    .matches("[A-Z]+-\\d+"));
  }

  @Test
  void testBasicInt() {
    int issue = 1234;

    Optional<Integer> none = Optional.empty();
    assertThat(none).isEmpty();

    Optional<Integer> opt = Optional.of(issue);
    assertThat(opt).contains(1234);

    assertThat(opt)
        .get(as(InstanceOfAssertFactories.INTEGER))
        .isGreaterThanOrEqualTo(1000)
        .isLessThanOrEqualTo(9999);
  }
}
