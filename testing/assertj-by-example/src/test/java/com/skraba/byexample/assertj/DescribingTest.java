package com.skraba.byexample.assertj;

import com.skraba.byexample.assertj.Project.Issue;
import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.Test;

/** Simple examples on how to describe tests meaningfully. */
class DescribingTest {

  /** @link https://assertj.github.io/doc/#assertj-core-assertion-description */
  @Test
  void testBasicDescriptions() {
    Issue issue = new Issue(new Project("ABC"), "123");
    String issueName = issue.getFullName();

    // This doesn't have a meaningful error message.
    Assertions.assertThat(issueName.toUpperCase().equals(issueName)).isTrue();
    // NB If you forget the ".isTrue", this does nothing.

    // "as" should describe what we're doing, without assuming it broke
    Assertions.assertThat(issueName.toUpperCase().equals(issueName))
        .as("check %s is upper case", issueName)
        .isTrue();

    // "overridingErrorMessage" and its alias "withFailMessage" should describe what failed
    Assertions.assertThat(issueName.toUpperCase().equals(issueName))
        .overridingErrorMessage("%s must be upper case", issueName)
        .isTrue();
    Assertions.assertThat(issueName.toUpperCase().equals(issueName))
        .withFailMessage("%s must be upper case", issueName)
        .isTrue();

    // All of the above can use a lambda or supplier instead
    Assertions.assertThat(issueName.toUpperCase().equals(issueName))
        .as(() -> "check" + issueName + " is upper case")
        .isTrue();
  }
}
