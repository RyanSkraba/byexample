package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import org.junit.jupiter.api.Test;

/** Simple assertions handling exceptions. */
class ExceptionTest {

  @Test
  void testBasic() {
    assertThatExceptionOfType(NullPointerException.class)
        .isThrownBy(() -> new Project.Issue(null, null))
        .withMessage("Issue.project is required");
  }
}
