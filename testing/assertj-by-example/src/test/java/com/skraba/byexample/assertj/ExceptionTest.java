package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.*;

import com.skraba.byexample.scala.Project;
import org.junit.jupiter.api.Test;

/** Simple assertions handling exceptions. */
class ExceptionTest {

  @Test
  void testBasic() {
    assertThatExceptionOfType(NullPointerException.class)
        .isThrownBy(() -> new Project.Issue(null, null))
        .withMessage("Issue.project is required");

    assertThatThrownBy(() -> new Project.Issue(null, null))
        .isInstanceOf(Exception.class)
        .hasMessage("Issue.project is required");

    NullPointerException npe =
        catchThrowableOfType(() -> new Project.Issue(null, null), NullPointerException.class);

    assertThat(npe).hasMessage("Issue.project is required");
  }
}
