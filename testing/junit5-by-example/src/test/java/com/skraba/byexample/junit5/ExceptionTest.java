package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.skraba.byexample.scala.Project;
import org.junit.jupiter.api.Test;

/** Simple assertions handling exceptions. */
class ExceptionTest {

  @Test
  void testBasic() {
    NullPointerException ex =
        assertThrows(NullPointerException.class, () -> new Project.Issue(null, null));
    assertThat(ex.getMessage(), is("Issue.project is required"));
  }
}
