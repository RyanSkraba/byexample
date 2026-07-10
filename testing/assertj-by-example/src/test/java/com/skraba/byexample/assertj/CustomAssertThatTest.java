package com.skraba.byexample.assertj;

import static com.skraba.byexample.assertj.CustomAssertThatTest.IssueAssert.assertThat;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import com.skraba.byexample.scala.Project;
import java.util.Objects;
import org.assertj.core.api.AbstractAssert;
import org.junit.jupiter.api.Test;

/** Create an assertion on a custom type. */
class CustomAssertThatTest {

  /** This is the way to add new types of assertions for a type of instance. */
  public static class IssueAssert extends AbstractAssert<IssueAssert, Project.Issue> {
    protected IssueAssert(Project.Issue issue) {
      super(issue, IssueAssert.class);
    }

    public static IssueAssert assertThat(Project.Issue actual) {
      return new IssueAssert(actual);
    }

    public IssueAssert hasProject(String prjName) {
      isNotNull();
      if (!Objects.equals(actual.getProject().getName(), prjName)) {
        failWithMessage(
            "Expected project name to be <%s> but was <%s>",
            prjName, actual.getProject().getName());
      }
      return this;
    }

    public IssueAssert hasNumber(String number) {
      isNotNull();
      if (!Objects.equals(actual.getNumber(), number)) {
        failWithMessage(
            "Expected issue number to be <%s> but was <%s>", number, actual.getNumber());
      }
      return this;
    }

    public IssueAssert hasFull(String full) {
      isNotNull();
      if (!Objects.equals(actual.getFullName(), full)) {
        failWithMessage(
            "Expected issue full name to be <%s> but was <%s>", full, actual.getFullName());
      }
      return this;
    }
  }

  @Test
  void testIssueAssert() {
    var issue = new Project.Issue(new Project("ABC"), "1234");
    assertThat(issue).hasProject("ABC").hasNumber("1234").hasFull("ABC-1234").isNotNull();
    assertThat(issue.getProject()).hasFieldOrPropertyWithValue("name", "ABC");
  }

  @Test
  void testErrorMessages() {
    var issue = new Project.Issue(new Project("ABC"), "1234");
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat((Project.Issue) null).hasProject("XYZ"))
        .withMessage("\nExpecting actual not to be null");
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat(issue).hasProject("XYZ"))
        .withMessage("Expected project name to be <XYZ> but was <ABC>");
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat(issue).hasNumber("2345"))
        .withMessage("Expected issue number to be <2345> but was <1234>");
    assertThatExceptionOfType(AssertionError.class)
        .isThrownBy(() -> assertThat(issue).hasFull("XYZ-2345"))
        .withMessage("Expected issue full name to be <XYZ-2345> but was <ABC-1234>");
  }
}
