package com.skraba.byexample.assertj;

import java.util.Objects;

/** Simple POJO with some logic to test. */
public class Project {

  private String name;

  public Project(String name) {
    setName(name);
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    Objects.requireNonNull(name, "Project.name is required");
    this.name = name;
  }

  @Override
  public boolean equals(Object o) {
    if (this == o) {
      return true;
    }
    if (o == null || getClass() != o.getClass()) {
      return false;
    }
    Project project = (Project) o;
    return Objects.equals(name, project.name);
  }

  @Override
  public int hashCode() {
    return Objects.hash(name);
  }

  @Override
  public String toString() {
    return "Project{name='" + name + "'}";
  }

  public static class Issue {

    private Project project;
    private String number;

    public Issue(Project project, String number) {
      setProject(project);
      setNumber(number);
    }

    public Project getProject() {
      return project;
    }

    public void setProject(Project project) {
      Objects.requireNonNull(project, "Issue.project is required");
      this.project = project;
    }

    public String getNumber() {
      return number;
    }

    public void setNumber(String number) {
      Objects.requireNonNull(number, "Issue.number is required");
      this.number = number;
    }

    public String getFullName() {
      return project.getName() + "-" + number;
    }

    @Override
    public boolean equals(Object o) {
      if (this == o) {
        return true;
      }
      if (o == null || getClass() != o.getClass()) {
        return false;
      }
      Issue issue = (Issue) o;
      return Objects.equals(project, issue.project) && Objects.equals(number, issue.number);
    }

    @Override
    public int hashCode() {
      return Objects.hash(project, number);
    }

    @Override
    public String toString() {
      return "Issue{project=" + project + ", number='" + number + "'}";
    }
  }
}
