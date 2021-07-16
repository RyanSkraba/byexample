package com.skraba.byexample.scala;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.nullValue;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.docopt.DocoptExitException;
import org.junit.jupiter.api.Test;
import scala.PartialFunction;
import scala.Tuple3;

/** Testing the {@link JavaScalaGo} command line utility. */
public class JavaScalaGoTest {

  @Test
  public void testReadability() {
    for (String line : JavaScalaGo.DOC.split("\n")) {
      assertThat("Check DOC line length: " + line, line.length(), lessThan(80));
    }
  }

  @Test
  public void testBasicExceptions() {
    // --version and --help don't run the tool but drop out with "successful" exit
    DocoptExitException ex =
        assertThrows(DocoptExitException.class, () -> withConsoleMatch("--version"));
    assertThat(ex.getExitCode(), is(0));
    assertThat(ex.getMessage(), is(JavaScalaGo.VERSION));
    ex = assertThrows(DocoptExitException.class, () -> withConsoleMatch("--help"));
    assertThat(ex.getExitCode(), is(0));
    assertThat(ex.getMessage(), is(JavaScalaGo.DOC));

    // Missing arguments are failures
    ex = assertThrows(DocoptExitException.class, () -> withConsoleMatch("--name"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), is("--name requires argument"));
    ex = assertThrows(DocoptExitException.class, () -> withConsoleMatch("--count"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), is("--count requires argument"));

    // Unknown arguments are failures.
    ex = assertThrows(DocoptExitException.class, () -> withConsoleMatch("--garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> withConsoleMatch("--debug", "--garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> withConsoleMatch("--garbage", "--debug"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> withConsoleMatch("--garbage", "garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
  }

  @Test
  public void testNoArguments() {
    String[] out = withConsoleMatch();

    assertThat(out[0], is("Hello, JavaScalaGo!\n".repeat(10)));
    assertThat(out[1], is(""));
  }

  @Test
  public void testWithScala() {
    String[] out = withConsoleMatch("--scala");
    assertThat(out[0], is("Hello, JavaScalaGo!\n".repeat(10)));
    assertThat(out[1], is(""));
  }

  @Test
  public void testWithName() {
    String[] out = withConsoleMatch("--name", "world");
    assertThat(out[0], is("Hello, world!\n".repeat(10)));
    assertThat(out[1], is(""));
    out = withConsoleMatch("--name=world");
    assertThat(out[0], is("Hello, world!\n".repeat(10)));
    assertThat(out[1], is(""));
  }

  @Test
  public void testWithCount() {
    String[] out = withConsoleMatch("--count", "3");
    assertThat(out[0], is("Hello, JavaScalaGo!\n".repeat(3)));
    assertThat(out[1], is(""));
    out = withConsoleMatch("--count=3");
    assertThat(out[0], is("Hello, JavaScalaGo!\n".repeat(3)));
    assertThat(out[1], is(""));
  }

  @Test
  public String[] withConsoleMatch(String... args) {
    return ScalaGoSpec.withConsoleMatch(
        () -> {
          JavaScalaGo.go(args);
          return null;
        },
        new PartialFunction<Tuple3<Void, String, String>, String[]>() {
          @Override
          public boolean isDefinedAt(Tuple3<Void, String, String> value) {
            return true;
          }

          @Override
          public String[] apply(Tuple3<Void, String, String> v1) {
            return new String[] {v1._2(), v1._3()};
          }
        });
  }
}
