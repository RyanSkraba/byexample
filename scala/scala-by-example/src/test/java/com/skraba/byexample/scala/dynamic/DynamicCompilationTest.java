package com.skraba.byexample.scala.dynamic;

import static com.skraba.byexample.scala.dynamic.DynamicCompilationSpec.greeterSnippet;
import static com.skraba.byexample.scala.dynamic.DynamicHelper.compile;
import static java.util.function.Predicate.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isA;
import static org.hamcrest.Matchers.not;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.skraba.byexample.scala.dynamic.DynamicCompilationSpec.Greeter;
import org.junit.jupiter.api.Test;

/**
 * Running dynamically compiled scala code in Java. See {@link DynamicCompilationSpec} for the
 * details.
 */
public class DynamicCompilationTest {

  @Test
  public void testAnExpressionShouldReturnHello() {
    assertThat(compile("\"Hello!\""), is("Hello!"));
    assertThat(compile("\"\"\"Hello!\"\"\""), is("Hello!"));
  }

  @Test
  public void testAnExpressionShouldDoSimpleMath() {
    assertThat(compile("1 + 2 * 3"), is(7));
  }

  @Test
  public void testShouldNotHaveAccessToVariableInScope() {
    int x = 3;
    assertThat(1 + 2 * x, is(7));
    assertThrows(scala.tools.reflect.ToolBoxError.class, () -> compile("1 + 2 * x"));
  }

  @Test
  public void testShouldHaveAccessToClasses() {
    assertThat(
        compile("com.skraba.byexample.scala.dynamic.DynamicCompilationSpec.greeterSnippet"),
        isA(String.class));
  }

  @Test
  public void testBasic() {
    Greeter g = (Greeter) compile(String.format(greeterSnippet(), "Hello, ", "!!"));
    assertThat(g.greet("dynamic class"), is("Hello, dynamic class!!"));

    // and the internal class can be defined again independently in another snippet
    Greeter g2 = (Greeter) compile(String.format(greeterSnippet(), "Greetings, ", "!!!"));
    assertThat(g2.greet("other class"), is("Greetings, other class!!!"));

    // The two greeters both implement Greeter, but are not the same classes.
    assertThat(g.getClass(), not(g2.getClass()));
    assertThat(g.getClass().getInterfaces(), is(g2.getClass().getInterfaces()));
  }
}
