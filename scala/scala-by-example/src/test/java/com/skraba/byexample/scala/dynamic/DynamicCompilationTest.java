package com.skraba.byexample.scala.dynamic;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import com.skraba.byexample.scala.dynamic.DynamicCompilationSpec.Greeter;
import org.junit.jupiter.api.Test;

/**
 * Running dynamically compiled scala code in Java. See {@link DynamicCompilationSpec} for the
 * details.
 */
public class DynamicCompilationTest {

  @Test
  public void testBasic() {
    Greeter g =
        (Greeter)
            DynamicCompilationSpec$.MODULE$.compile(
                getClass().getClassLoader(),
                String.format(DynamicCompilationSpec$.MODULE$.myGreeterClass(), "Hello, ", "!!"));
    assertThat(g.greet("dynamic class"), is("Hello, dynamic class!!"));
  }
}
