package com.skraba.byexample.benchmarking.toprofile;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import org.docopt.DocoptExitException;
import org.junit.jupiter.api.Test;

/** Unit tests for {@link ToProfileGo}. */
public class ToProfileGoTest {

  @Test
  public void testReadability() {
    for (String line : ToProfileGo.DOC.split("\n")) {
      assertThat("Check DOC line length: " + line, line.length(), lessThan(80));
    }
  }

  @Test
  public void testBasicExceptions() {
    // --version and --help don't run the tool but drop out with "successful" exit
    DocoptExitException ex =
        assertThrows(DocoptExitException.class, () -> ToProfileGo.go("--version"));
    assertThat(ex.getExitCode(), is(0));
    assertThat(ex.getMessage(), is(ToProfileGo.VERSION));
    ex = assertThrows(DocoptExitException.class, () -> ToProfileGo.go("--help"));
    assertThat(ex.getExitCode(), is(0));
    assertThat(ex.getMessage(), is(ToProfileGo.DOC));

    // Unknown arguments are failures.
    ex = assertThrows(DocoptExitException.class, () -> ToProfileGo.go("--garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> ToProfileGo.go("--debug", "--garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> ToProfileGo.go("--garbage", "--debug"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
    ex = assertThrows(DocoptExitException.class, () -> ToProfileGo.go("--garbage", "garbage"));
    assertThat(ex.getExitCode(), is(1));
    assertThat(ex.getMessage(), nullValue());
  }

  @Test
  public void testSieve() {
    Set<Integer> consumer = new HashSet<>();
    Set<Integer> primes = ToProfileGo.sieveOfEratosthenes(600, false, false, false, consumer::add);

    assertThat(primes, hasSize(109));
    assertThat(consumer, equalTo(primes));
  }

  @Test
  public void testSieveSuperHappySexy() {
    // Only get the super prime numbers.
    List<Integer> onlySuper = new ArrayList<>();
    Set<Integer> primes = ToProfileGo.sieveOfEratosthenes(600, true, false, false, onlySuper::add);
    assertThat(primes, hasSize(109));
    assertThat(onlySuper, hasSize(29));

    // Only get the happy prime numbers.
    List<Integer> onlyHappy = new ArrayList<>();
    assertThat(
        ToProfileGo.sieveOfEratosthenes(600, false, true, false, onlyHappy::add), equalTo(primes));
    assertThat(onlyHappy, hasSize(24));

    // Only the sexy prime numbers.
    List<Integer> onlySexy = new ArrayList<>();
    assertThat(
        ToProfileGo.sieveOfEratosthenes(600, false, false, true, onlySexy::add), equalTo(primes));
    assertThat(onlySexy, hasSize(85));

    // These should be the numbers that are super, happy and sexy
    List<Integer> expectedAll =
        onlySuper.stream()
            .filter(onlyHappy::contains)
            .filter(onlySexy::contains)
            .collect(Collectors.toList());
    assertThat(expectedAll, contains(31, 109, 331, 367, 563));

    // TODO: This currently is broken!
    List<Integer> all = new ArrayList<>();
    assertThat(ToProfileGo.sieveOfEratosthenes(600, true, true, true, all::add), equalTo(primes));
    // assertThat(all, hasSize(5));
  }
}
