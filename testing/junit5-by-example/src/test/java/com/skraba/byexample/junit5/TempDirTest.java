package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.arrayWithSize;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.io.FileMatchers.anExistingDirectory;
import static org.junit.jupiter.api.Assertions.assertFalse;

import java.io.IOException;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.RepeatedTest;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** Temporary directories. */
class TempDirTest {

  /** A temporary directory set once for all tests. */
  @TempDir private static Path memberAllTmp;

  /** A temporary directory set at the start of each test. */
  @TempDir private Path memberEachTmp;

  /** A temporary directory set once for all tests via the @BeforeAll annotation. */
  private static Path beforeAllTmp = null;

  /** A temporary directory set once for all tests via the @BeforeEach annotation. */
  private Path beforeEachTmp = null;

  /** A temporary directory passed by arguments. */
  private Path argTmp = null;

  private static Set<Path> encountered = new HashSet<>();

  @BeforeAll
  static void beforeAll(@TempDir Path tmp) {
    // No temporary directories have been recorded yet
    assertThat(encountered, hasSize(0));
    assertThat(beforeAllTmp, nullValue());
    beforeAllTmp = tmp;

    // These two directories can be encountered here for the first time
    assertThat(memberAllTmp.toFile(), anExistingDirectory());
    assertThat(beforeAllTmp.toFile(), anExistingDirectory());

    // Add them to the encountered list
    assertThat(encountered.add(memberAllTmp), is(true));
    assertThat(encountered.add(beforeAllTmp), is(true));
  }

  @BeforeEach
  void beforeEach(@TempDir Path tmp) {
    // Another created directory before each test.
    testWithDirectory(memberEachTmp);
    testWithDirectory(tmp);
    beforeEachTmp = tmp;
  }

  @AfterEach
  void afterEach() {
    // Ensure that of all the encountered temporary directories, only these five exist after the
    // test.
    for (Path e : encountered) {
      if (e.equals(memberAllTmp)
          || e.equals(memberEachTmp)
          || e.equals(beforeAllTmp)
          || e.equals(beforeEachTmp)
          || e.equals(argTmp)) {
        assertThat(e.toFile(), anExistingDirectory());
      } else {
        assertThat(e.toFile(), not(anExistingDirectory()));
      }
    }
    argTmp = null;
    memberEachTmp = null;
    beforeEachTmp = null;
  }

  @AfterAll
  static void afterAll() {

    // For every testWithDirectory, 3 TempDir were created (for BeforeEach, as a member, and as an
    // argument)
    // Likewise 3 TempDir were created for the testWithFile
    // And 2 were created for BeforeAll and as a member.
    assertThat(encountered, hasSize(35));

    // Of all those encountered directories, only two still exist at this point.
    for (Path e : encountered) {
      if (e.equals(memberAllTmp) || e.equals(beforeAllTmp)) {
        assertThat(e.toFile(), anExistingDirectory());
      } else {
        assertThat(e.toFile(), not(anExistingDirectory()));
      }
    }
  }

  @RepeatedTest(10)
  void testWithDirectory(@TempDir Path tmp) {
    // This is a totally new directory
    assertThat(tmp.toFile(), anExistingDirectory());
    assertThat(encountered.add(tmp), is(true));
    argTmp = tmp;
  }

  @Test
  void testWithFile(@TempDir java.io.File tmp) throws IOException {
    testWithDirectory(tmp.toPath());
    assertThat(tmp.listFiles(), arrayWithSize(0));
    assertFalse(tmp.createNewFile());
  }
}
