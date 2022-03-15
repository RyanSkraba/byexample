package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.arrayWithSize;
import static org.hamcrest.io.FileMatchers.anExistingDirectory;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.fail;

import java.io.IOException;
import java.nio.file.Path;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** Temporary directories. */
class TempDirTest {

  @TempDir private static Path memberAllTmp;

  @TempDir private Path memberEachTmp;

  private static Path beforeAllTmp = null;
  private Path beforeEachTmp = null;

  @BeforeAll
  static void beforeAll(@TempDir Path tmp) {
    // The directories are already created
    assertThat(tmp.toFile(), anExistingDirectory());
    assertThat(memberAllTmp.toFile(), anExistingDirectory());

    if (beforeAllTmp != null && beforeAllTmp.equals(tmp))
      fail("beforeAllTmp exists and isn't different");
    if (memberAllTmp.equals(tmp)) fail("memberAllTmp isn't different");
    beforeAllTmp = tmp;
  }

  @BeforeEach
  void beforeEach(@TempDir Path tmp) {
    testWithDirectory(tmp);
    beforeEachTmp = tmp;
  }

  @Test
  void testWithDirectory(@TempDir Path tmp) {
    assertThat(tmp.toFile(), anExistingDirectory());
    if (beforeAllTmp != null && beforeAllTmp.equals(tmp))
      fail("beforeAllTmp exists and isn't different");
    if (memberAllTmp.equals(tmp)) fail("memberAllTmp isn't different");
    if (beforeEachTmp != null && beforeEachTmp.equals(tmp))
      fail("beforeEachTmp exists and isn't different");
    if (memberEachTmp.equals(tmp)) fail("memberAllTmp isn't different");
  }

  @Test
  void testWithDirectoryAgain(@TempDir Path tmp) {
    testWithDirectory(tmp);
  }

  @Test
  void testWithFile(@TempDir java.io.File tmp) throws IOException {
    assertThat(tmp, anExistingDirectory());
    assertThat(tmp.listFiles(), arrayWithSize(0));
    assertFalse(tmp.createNewFile());
  }
}
