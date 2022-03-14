package com.skraba.byexample.junit5;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsStringIgnoringCase;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.io.FileMatchers.aFileNamed;
import static org.hamcrest.io.FileMatchers.aFileWithAbsolutePath;
import static org.hamcrest.io.FileMatchers.aFileWithCanonicalPath;
import static org.hamcrest.io.FileMatchers.aFileWithSize;
import static org.hamcrest.io.FileMatchers.aReadableFile;
import static org.hamcrest.io.FileMatchers.aWritableFile;
import static org.hamcrest.io.FileMatchers.anExistingDirectory;
import static org.hamcrest.io.FileMatchers.anExistingFile;
import static org.hamcrest.io.FileMatchers.anExistingFileOrDirectory;

/** Testing the file system. */
class FileSystemTest {

  @TempDir private static Path tmp;

  @BeforeAll
  static void beforeAll() throws IOException {
    assertThat(tmp.toFile(), anExistingDirectory());
    try (Writer w = Files.newBufferedWriter(tmp.resolve("hello.txt"))) {
      w.write("Hello world!");
    }
  }

  @Test
  void testFiles() {
    // Only files are supported, not paths.
    File hello = tmp.resolve("hello.txt").toFile();

    assertThat(hello, anExistingFile());
    assertThat(hello, anExistingFileOrDirectory());
    assertThat(hello, not(anExistingDirectory()));

    assertThat(hello, aReadableFile());
    assertThat(hello, aWritableFile());
    assertThat(hello, aFileWithSize(12));

    assertThat(hello, aFileNamed(containsStringIgnoringCase("hello")));
    assertThat(hello, aFileNamed(not(containsStringIgnoringCase(tmp.getFileName().toString()))));
    assertThat(hello, aFileWithAbsolutePath(containsStringIgnoringCase(tmp.getFileName().toString())));
    assertThat(hello, aFileWithCanonicalPath(containsStringIgnoringCase(tmp.getFileName().toString())));
  }
}
