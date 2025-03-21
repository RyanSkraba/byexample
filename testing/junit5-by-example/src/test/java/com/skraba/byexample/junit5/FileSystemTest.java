package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.io.FileMatchers.*;

import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

/** Testing the file system. */
class FileSystemTest {

  @TempDir private static Path tmp;

  @BeforeAll
  static void beforeAll() throws IOException {
    assertThat(tmp.toFile(), anExistingDirectory());
    Path hello = tmp.resolve("sub/hello.txt");
    Files.createDirectories(hello.getParent());
    try (Writer w = Files.newBufferedWriter(hello, StandardCharsets.UTF_8)) {
      w.write("Hello world!");
    }
  }

  @Test
  void testFileAsFile() {
    File file = tmp.resolve("sub/hello.txt").toFile();

    assertThat(file, anExistingFile());
    assertThat(file, anExistingFileOrDirectory());
    assertThat(file, not(anExistingDirectory()));

    assertThat(file, aReadableFile());
    assertThat(file, aWritableFile());
    assertThat(file, aFileWithSize(12));

    assertThat(file, aFileNamed(containsString("hello")));
    assertThat(file, aFileNamed(not(containsString(tmp.getFileName().toString()))));
    assertThat(file, aFileWithAbsolutePath(containsString(tmp.getFileName().toString())));
    assertThat(file, aFileWithCanonicalPath(containsString(tmp.getFileName().toString())));
  }

  @Test
  void testFileAsDirectory() {
    File dir = tmp.resolve("sub").toFile();

    assertThat(dir, anExistingDirectory());
    assertThat(dir, anExistingFileOrDirectory());
    assertThat(dir, not(anExistingFile()));

    assertThat(dir, aReadableFile());
    assertThat(dir, aWritableFile());

    assertThat(dir, aFileNamed(containsString("sub")));
    assertThat(dir, aFileWithAbsolutePath(containsString(tmp.getFileName().toString())));
    assertThat(dir, aFileWithCanonicalPath(containsString(tmp.getFileName().toString())));
  }
}
