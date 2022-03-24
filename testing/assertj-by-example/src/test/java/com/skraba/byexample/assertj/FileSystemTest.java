package com.skraba.byexample.assertj;

import static org.assertj.core.api.Assertions.assertThat;

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
    assertThat(tmp).exists();
    Path hello = tmp.resolve("sub/hello.txt");
    Files.createDirectories(hello.getParent());
    try (Writer w = Files.newBufferedWriter(hello, StandardCharsets.UTF_8)) {
      w.write("Hello world!");
    }
  }

  @Test
  void testFileAsFile() {
    File file = tmp.resolve("sub/hello.txt").toFile();
    assertThat(file)
        .exists()
        .isFile()
        .isNotEmpty()
        .isReadable()
        .isWritable()
        .hasSize(12)
        .hasFileName("hello.txt")
        .hasName("hello.txt")
        .content(StandardCharsets.UTF_8)
        .isEqualTo("Hello world!");
  }

  @Test
  void testPathAsFile() {
    Path file = tmp.resolve("sub/hello.txt");
    assertThat(file)
        .exists()
        .isReadable()
        .isWritable()
        .hasSize(12)
        .hasFileName("hello.txt")
        .content(StandardCharsets.UTF_8)
        .isEqualTo("Hello world!");
  }

  @Test
  void testFileAsDirectory() {
    File dir = tmp.resolve("sub").toFile();
    assertThat(dir)
        .exists()
        .isDirectory()
        .isReadable()
        .isWritable()
        .hasFileName("sub")
        .hasName("sub");
  }

  @Test
  void testPathAsDirectory() {
    Path dir = tmp.resolve("sub");
    assertThat(dir).exists().isDirectory().isReadable().isWritable().hasFileName("sub");
  }
}
