package com.skraba.byexample.junit5;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.*;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.DayOfWeek;
import java.util.Arrays;
import java.util.Collection;
import java.util.EnumSet;
import java.util.stream.Stream;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.*;

/** Simple assertions on string primitives. */
class ParamTest {

  @ParameterizedTest
  // Almost all the primitives are supported
  @ValueSource(doubles = {1, 2, 3, Double.MAX_VALUE, Double.MIN_VALUE})
  void testValueSourceDoubles(double in) {
    assertThat(in, greaterThan(0d));
  }

  @ParameterizedTest
  @ValueSource(strings = {"", "\n", "\t  "})
  void testValueSourceStrings(String in) {
    assertThat(in, is(blankOrNullString()));
  }

  @ParameterizedTest
  @NullSource
  void testNullSource(String in) {
    assertThat(in, is(nullValue()));
  }

  @ParameterizedTest
  @NullSource
  @EmptySource
  @NullAndEmptySource
  @ValueSource(strings = {"", "\n", "\t  "})
  void testCombinedSources(String in) {
    assertThat(in, is(blankOrNullString()));
  }

  /** Tests that iterate over values of an enum. */
  @Nested
  class EnumSourceTest {

    @ParameterizedTest
    @EnumSource(DayOfWeek.class)
    void testEnumSource(DayOfWeek dow) {
      // Every day of the week is the same seven days laters.
      assertThat(dow.plus(7), is(dow));
    }

    @ParameterizedTest
    @EnumSource(
        value = DayOfWeek.class,
        names = {"SUNDAY", "SATURDAY"})
    void testEnumSourceSomeDays(DayOfWeek dow) {
      assertThat(dow.toString(), startsWith("S"));
    }

    @ParameterizedTest
    @EnumSource(value = DayOfWeek.class, names = "S.+DAY", mode = EnumSource.Mode.MATCH_ANY)
    void testEnumSourceSomeDaysRegex(DayOfWeek dow) {
      EnumSet<DayOfWeek> dows = EnumSet.of(DayOfWeek.SATURDAY, DayOfWeek.SUNDAY);
      assertTrue(dows.contains(dow));
    }
  }

  @ParameterizedTest
  @CsvSource({
    "SATURDAY, 1, SUNDAY, true",
    "SUNDAY, 1, MONDAY, true",
    "TUESDAY, 2, THURSDAY, true",
    "WEDNESDAY, 9, FRIDAY, true",
    "WEDNESDAY, 9, SATURDAY, false"
  })
  void testCsvSourceEnum(DayOfWeek in, int days, DayOfWeek out, boolean truth) {
    if (truth) {
      assertThat(in.plus(days), is(out));
    } else {
      assertThat(in.plus(days), not(out));
    }
  }

  /**
   * A method to test.
   *
   * @param in An input string
   * @return The SHA-1 digest of that string
   * @throws NoSuchAlgorithmException If the JVM can't construct an SHA-1 message digest.
   */
  public static String getSha1Digest(String in) throws NoSuchAlgorithmException {
    MessageDigest dig = MessageDigest.getInstance("SHA-1");
    dig.update(in.getBytes(StandardCharsets.UTF_8));
    return new BigInteger(1, dig.digest()).toString(16);
  }

  @ParameterizedTest
  @CsvSource({
    "apple,  d0be2dc421be4fcd0172e5afceea3970e2f3d940",
    "banana, 250e77f12a5ab6972a0895d290c4792f0a326ea8",
    "carrot, d8bfad4b74d554312313bd842f4d05364c1ffadd",
    "dog,    e49512524f47b4138d850c9d9d85972927281da0"
  })
  void testCsvSourceSHA1(String in, String out) throws NoSuchAlgorithmException {
    assertThat(getSha1Digest(in), is(out));
  }

  // If the method isn't named, use the same as the test name.
  @ParameterizedTest
  @MethodSource
  void testMethodSource(String in) {
    assertThat(in, is(blankOrNullString()));
  }

  private static Stream<String> testMethodSource() {
    return Stream.of(null, "", "\n", "\t  ");
  }

  @ParameterizedTest
  @MethodSource("getSha1Examples")
  void testMethodSourceByName(String in, String out) throws NoSuchAlgorithmException {
    assertThat(getSha1Digest(in), is(out));
  }

  private static Stream<Arguments> getSha1Examples() {
    return Stream.of(
        Arguments.of("apple", "d0be2dc421be4fcd0172e5afceea3970e2f3d940"),
        Arguments.of("banana", "250e77f12a5ab6972a0895d290c4792f0a326ea8"),
        Arguments.of("carrot", "d8bfad4b74d554312313bd842f4d05364c1ffadd"),
        Arguments.of("dog", "e49512524f47b4138d850c9d9d85972927281da0"));
  }

  @ParameterizedTest(name = "{displayName}-at-{index}")
  @MethodSource("getSha1Examples")
  void testMethodSourceWithDisplayName(String in, String out) throws NoSuchAlgorithmException {
    assertThat(getSha1Digest(in), is(out));
  }

  @ParameterizedTest(name = "Check SHA({0}) == {1}")
  @MethodSource("getSha1Examples")
  void testMethodSourceWithArgumentsName(String in, String out) throws NoSuchAlgorithmException {
    assertThat(getSha1Digest(in), is(out));
  }

  @ParameterizedTest
  @MethodSource("getSha1ExamplesByCollectionOfArguments")
  void testMethodSourceByCollectionOfArguments(String in, String out)
      throws NoSuchAlgorithmException {
    assertThat(getSha1Digest(in), is(out));
  }

  private static Collection<Arguments> getSha1ExamplesByCollectionOfArguments() {
    return Arrays.asList(
        Arguments.of("apple", "d0be2dc421be4fcd0172e5afceea3970e2f3d940"),
        Arguments.of("banana", "250e77f12a5ab6972a0895d290c4792f0a326ea8"),
        Arguments.of("carrot", "d8bfad4b74d554312313bd842f4d05364c1ffadd"),
        Arguments.of("dog", "e49512524f47b4138d850c9d9d85972927281da0"));
  }
}
