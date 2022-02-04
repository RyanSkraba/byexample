package com.skraba.byexample.jmh;

import java.math.BigInteger;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.OperationsPerInvocation;
import org.openjdk.jmh.infra.Blackhole;

/**
 * Benchmarks on calculating the maximum decimal precision that can be held in a number of bytes.
 *
 * <p>In other words, we know that one byte can hold all 2-digit positive and negative number, but
 * not all 3-digit decimal numbers. How many decimal digits can a 100 byte buffer fully represent?
 * This comes up in the issue https://issues.apache.org/jira/browse/AVRO-2620.
 *
 * <p>There are a number of bit-fiddling and math techniques that can calculate the answer. This
 * benchmark attempts to answer which one is the most efficient.
 *
 * <pre>
 * byexample_go_jmh -f 3 -wi 2 -i 5 -bm AverageTime -tu ns MaxPrecisionBenchmark
 *   ...
 * Benchmark                                            Mode  Cnt      Score      Error  Units
 * MaxPrecisionBenchmark.maxPrecisionBigInteger         avgt   15  59841.643 ± 3907.767  ns/op
 * MaxPrecisionBenchmark.maxPrecisionLogApprox40bits    avgt   15      3.245 ±    0.391  ns/op
 * MaxPrecisionBenchmark.maxPrecisionLogPowerRuleCast   avgt   15      3.024 ±    0.111  ns/op
 * MaxPrecisionBenchmark.maxPrecisionLogPowerRuleRound  avgt   15      7.441 ±    0.189  ns/op
 * MaxPrecisionBenchmark.maxPrecisionOriginalAvro       avgt   15     25.112 ±    0.374  ns/op
 * </pre>
 */
public class MaxPrecisionBenchmark {

  private static final double LOGTWO = Math.log10(2);

  public static final long NUMERATOR_40_BITS = getApproxNumerator(40);

  /**
   * The original Avro implementation is a straight translation of getting the log10 of the largest
   * positive magnitude possible with the twos complement binary number that can fit in the buffer.
   *
   * @param size the number of bytes available to express the number.
   * @return the number of decimal digits that can fit in the bytes.
   */
  public static long getMaxPrecisionOriginalAvro(int size) {
    // Starts failing at size >= 309
    return Math.round(Math.floor(Math.log10(Math.pow(2, 8L * size - 1) - 1)));
  }

  /**
   * Improvement on the straight translation using the log power rule to avoid double overflow.
   *
   * @param size the number of bytes available to express the number.
   * @return the number of decimal digits that can fit in the bytes. The fractional part must be
   *     discarded.
   */
  public static double getMaxPrecisionLogPowerRule(int size) {
    return (8L * size - 1) * LOGTWO;
  }

  /**
   * Calculates the long constant for {@link #getMaxPrecisionWithApprox} for reuse.
   *
   * @param shift The denominator to use expressed as a shift (i.e. {@code 2 ^ shift})
   * @return a good approximation of {@code Math.log(2)} when divided by the denominator.
   */
  public static long getApproxNumerator(int shift) {
    long denominator = 1L << shift;
    double approx = LOGTWO * denominator;
    return Math.round(approx);
  }

  /**
   * Use a numeric approximation for the calculation without using any double arithmetic. This is
   * equivalent to {@link #getMaxPrecisionLogPowerRule(int)} with the {@code Math.log(2)} converted
   * to a long multiplication and division.
   *
   * @param size the number of bytes available to express the number.
   * @param numerator When divided by {@code 2 ^ shift}, returns a good approximation of {@code
   *     Math.log(2)}. This can be calculated once by {@code #getApproxNumerator}.
   * @param shift The denominator to use expressed as a shift (i.e. {@code 2 ^ shift})
   * @return the number of decimal digits that can fit in the bytes.
   */
  public static long getMaxPrecisionWithApprox(long size, long numerator, int shift) {
    return (8L * size - 1) * numerator >> shift;
  }

  /**
   * Use {@link BigInteger#toString()} to calculate the precision. This is especially slow but uses
   * the numerical methods in the Java libraries.
   *
   * @param size the number of bytes available to express the number.
   * @return the number of decimal digits that can fit in the bytes.
   */
  public static long getMaxPrecisionWithBigInteger(int size) {
    return BigInteger.valueOf(1).shiftLeft(8 * size - 1).toString().length() - 1;
  }

  @OperationsPerInvocation(1024 * 1024)
  @Benchmark
  public void maxPrecisionOriginalAvro(Blackhole bh) {
    // Inaccurate after 310 digits.
    for (int size = 0; size < 1024 * 1024; size++) {
      bh.consume(getMaxPrecisionOriginalAvro(size));
    }
  }

  @OperationsPerInvocation(1024 * 1024)
  @Benchmark
  public void maxPrecisionLogPowerRuleRound(Blackhole bh) {
    for (int size = 0; size < 1024 * 1024; size++) {
      bh.consume(Math.round(Math.floor(getMaxPrecisionLogPowerRule(size))));
    }
  }

  @OperationsPerInvocation(1024 * 1024)
  @Benchmark
  public void maxPrecisionLogPowerRuleCast(Blackhole bh) {
    for (int size = 0; size < 1024 * 1024; size++) {
      bh.consume((long) getMaxPrecisionLogPowerRule(size));
    }
  }

  @OperationsPerInvocation(1024 * 1024)
  @Benchmark
  public void maxPrecisionLogApprox40bits(Blackhole bh) {
    for (int size = 0; size < 1024 * 1024; size++)
      bh.consume(getMaxPrecisionWithApprox(size, NUMERATOR_40_BITS, 40));
  }

  @OperationsPerInvocation(1024)
  @Benchmark
  public void maxPrecisionBigInteger(Blackhole bh) {
    // Going to 1 million takes excessively long.
    for (int size = 0; size < 1024; size++) {
      bh.consume(getMaxPrecisionWithBigInteger(size));
    }
  }
}
