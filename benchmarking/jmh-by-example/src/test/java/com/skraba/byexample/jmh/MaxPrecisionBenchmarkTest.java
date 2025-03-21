package com.skraba.byexample.jmh;

import static com.skraba.byexample.jmh.MaxPrecisionBenchmark.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import ch.obermuhlner.math.big.BigDecimalMath;
import java.math.BigDecimal;
import java.math.MathContext;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

/** Some information about the accuracy of the methods in {@link MaxPrecisionBenchmark}. */
public class MaxPrecisionBenchmarkTest {

  /** Runs over different methods to check that they provide accurate results. */
  @Disabled("Long test takes about a minute to run.")
  @Test
  public void testForAccurateResults() {

    int knownPowerRuleSuccess = 382910;
    int size = 1; // knownPowerRuleSuccess

    // The size of the byte buffer where each method fails.
    Integer failAvro = null; // Fails at 129 bytes / 310 digits
    Integer failApprox = null; // Fails at 1404824 bytes / 3383152 digits
    // OK with BigInteger test up to 382910 bytes / 922138 digits
    Integer failPowerRule = null; // Fails at 122202250 bytes / 294292341 digits

    // Print results every 10 seconds.
    long startTime = System.currentTimeMillis();
    long nextTime = startTime + 10000;

    MathContext mathContext = new MathContext(100);
    BigDecimal log2 = BigDecimalMath.log10(BigDecimal.valueOf(2), mathContext);

    // Continue while at least one method is still succeeding.
    while (failPowerRule == null || failApprox == null || failAvro == null) {

      // Get the expected length using the fastest method available.
      long truth = 0L;
      if (size < knownPowerRuleSuccess) {
        // This has been tested to be accurate up to around this point.
        truth = (long) getMaxPrecisionLogPowerRule(size);
      } else {
        truth = log2.multiply(new BigDecimal(8L * size - 1)).longValue();
      }

      // Detect when a method fails.
      if (failAvro == null && truth != getMaxPrecisionOriginalAvro(size)) {
        System.out.println("originalAvro returns wrong value at " + size + ":" + truth);
        failAvro = size;
      }
      if (failApprox == null && truth != getMaxPrecisionWithApprox(size, NUMERATOR_40_BITS, 40)) {
        System.out.println("approx40 returns wrong value at " + size + ":" + truth);
        failApprox = size;
      }
      if (failPowerRule == null && truth != (long) getMaxPrecisionLogPowerRule(size)) {
        System.out.println("logPowerRule returns wrong value at " + size + ":" + truth);
        failPowerRule = size;
      }

      // About every second, print out a status.
      if (size % 10 == 0) {
        long now = System.currentTimeMillis();
        if (now > nextTime) {
          System.out.println(
              "size="
                  + size
                  + ":"
                  + truth
                  + " failAvro="
                  + failAvro
                  + " failApprox="
                  + failApprox
                  + " failPowerRule="
                  + failPowerRule);
          while (nextTime < now) nextTime += 1000;
        }
      }

      size += 1;
    }

    assertThat(failAvro, is(129));
    assertThat(failApprox, is(1_404_824));
    assertThat(failPowerRule, is(122_202_250));
  }
}
