package com.skraba.byexample.benchmarking.toprofile;

import com.tinfoiled.docopt4s.shaded.docoptjava.Docopt;
import com.tinfoiled.docopt4s.shaded.docoptjava.DocoptExitException;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Consumer;

/**
 * Counts and prints prime numbers.
 *
 * <p>Not very efficient or interesting, just an example that can be benchmarked.
 *
 * <p>Prime numbers can have super or sexy characteristics! Numbers are fun.
 */
public class ToProfileGo {

  public static final String VERSION = "0.0.1-SNAPSHOT";

  public static final String DOC =
      String.join(
          "\n",
          "Runs some tasks that can be profiled.",
          "",
          "Usage:",
          "  ToProfileGo sieve <max> [--super] [--happy] [--sexy] [--print] [--count]",
          "",
          "Options:",
          "  -h --help   Show this screen.",
          "  --print     Print the primes to standard out.",
          "  --count     Print the count to standard out.",
          "  --super     Only accept super primes.",
          "  --happy     Only accept happy primes.",
          "  --sexy      Only accept sexy primes.",
          "", //
          "Commands:",
          "   sieve  Use the Sieve of Eratosthenes to find prime numbers.");

  /**
   * Runs the tool. This does not handle any docopt exception automatically while parsing the
   * command line.
   *
   * @param args command-line arguments as described in DOC .
   */
  public static void go(String... args) throws DocoptExitException, InternalDocoptException {
    Map<String, Object> opts = new Docopt(DOC).withVersion(VERSION).withExit(false).parse(args);

    if (Boolean.parseBoolean(String.valueOf(opts.get("sieve")))) {

      final int max = Integer.parseInt(String.valueOf(opts.get("<max>")));
      final boolean onlySuper = Boolean.parseBoolean(String.valueOf(opts.get("--super")));
      final boolean onlyHappy = Boolean.parseBoolean(String.valueOf(opts.get("--happy")));
      final boolean onlySexy = Boolean.parseBoolean(String.valueOf(opts.get("--sexy")));
      final boolean doPrint = Boolean.parseBoolean(String.valueOf(opts.get("--print")));
      final boolean doCount = Boolean.parseBoolean(String.valueOf(opts.get("--count")));
      AtomicInteger count = new AtomicInteger(0);

      sieveOfEratosthenes(
          max,
          onlySuper,
          onlyHappy,
          onlySexy,
          prime -> {
            if (doPrint) {
              System.out.println(prime);
            }
            if (doCount) {
              count.incrementAndGet();
            }
          });
      if (doCount) {
        System.out.println("COUNT: " + count);
      }
    }
  }

  /** Main entry point to the application. */
  public static void main(String[] args) {
    // All of the command is executed in the go method, and this wraps DocOpt and exceptions for
    // console feedback.
    try {
      go(args);
    } catch (DocoptExitException ex) {
      if (ex.getMessage() != null)
        (ex.getExitCode() == 0 ? System.out : System.err).println(ex.getMessage());
      System.exit(ex.getExitCode());
    } catch (InternalDocoptException ex) {
      System.out.println(ex.getDocopt());
      if (ex.getMessage() != null) {
        System.out.println();
        System.out.println(ex.getMessage());
      }
      System.exit(1);
    } catch (Exception ex) {
      System.err.println(DOC);
      System.err.println();
      ex.printStackTrace();
      System.exit(1);
    }
  }

  /**
   * {@link com.tinfoiled.docopt4s.shaded.docoptjava.DocoptExitException} constructors are
   * protected.
   */
  public static class InternalDocoptException extends RuntimeException {

    private final String docopt;

    public InternalDocoptException(String message, Throwable cause, String docopt) {
      super(message, cause);
      this.docopt = docopt;
    }

    public String getDocopt() {
      return docopt;
    }
  }

  /**
   * A number is happy if successively applying a specific transformation does not end in a cycle.
   *
   * <p>The transformation is summing the square of its 10-based digits.
   *
   * @param num The number to check.
   * @return If the candidate is a happy number.
   * @throws IllegalArgumentException if the number is zero or negative.
   */
  public static boolean isHappy(int num) {
    if (num < 1) {
      throw new IllegalArgumentException("The number " + num + "is neither happy or unhappy.");
    }
    // All happy numbers end up as one.
    if (num == 1) {
      return true;
    }
    // All unhappy numbers end up in a sequence containing four.
    if (num == 4) {
      return false;
    }
    int sumSquareDigits = 0;
    while (num > 0) {
      int digit = num % 10;
      sumSquareDigits += digit * digit;
      num /= 10;
    }
    return isHappy(sumSquareDigits);
  }

  /**
   * A not-very-efficient version of the Sieve of Eratosthenes to find primes, sending some of the
   * primes to a consumer.
   *
   * @param max The number to count to.
   * @param onlySuper Only consume primes that are super (have an index that is also prime).
   * @param onlyHappy Only consume primes that are happy.
   * @param onlySexy Only consume primes that are sexy (differ in 6 by another prime).
   * @param f a consumer that is called when a prime is found, after filtering.
   * @return all of the primes found, regardless of which were consumed.
   * @link <a href="https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes">Sieve of Erasthenes</a>
   */
  public static Set<Integer> sieveOfEratosthenes(
      int max, boolean onlySuper, boolean onlyHappy, boolean onlySexy, Consumer<Integer> f) {
    Set<Integer> primes = new TreeSet<>();
    NEXT_CANDIDATE:
    for (int candidate = 2; candidate <= max; candidate++) {
      // The "sieve" is to only check the candidate against the list of known primes (which
      // grows with the number of candidates checked).
      for (int p : primes) {
        if (candidate % p == 0) {
          continue NEXT_CANDIDATE;
        }
      }
      primes.add(candidate);
      if (onlySuper && !primes.contains(primes.size())) {
        continue;
      }
      if (onlyHappy && !isHappy(candidate)) {
        continue;
      }
      if (onlySexy) {
        if (!primes.contains(candidate - 6)) {
          // We may have missed a precedent sexy prime.
          continue;
        }
        // If it is sexy, make sure that we printed out its partner.  Note that this means they
        // might be accepted out of order!
        if (candidate > 6 && !primes.contains(candidate - 12)) {
          f.accept(candidate - 6);
        }
      }
      f.accept(candidate);
    }
    return primes;
  }
}
