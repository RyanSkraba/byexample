package com.skraba.byexample.jmh;

import java.util.HashMap;
import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;

/**
 * Which is faster: accessing an element in a HashMap or array?
 *
 * <p>They are both O(1) operations, but we'd expect the array lookup to be faster. Is it? And by
 * how much?
 *
 * <p>Using this benchmark on one machine and JDK, it looks like the array lookup is twice as fast
 * as the hash lookup.
 *
 * <pre>
 * JmhGo -f 3 -wi 2 -i 10 -bm AverageTime -tu ns HashBenchmark
 *  ...
 * Benchmark            Mode  Cnt   Score   Error  Units
 * HashBenchmark.array  avgt   30  30.497 ± 1.049  ns/op
 * HashBenchmark.hash   avgt   30  75.375 ± 1.354  ns/op
 * </pre>
 */
public class HashBenchmark {

  /**
   * A state object is created and shared among the benchmark methods automatically. It just needs
   * to be passed in as an argument to the method.
   */
  @State(Scope.Benchmark)
  public static class HashLookup {

    volatile HashMap<String, String> map = new HashMap<>();

    public HashLookup() {
      map.put("zero", "zero");
      map.put("one", "un");
      map.put("two", "deux");
      map.put("three", "trois");
      map.put("four", "quatre");
      map.put("five", "cinq");
      map.put("six", "six");
      map.put("seven", "sept");
      map.put("eight", "huit");
      map.put("nine", "neuf");
    }

    public String get(String name) {
      return map.get(name);
    }
  }

  @State(Scope.Benchmark)
  public static class ArrayLookup {

    volatile String[] values = {
      "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten"
    };

    public String get(int index) {
      return values[index];
    }
  }

  @Benchmark
  public void hash(HashLookup hash, Blackhole bh) {
    bh.consume(hash.get("zero"));
    bh.consume(hash.get("one"));
    bh.consume(hash.get("two"));
    bh.consume(hash.get("three"));
    bh.consume(hash.get("four"));
    bh.consume(hash.get("five"));
    bh.consume(hash.get("six"));
    bh.consume(hash.get("seven"));
    bh.consume(hash.get("eight"));
    bh.consume(hash.get("nine"));
  }

  @Benchmark
  public void array(ArrayLookup array, Blackhole bh) {
    bh.consume(array.get(0));
    bh.consume(array.get(1));
    bh.consume(array.get(2));
    bh.consume(array.get(3));
    bh.consume(array.get(4));
    bh.consume(array.get(5));
    bh.consume(array.get(6));
    bh.consume(array.get(7));
    bh.consume(array.get(8));
    bh.consume(array.get(9));
  }
}
