JMH by example
==============================================================================

JHM is a micro-benchmarking tool, included as part of the openjdk project.

Resources
------------------------------------------------------------------------------

* [Home page](http://openjdk.java.net/projects/code-tools/jmh/)
* [JMH Samples](http://hg.openjdk.java.net/code-tools/jmh/file/tip/jmh-samples/src/main/java/org/openjdk/jmh/samples/): The best place to read, understand and learn what is happening.
* Tutorials
    - (http://tutorials.jenkov.com/java-performance/jmh.html)
* [Online result visualizer](https://jmh.morethan.io) ([jzillmann/jmh-visualizer](https://github.com/jzillmann/jmh-visualizer)) Very pretty UI for visualizing jmh reports.

JMH Options (`JmhGo -h`)
------------------------------------------------------------------------------

Option       | Explanation 
---          | --- 
`-bm <mode>` | **Benchmark mode** (default: `Throughput`): <ul><li>`Throughput`/`thrpt`</li><li>`AverageTime`/`avgt`</li><li>`SampleTime`/`sample`</li><li>`SingleShotTime`/`ss`</li><li>`All`/`all`.</li></ul>
`-tu <TU>`   | **Time units** (default: `s`): `m`, `s`, `ms`, `us`, `ns`

Running the benchmark
------------------------------------------------------------------------------

The easiest way to run a JMH benchmark is to create an uber-jar with the JMH driver included.

```bash
mvn package
# Using the uber jar
alias JmhGo='java -jar '$(pwd)'/target/jmh-by-example-*-SNAPSHOT.jar'
# Get help on the tool.
JmhGo --h

# Sanity check on the hash benchmarks
# benchmark mode: single shot
# time unit: nanoseconds
JmhGo -bm ss -tu ns HashBenchmark.hash

# Run the whole benchmark in three processes, with 4 warmups and 5 iterations
JmhGo -f 3 -wi 4 -i 5 -bm Throughput,AverageTime -tu ns

# Test one of the benchmark methods and generate a json report
# This creates a jmh-result.json file that you can drop in the online visualiser.
JmhGo -f 1 -wi 0 -i 1 -tu ns MaxPrecisionBenchmark -rf json
```
