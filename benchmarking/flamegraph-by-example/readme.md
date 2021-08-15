Flamegraph by example
==============================================================================

Flamegraphs are a useful way to look at how an application is spending its time, by sampling across
all threads periodically and aggregating the results.

They have the following characteristics:
* Horizontal order doesn't have any meaning, but horizontal width corresponds to the number of
  *samples* that were spent in the method call.
* Vertical height is the depth of the call stack (and order is relevant).

Building this project will create an image `flamegraph-influxdb:1.7.4` (based on the influxdb image)
with a couple of useful scripts and some code gluing them together for easy deployment.  There's
also an java application used in the examples below.

Resources
------------------------------------------------------------------------------

* [An introduction to Flame Graphs](http://www.brendangregg.com/flamegraphs.html)
* Code included in the image:
    * [SVG image generator](https://github.com/brendangregg/FlameGraph/) ([CDDL-1.0](https://github.com/brendangregg/FlameGraph/blob/master/docs/cddl1.txt))
    * [statsd InfluxDB adapter](https://github.com/etsy/statsd-jvm-profiler) ([MIT](https://github.com/etsy/statsd-jvm-profiler/blob/master/LICENSE))
    * [InfluxDB](https://hub.docker.com/_/influxdb) ([MIT](https://github.com/influxdata/influxdb/blob/master/LICENSE))
* The glue code was heavily inspired from <https://medium.com/paypal-tech/spark-in-flames-profiling-spark-applications-using-flame-graphs-71e77b26516e/>
* The docker build uses the plugin from [Fabric8](https://dmp.fabric8.io/)

Running the launcher
------------------------------------------------------------------------------

```bash
# Either of the commands build the image.
mvn package -Ddocker.verbose=true
docker build -t flamegraph-influxdb:1.7.4 src/main/docker/

# Run the server in one process
docker run --rm \
    --name influxdb \
    -p 8086:8086 \
    -e INFLUXDB_DB=profiler \
    flamegraph-influxdb:1.7.4

# If necessary, get the profiler java agent out of the image.  
JAVAAGENT_DIR=/tmp/javaagent
mkdir -p $JAVAAGENT_DIR
docker cp influxdb:/opt/flamegraph/statsd-jvm-profiler-2.1.0-jar-with-dependencies.jar $JAVAAGENT_DIR/

# If the process to bench is in another container, copy the java agent there.
DKR=container-to-profile
docker cp /tmp/statsd-jvm-profiler-2.1.0-jar-with-dependencies.jar $DKR:$JAVAAGENT_DIR

# Run the application to profile, with the javaagent below.
TAG_MAPPING=org.application
PREFIX=org.Application
INFLUX_HOST=$(docker inspect -f {{.NetworkSettings.IPAddress}} influxdb)
java -javaagent:$JAVAAGENT_DIR/statsd-jvm-profiler-2.1.0-jar-with-dependencies.jar=server=$INFLUX_HOST,port=8086,reporter=InfluxDBReporter,database=profiler,username=profiler,password=profiler,prefix=$PREFIX,tagMapping=$TAG_MAPPING -jar $(pwd)/target/flamegraph-by-example-*-SNAPSHOT.jar sieve 1000000 --super --happy --sexy --count
# This should take a few minutes, use a smaller number for a shorter example.

# Generate the flame graph.
****docker exec influxdb sh go.sh $TAG_MAPPING $PREFIX > /tmp/flamegraph.svg****
```

### The Java Agent

Uses the `$JAVA_AGENTDIR` to locate the jar, and `$TAG_MAPPING`, `$PREFIX` to uniquely identify the
samples.

```
-javaagent:$JAVAAGENT_DIR/statsd-jvm-profiler-2.1.0-jar-with-dependencies.jar=server=$INFLUX_HOST,port=8086,reporter=InfluxDBReporter,database=profiler,username=profiler,password=profiler,prefix=$PREFIX,tagMapping=$TAG_MAPPING
```

Future work
------------------------------------------------------------------------------

* Update the versions.
