FROM influxdb:1.7.4

RUN apt-get update && apt-get install -y --no-install-recommends \
  python-pip=9.0.1-2+deb9u2 \
	python-all-dev=2.7.13-2 \
	python-setuptools=33.1.1-1 \
	gcc=4:6.3.0-4 \
	perl=5.24.1-3+deb9u7 \
	&& rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache influxdb==5.2.1 blist==1.3.6

WORKDIR /opt/flamegraph

# RUN wget https://raw.githubusercontent.com/etsy/statsd-jvm-profiler/30f22f182ba496acff394dab6225897450926196/visualization/influxdb_dump.py
RUN wget https://raw.githubusercontent.com/aviemzur/statsd-jvm-profiler/3b8202d6a63bcb056c25361a02b7186175fa8799/visualization/influxdb_dump.py
RUN wget https://raw.githubusercontent.com/brendangregg/FlameGraph/1b1c6deede9c33c5134c920bdb7a44cc5528e9a7/flamegraph.pl
RUN wget https://repo1.maven.org/maven2/com/etsy/statsd-jvm-profiler/2.1.0/statsd-jvm-profiler-2.1.0.jar
RUN wget https://repo1.maven.org/maven2/com/etsy/statsd-jvm-profiler/2.1.0/statsd-jvm-profiler-2.1.0-jar-with-dependencies.jar

COPY go.sh go.sh
