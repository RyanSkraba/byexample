FROM influxdb:2.7.1

RUN apt-get update && apt-get install -y --no-install-recommends \
	python-all-dev=2.7.18-3 \
	python3-all-dev \
	python3-pip=20.3.4-4+deb11u1 \
	python-setuptools=44.1.1-1 \
	gcc=4:10.2.1-1 \
	perl=5.32.1-4+deb11u2 \
	&& rm -rf /var/lib/apt/lists/*

RUN pip install --no-cache influxdb==5.3.1 blist==1.3.6

WORKDIR /opt/flamegraph

# TODO: These are unlikely to work with the versions of python above...
RUN curl -O https://raw.githubusercontent.com/etsy/statsd-jvm-profiler/30f22f182ba496acff394dab6225897450926196/visualization/influxdb_dump.py
RUN curl -O https://raw.githubusercontent.com/aviemzur/statsd-jvm-profiler/3b8202d6a63bcb056c25361a02b7186175fa8799/visualization/influxdb_dump.py
RUN curl -O https://raw.githubusercontent.com/brendangregg/FlameGraph/1b1c6deede9c33c5134c920bdb7a44cc5528e9a7/flamegraph.pl
RUN curl -O https://repo1.maven.org/maven2/com/etsy/statsd-jvm-profiler/2.1.0/statsd-jvm-profiler-2.1.0.jar
RUN curl -O https://repo1.maven.org/maven2/com/etsy/statsd-jvm-profiler/2.1.0/statsd-jvm-profiler-2.1.0-jar-with-dependencies.jar

COPY go.sh go.sh
