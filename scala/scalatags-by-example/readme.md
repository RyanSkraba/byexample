Scalatags by example
====================

Library for creating HTML/XML/SVG documents.

Resources
---------

* Source: <https://github.com/lihaoyi/scalatags>
* Docs: <https://www.lihaoyi.com/scalatags/>

Running the driver
------------------

This project includes an [example executable](src/main/scala/com/skraba/byexample/scalatags/ScalatagsGo.scala)
that does a simple hello world mixing Java and Scala code.

```bash
mvn package
# Using the uber jar
alias ScalatagsGo='java -jar '$(pwd)'/target/scalatags-by-example-*-SNAPSHOT.jar'
ScalatagsGo --help
# Using the maven-generated classpath
mvn exec:java -Dexec.args="--help"
mvn exec:java -Dexec.mainClass="com.skraba.byexample.scalatags.ScalatagsGo" -Dexec.args="--help" 
```
