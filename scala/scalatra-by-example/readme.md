Scalatra by example
==============================================================================

A command line utility for running a scalatra server

Running the driver
------------------------------------------------------------------------------

This project includes an [example executable](src/main/java/com/skraba/byexample/scala/markd/MarkdGo.java) that can perform some tasks serving pages.

```bash
mvn package
# Using the uber jar from the command line
alias byexample_go_scalatra="java -jar $(find ~+ -name scalatra-by-example-*.jar | sort | head -n1)"
byexample_go_scalatra --help
byexample_go_scalatra server
# Using the maven-generated classpath
mvn exec:java -Dexec.args="server"
mvn exec:java -Dexec.mainClass="com.skraba.byexample.scalatra.ScalatraGo" -Dexec.args="server" 
```