Markd by example
==============================================================================

A command line utility for working with Markdown files

Running the driver
------------------------------------------------------------------------------

This project includes an [example executable](src/main/java/com/skraba/byexample/scala/markd/MarkdGo.java) that can perform a variety of tasks on a markdown file.

```bash
mvn package
# Using the uber jar from the command line
alias byexample_go_markd="java -jar $(find ~+ -name markd-by-example-*.jar | sort | head -n1)"
byexample_go_markd --help
# Using the maven-generated classpath
mvn exec:java -Dexec.args="beautify readme.md"
mvn exec:java -Dexec.mainClass="com.skraba.byexample.scala.markd.MarkdGo" -Dexec.args="beautify readme.md" 

# MarkdGo is another internal application to this project.
byexample_go_markd beautify readme.md
byexample_go_markd beautify .
```