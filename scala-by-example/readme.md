Scala by example
================

There's a LOT to the Scala programming language.  This project includes some common and useful 
libraries and techniques.

How do I... | Examples 
---         | ---
compile and run scala code dynamically _(eval)_?| [com.skraba.byexample.scala.dynamic](src/test/scala/com/skraba/byexample/scala/dynamic/)
use self-types versus traits?| [TraitSelfTypeSpec](src/test/scala/com/skraba/byexample/scala/TraitSelfTypeSpec.scala)

Resources
---------

Other useful resources to learn more about Scala and functional programming: 

* [Jargon help](https://github.com/hemanth/functional-programming-jargon)
* [Cheat sheet](https://docs.scala-lang.org/cheatsheets/)
* [Functional Programming for Mortals](https://leanpub.com/fpmortals)

Running the launcher
--------------------

This project includes an [example executable](src/main/java/com/skraba/byexample/scala/JavaScalaGo.java)
that does a simple hello world mixing Java and Scala code.

```bash
mvn package
# Using the fat jar
alias ScalaGo='java -jar '$(pwd)'/target/scala-by-example-*-SNAPSHOT.jar'
ScalaGo --help
# Using the maven-generated classpath
mvn exec:java -Dexec.args="--name=World --count=7"
mvn exec:java -Dexec.mainClass="com.skraba.byexample.scala.ScalaGo" -Dexec.args="--name=world --count=7" 
```
