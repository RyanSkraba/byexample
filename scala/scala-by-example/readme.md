Scala by example
==============================================================================

There's a LOT to the Scala programming language.  This project includes some common and useful
libraries and techniques.

| How do I...                                      | Examples                                                                                          |
|--------------------------------------------------|---------------------------------------------------------------------------------------------------|
| compile and run scala code dynamically _(eval)_? | [com.skraba.byexample.scala.dynamic](src/test/scala/com/skraba/byexample/scala/dynamic/)          |
| use self-types versus traits?                    | [TraitSelfTypeSpec](src/test/scala/com/skraba/byexample/scala/TraitSelfTypeSpec.scala)            |
| use Regex?                                       | [Markd](src/main/scala/com/skraba/byexample/scala/markd/MarkdGo.scala) (A simple markdown parser) |

Some of the code samples were taken or adapted into unit tests from other learning resources.  All
of these are highly recommended!

| Source                                                                                                 | Examples                                                                                         |
|--------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------|
| [Official Tour of Scala](https://docs.scala-lang.org/tour/tour-of-scala.html)                          | [com.skraba.byexample.scala.tour](src/test/scala/com/skraba/byexample/scala/tour/)               |
| [Scala collections overview](https://docs.scala-lang.org/overviews/collections-2.13/introduction.html) | [com.skraba.byexample.scala.collections](src/test/scala/com/skraba/byexample/scala/collections/) |
| [ScalaTest overview](https://www.scalatest.org/user_guide)                                             | [com.skraba.byexample.scala.scalatest](src/test/scala/com/skraba/byexample/scala/scalatest/)     |

Resources
------------------------------------------------------------------------------

Other useful resources to learn more about Scala and functional programming:

* [Jargon help](https://github.com/hemanth/functional-programming-jargon)
* [Cheat sheet](https://docs.scala-lang.org/cheatsheets/)
* [Functional Programming for Mortals](https://leanpub.com/fpmortals)

Running the driver
------------------------------------------------------------------------------

This project includes an [example executable](src/main/java/com/skraba/byexample/scala/JavaScalaGo.java)
that does a simple hello world mixing Java and Scala code.

```bash
mvn package
# Using the uber jar from the command line
alias byexample_go_scala="java -jar $(find ~+ -name scala-by-example-*.jar)"
byexample_go_scala --help
# Using the maven-generated classpath
mvn exec:java -Dexec.args="--name=World --count=7"
mvn exec:java -Dexec.mainClass="com.skraba.byexample.scala.ScalaGo" -Dexec.args="--name=world --count=7" 

# MarkdGo is another internal application to this project.

alias byexample_go_markd="java -classpath $(find ~+ -name scala-by-example-*.jar) com.skraba.byexample.scala.markd.MarkdGo"
byexample_go_markd beautify readme.md
byexample_go_markd beautify $(find . -name \*.md)
```

By default, all of the slow tests (tagged with `org.scalatest.tags.Slow`) are excluded, but you can run them using the command line:

```bash
mvn package -Dplugin.maven.scalatest.exclude=
```

In an IDE like IntelliJ, set the **Program arguments** in the Run/Debug Configuration to `-l org.scalatest.tags.Slow` to exclude slow tests.
