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
that can be used to demonstrate some SVG generation tools.

```bash
mvn package
# Using the uber jar
alias byexample_go_scalatags='java -jar '$(pwd)'/target/scalatags-by-example-*-SNAPSHOT.jar'
byexample_go_scalatags --help

# 
```
