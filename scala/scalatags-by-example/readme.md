Scalatags by example
==============================================================================

Library for creating HTML/XML/SVG documents.

Resources
------------------------------------------------------------------------------

* Source: <https://github.com/lihaoyi/scalatags>
* Docs: <https://www.lihaoyi.com/scalatags/>

SVG Resources
------------------------------------------------------------------------------

* Describing paths: https://www.nan.fyi/svg-paths/

Running the driver
------------------------------------------------------------------------------

This project includes an [example executable](src/main/scala/com/skraba/byexample/scalatags/ScalatagsGo.scala)
that can be used to demonstrate some SVG generation tools.

```bash
mvn package
# Using the uber jar from the command line
alias byexample_go_scalatags="java -jar $(find ~+ -name scalatags-by-example-*.jar)"
byexample_go_scalatags --help

# Dump Duolingo Chinese section 3 vocabulary into a cheatsheet.
byexample_go_scalatags cheatsheet --section 3 > /tmp/section3.svg 
```

Future work (TODO)
==============================================================================

Unit tests for `cheatsheet` make web calls to fetch duolingo vocabulary.
