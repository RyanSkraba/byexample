Ammonite by example
==============================================================================

[Ammonite] is a pretty great tool for running scripts written in Scala.  This project includes some
examples.

* You need to install the `amm` executable following the instructions on the website.
* [IntelliJ](https://www.jetbrains.com/help/idea/work-with-scala-worksheet-and-ammonite.html) has
  some support for ammonite scripts, located in the [src/test/resources](src/test/resources) 
  directory.
* The [pom.xml](pom.xml) needs to have some test dependencies added for them to be visible in the
  project.

```
# Install this jar into the local repository so we can use its classes in the scripts.
# This only needs to be done when something changes in src/main !
mvn install

# Run the script directly from the resources directory.  You can make changes and re-run
# without rebuilding with maven.
src/test/resources/getting_things_done.sc help
src/test/resources/file_renamer.sc help
src/test/resources/ammonite_example.sc help
```

[Ammonite]: https://ammonite.io/