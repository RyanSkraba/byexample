Ammonite by example
==============================================================================

[Ammonite] is a pretty great tool for running scripts written in Scala.  This project includes some
examples.

* You need to install the `amm` executable following the instructions on the website.
* [IntelliJ](https://www.jetbrains.com/help/idea/work-with-scala-worksheet-and-ammonite.html) has
  some support for ammonite scripts, located in the [src/test/resources](src/test/resources) and
  [src/main/resources](src/main/resources) directories.
* The [pom.xml](pom.xml) needs to have some test dependencies added for them to be visible in the
  project.

| Script                   | Description                                                                                         |
|--------------------------|-----------------------------------------------------------------------------------------------------|
| [ammonite_example.sc]    | Various examples of using ammonite, including REST API calls, system calls, arguments parsing, etc. |
| [file_renamer.sc]        | Utility tasks for moving and renaming files                                                         |
| [getting_things_done.sc] | Interacting with Getting Things Done filesr                                                         |

```
# Install this jar into the local repository so we can use its classes in the scripts.
# This also runs unit tests.
mvn install

# Run the script directly from the resources directory.  You can make changes and re-run
# without rebuilding with maven.
src/main/resources/getting_things_done.sc help
src/main/resources/file_renamer.sc help
src/test/resources/ammonite_example.sc help
```

The script can be unit-tested using scalatest, as demonstrated by [AmmoniteSpec], which tests [ammonite_example.sc].
The unit test needs to compile the script, which can take more than 10 seconds. You can set the `AMMONITESPEC_HOME`
environment variable to anything non-empty to create and reuse compiled scripts, which can speed up performance on 
subsequent runs.

[Ammonite]: https://ammonite.io/
[AmmoniteSpec]: src/test/scala/com/skraba/byexample/scala/ammonite/AmmoniteSpec.scala
[ammonite_example.sc]: src/test/resources/ammonite_example.sc
[file_renamer.sc]: src/main/resources/file_renamer.sc
[getting_things_done.sc]: src/main/resources/getting_things_done.sc
