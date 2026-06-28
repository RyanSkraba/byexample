Tools by example
==============================================================================

You used to be able to script with Scala using a library called Ammonite.
It was fine, but not great.
When it was deprecated, I still wanted to use some of the tools I created.
This project packages them as an uberjar.

| Script                | Description                                                                                        |
|-----------------------|----------------------------------------------------------------------------------------------------|
| [FileRenamerGo]       | Utility tasks for moving and renaming files                                                        |
| [GettingThingsDoneGo] | Interacting with Getting Things Done files                                                         |

Running the driver
------------------------------------------------------------------------------

This project includes a example executables like [FileRenamerGo](src/main/scala/com/skraba/byexample/scalatools/FileRenamerGo.scala)

```bash
mvn package
# Using the uber jar from the command line
alias file_renamer="java -classpath $(find ~+ -name tools-by-example-*.jar) com.skraba.byexample.scalatools.filerenamer.FileRenamerGo" 
file_renamer --help

# Copy files from the mounted phone directory to the local hard disk
file_renamer cameraphone 
```
