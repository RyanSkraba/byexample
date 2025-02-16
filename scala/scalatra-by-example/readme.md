Scalatra by example
==============================================================================

A command line utility for running a scalatra server, especially one embedded
in an uber jar.

Running the driver
------------------------------------------------------------------------------

This project includes an [example executable](src/main/java/com/skraba/byexample/scala/markd/MarkdGo.java) that can perform some tasks serving pages.

```bash
# Build and create an alias for using the uber jar.
mvn package
alias byexample_go_scalatra="java -jar $(find ~+ -name scalatra-by-example-*.jar | sort | head -n1)"
byexample_go_scalatra --help

# Run the helloworld example
byexample_go_scalatra helloworld
# Go to https://localhost:8080/ to see the message
# Go to https://localhost:8080/_shutdown to turn the server off 
```