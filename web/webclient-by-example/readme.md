WebClient by example
==============================================================================

A command line utility for making HTTP calls with different client libaries.

Running the driver
------------------------------------------------------------------------------

This project includes an [example executable](src/main/scala/com/skraba/byexample/webclient/WebClientGo.scala) that can perform some tasks serving pages.

```bash
# Build and create an alias for using the uber jar.
mvn package
alias byexample_go_webclient="java -jar $(find ~+ -name webclient-by-example-*.jar | sort | head -n1)"
byexample_go_webclient --help

# Fetch an URI with the method GET
byexample_go_webclient get http://google.com/
```