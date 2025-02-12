Webapp by example
==============================================================================

This project is a basic web application (WAR) in Java.

* A [static HTML][resource-static-html] file,
* Dynamic HTML served by a [Jakarta Servlet][resource-servlet],
* Dynamic HTML served by a [Jakarta Server Pages][resource-jsp], and
* a simple, static [TXT][resource-static-txt] file.

It uses Maven's [`<packaging>war</packaging>`][maven-war-plugin] to create a WAR file, which requires an application server to run.

The web application does not depend on any specific web or application server.  The unit tests and simplified deployment for testing via Maven use [Jetty].

[resource-static-html]: src/main/webapp/index.html
[resource-servlet]: src/main/java/com/skraba/byexample/webapp/hello/HelloServlet.java
[resource-jsp]: src/main/webapp/jsp/hello.jsp
[resource-static-txt]: src/main/webapp/hello.txt
[maven-war-plugin]: https://maven.apache.org/plugins/maven-war-plugin/usage.html
[Jetty]: https://www.eclipse.org/jetty/

Resources
------------------------------------------------------------------------------

* [Deploy a Java web application inside a Tomcat server container](https://www.jetbrains.com/help/idea/deploying-a-web-app-into-an-app-server-container.html)

* [Jetty and Maven](https://www.eclipse.org/jetty/documentation/jetty-12/programming-guide/index.html#maven-and-jetty).

Running the web application
------------------------------------------------------------------------------

```bash
mvn package
# Use jetty to serve the application
mvn jetty:run
# Go to http://0.0.0.0:8080/ to see the application
```

Future work
------------------------------------------------------------------------------

* Unit test static TXT
* Unit test JSP
