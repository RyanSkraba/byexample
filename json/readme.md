json
==============================================================================

[JSON](https://www.json.org/) is human-readable and widely adopted.  The specification [ECMA-404] is only four pages!

As a result, there's quite a few Java and Scala toolkits that can read, write and manipulate JSON.

This project demonstrates using some of these toolkits to perform common tasks.

How work with 
------------------------------------------------------------------------------

The [JsonTestBase.java] and [JsonSpecBase.scala] test classes describe some common manipulations that are demonstrated in every toolkit:

* Parse a String/String into a generic JSON object

<!--

* Write a generic JSON object into a string, different ways it can be controlled
* Parse a String/String into a POJO

* Create a JSON object in memory.
  - modifications, add field, remove field, modify field.
* Turn it into a string / parse it from a string.
* Stream it in / stream it out.
* Bind it to a POJO.
-->

The [JsonTestResources.scala] class contains some example JSON texts and helper methods that can be used in both Scala and Java.

[JsonTestBase.java]: json-base-by-example/src/test/java/com/skraba/byexample/json/base/JsonTestBase.java
[JsonSpecBase.scala]: json-base-by-example/src/test/scala/com/skraba/byexample/json/base/JsonSpecBase.scala
[JsonTestResources.scala]: json-base-by-example/src/main/scala/com/skraba/byexample/json/base/JsonTestResources.scala

Toolkits
------------------------------------------------------------------------------

They're all tied for first place.

| Name                      | Notes                                                                                                 |
|---------------------------|-------------------------------------------------------------------------------------------------------|
| [GSON](./gson-by-example) | [🏠 Home][GSON Home] Google's JSON library to convert POJOs into JSON and back.  In maintenance mode. |
| Jackson                   | TODO                                                                                                  |
| ujson                     | TODO                                                                                                  |

<--
* json4s 
* jsonp   
* play-json   
* yaml isn't json but json is yaml
-->

<!--
To do
------------------------------------------------------------------------------

* Create simple JSON data for examples (in string and stream)
  * object with string, number, boolean (and null) values, a subrecord, array of subrecords
* Create complex JSON data for examples (in string and stream)
  * simple data + array of heterogeneous primitives + array of heterogeneous objects, array of array
* Create interface for "how to" (read, write, add field, remove field, modify field)
* Reading and writing JSON line
-->

[GSON Home]: https://github.com/google/gson
