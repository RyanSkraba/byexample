Profiling application by example
==============================================================================

An example CLI and classes that can be used for demonstrating benchmarking and profiling.

Resources
------------------------------------------------------------------------------

* [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).

Running the launcher
------------------------------------------------------------------------------

```bash
mvn package
# Using the uber jar
alias byexample_go_toprofile='java -jar '$(pwd)'/target/to-profile-by-example-*-SNAPSHOT.jar'
byexample_go_toprofile --help
```
