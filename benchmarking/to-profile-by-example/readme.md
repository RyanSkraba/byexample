Profiling application by example
==============================================================================

An example CLI and classes that can be used for demonstrating benchmarking and profiling.

Resources
------------------------------------------------------------------------------

* [Sieve of Eratosthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes).

In addition, you can select only those prime numbers that are either [super], [happy], [sexy] or
a combination of the above.

[super]: https://en.wikipedia.org/wiki/Super-prime "If n is prime, the n'th prime number is super"
[happy]: https://en.wikipedia.org/wiki/Happy_number "A happy number eventually reaches one when summing it's digits squared"
[sexy]: https://en.wikipedia.org/wiki/Sexy_prime "A prime that differs by six with another prime"

Running the launcher
------------------------------------------------------------------------------

```bash
mvn package
# Using the uber jar from the command line
alias byexample_go_toprofile="java -jar $(find ~+ -name to-profile-by-example-*.jar)"
byexample_go_toprofile --help
```
