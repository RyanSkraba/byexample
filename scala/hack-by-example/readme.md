Hack by example
==============================================================================

Coding challenges and games!

Advent of Code 2022
------------------------------------------------------------------------------

| Day                                               | Techniques and notes                                                                               |
|---------------------------------------------------|----------------------------------------------------------------------------------------------------|
| [Day 1][AoC2022-01] ([Solution][Sol-AoC2022-01])  | Using `foldLeft` to accumulate over lists joined by empty lines.                                   |
| [Day 2][AoC2022-02] ([Solution][Sol-AoC2022-02])  |                                                                                                    |
| [Day 3][AoC2022-03] ([Solution][Sol-AoC2022-03])  |                                                                                                    |
| [Day 4][AoC2022-04] ([Solution][Sol-AoC2022-04])  |                                                                                                    |
| [Day 5][AoC2022-05] ([Solution][Sol-AoC2022-05])  |                                                                                                    |
| [Day 6][AoC2022-06] ([Solution][Sol-AoC2022-06])  |                                                                                                    |
| [Day 7][AoC2022-07] ([Solution][Sol-AoC2022-07])  | Parsing console output with regular expressions. Creating and navigating a tree-like `case class`. |
| [Day 8][AoC2022-08] ([Solution][Sol-AoC2022-08])  |                                                                                                    |
| [Day 9][AoC2022-09] ([Solution][Sol-AoC2022-09])  |                                                                                                    |
| [Day 10][AoC2022-10] ([Solution][Sol-AoC2022-10]) |                                                                                                    |
| [Day 11][AoC2022-11] ([Solution][Sol-AoC2022-11]) | Manually hard-coding some inputs instead of parsing.                                               |
| [Day 12][AoC2022-12] ([Solution][Sol-AoC2022-12]) | Breadth-First Search on a height map.                                                              |
| [Day 13][AoC2022-13] ([Solution][Sol-AoC2022-13]) |                                                                                                    |
| [Day 14][AoC2022-14] ([Solution][Sol-AoC2022-14]) | Calculate falling sand in a cave.                                                                  |
| [Day 15][AoC2022-15] ([Solution][Sol-AoC2022-15]) |                                                                                                    |
| [Day 16][AoC2022-16] ([Solution][Sol-AoC2022-16]) | Floyd-Warshall minimum paths.  Depth-First Search optimising open valves. _(57 minutes)_           |
| [Day 17][AoC2022-17] ([Solution][Sol-AoC2022-17]) | Cycle detection in a tetris like falling-rock game.                                                |
| [Day 18][AoC2022-18] ([Solution][Sol-AoC2022-18]) | Three-dimensional surface detection.                                                               |
| [Day 19][AoC2022-19] ([Solution][Sol-AoC2022-19]) | Building golems to build golems: DFS optimisation with some tricky heuristics. _(2 minutes)_       |
| [Day 20][AoC2022-20] ([Solution][Sol-AoC2022-20]) |                                                                                                    |
| [Day 21][AoC2022-21] ([Solution][Sol-AoC2022-21]) |                                                                                                    |
| [Day 22][AoC2022-22] ([Solution][Sol-AoC2022-22]) | Moving around an unfolded cube, manually setting up the folded edges.                              |
| [Day 23][AoC2022-23] ([Solution][Sol-AoC2022-23]) |                                                                                                    |
| [Day 24][AoC2022-24] ([Solution][Sol-AoC2022-24]) |                                                                                                    |
| [Day 25][AoC2022-25] ([Solution][Sol-AoC2022-25]) |                                                                                                    |

[AoC2022-01]: https://adventofcode.com/2022/day/1
[AoC2022-02]: https://adventofcode.com/2022/day/2
[AoC2022-03]: https://adventofcode.com/2022/day/3
[AoC2022-04]: https://adventofcode.com/2022/day/4
[AoC2022-05]: https://adventofcode.com/2022/day/5
[AoC2022-06]: https://adventofcode.com/2022/day/6
[AoC2022-07]: https://adventofcode.com/2022/day/7
[AoC2022-08]: https://adventofcode.com/2022/day/8
[AoC2022-09]: https://adventofcode.com/2022/day/9
[AoC2022-10]: https://adventofcode.com/2022/day/10
[AoC2022-11]: https://adventofcode.com/2022/day/11
[AoC2022-12]: https://adventofcode.com/2022/day/12
[AoC2022-13]: https://adventofcode.com/2022/day/13
[AoC2022-14]: https://adventofcode.com/2022/day/14
[AoC2022-15]: https://adventofcode.com/2022/day/15
[AoC2022-16]: https://adventofcode.com/2022/day/16
[AoC2022-17]: https://adventofcode.com/2022/day/17
[AoC2022-18]: https://adventofcode.com/2022/day/18
[AoC2022-19]: https://adventofcode.com/2022/day/19
[AoC2022-20]: https://adventofcode.com/2022/day/20
[AoC2022-21]: https://adventofcode.com/2022/day/21
[AoC2022-22]: https://adventofcode.com/2022/day/22
[AoC2022-23]: https://adventofcode.com/2022/day/23
[AoC2022-24]: https://adventofcode.com/2022/day/24
[AoC2022-25]: https://adventofcode.com/2022/day/25
[Sol-AoC2022-01]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay1Spec.scala
[Sol-AoC2022-02]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay2Spec.scala
[Sol-AoC2022-03]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay3Spec.scala
[Sol-AoC2022-04]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay4Spec.scala
[Sol-AoC2022-05]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay5Spec.scala
[Sol-AoC2022-06]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay6Spec.scala
[Sol-AoC2022-07]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay7Spec.scala
[Sol-AoC2022-08]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay8Spec.scala
[Sol-AoC2022-09]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay9Spec.scala
[Sol-AoC2022-10]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay10Spec.scala
[Sol-AoC2022-11]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay11pec.scala
[Sol-AoC2022-12]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay12Spec.scala
[Sol-AoC2022-13]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay13Spec.scala
[Sol-AoC2022-14]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay14Spec.scala
[Sol-AoC2022-15]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay15Spec.scala
[Sol-AoC2022-16]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay16Spec.scala
[Sol-AoC2022-17]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay17Spec.scala
[Sol-AoC2022-18]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay18Spec.scala
[Sol-AoC2022-19]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay19Spec.scala
[Sol-AoC2022-20]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay20Spec.scala
[Sol-AoC2022-21]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay21Spec.scala
[Sol-AoC2022-22]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay22Spec.scala
[Sol-AoC2022-23]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay23Spec.scala
[Sol-AoC2022-24]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay24Spec.scala
[Sol-AoC2022-25]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay25Spec.scala
