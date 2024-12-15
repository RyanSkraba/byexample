Hack by example
==============================================================================

Coding challenges and games!

Advent of Code
==============================================================================

For the [Advent of Code](https://adventofcode.com/), this is a pretty good coding challenge and lots of fun.  I'm not playing competitively, just looking to find interesting and elegant solutions to these problems!

It's a point of pride to do these on my own, but I don't consider it cheating to look for [hints](https://www.reddit.com/r/adventofcode/) when I am obviously and hopelessly stuck.  Sometimes just a few words (`quadratic` or `Krager's`) is enough to lead me in the right direction for my own implementation, but sometimes I need to work through someone's implementation to find the bugs in mine.  I've noted the problems where I've gotten a bit of help!

Advent of Code 2024
------------------------------------------------------------------------------

🟢 Easy / 🔶 Medium / 🟥 Hard / 💜️ Fun / 😱️ Frustrating / 🔵 Needed hints

| Day                                                 | Techniques and notes                                                                                                 |
|-----------------------------------------------------|----------------------------------------------------------------------------------------------------------------------|
| 🟢[Day 1][AoC2024-01] ([Solution][Sol-AoC2024-01])  | Input as two columns of numbers, then sorting and finding matches.                                                   |
| 🟢[Day 2][AoC2024-02] ([Solution][Sol-AoC2024-02])  | Each line is a list of numbers. Find the differences between numbers, exhaustively check when one number is removed. |
| 🟢[Day 3][AoC2024-03] ([Solution][Sol-AoC2024-03])  | One long corrupted string over several lines.  Regex, and splitting.                                                 |
| 🟢[Day 4][AoC2024-04] ([Solution][Sol-AoC2024-04])  | Looking up words in any direction in a 2D array of characters.                                                       |
| 🟢[Day 5][AoC2024-05] ([Solution][Sol-AoC2024-05])  | Finding pair-wise violations in a list of longs, sorting with custom rules                                           |
| 🔶[Day 6][AoC2024-06] ([Solution][Sol-AoC2024-06])  | Moving around a map with obstacles, adding a blockage to induce a loop.  Good map example.                           |
| 🟢[Day 7][AoC2024-07] ([Solution][Sol-AoC2024-07])  | Recursive solution to applying operations to a list of numbers.  Also a DFS implemented using LazyLists.             |
| 🟢[Day 8][AoC2024-08] ([Solution][Sol-AoC2024-08])  | Find points on a grid that are equidistant from different antenna.                                                   |
| 🟢[Day 9][AoC2024-09] ([Solution][Sol-AoC2024-09])  | Move intervals of blocks into empty spaces.                                                                          |
| 🟢[Day 10][AoC2024-10] ([Solution][Sol-AoC2024-10]) | Find hiking paths through a topological map that cover 0..9.                                                         |
| 🟢[Day 11][AoC2024-11] ([Solution][Sol-AoC2024-11]) | Apply rules to a set of numbers. Two good solutions, one is a nice example of memoization.                           |
| 🔶[Day 12][AoC2024-12] ([Solution][Sol-AoC2024-12]) | Finding disjoint sets (using union-find or merge-find) to calculate perimeters or sides of garden plots in a 2D map. |
| 🔶[Day 13][AoC2024-13] ([Solution][Sol-AoC2024-13]) | Miniature linear algebra, finding integer solutions to two equations.                                                |
| 🔶[Day 14][AoC2024-14] ([Solution][Sol-AoC2024-14]) | 💜Robots moving around a grid given a starting point and vector.  Find the pretty picture they make.                 |
| 🟥[Day 15][AoC2024-15] ([Solution][Sol-AoC2024-15]) | A Robot pushing movable boxes in a 2D warehouse, where boxes can push other boxes.                                   |

[AoC2024-01]: https://adventofcode.com/2024/day/1
[AoC2024-02]: https://adventofcode.com/2024/day/2
[AoC2024-03]: https://adventofcode.com/2024/day/3
[AoC2024-04]: https://adventofcode.com/2024/day/4
[AoC2024-05]: https://adventofcode.com/2024/day/5
[AoC2024-06]: https://adventofcode.com/2024/day/6
[AoC2024-07]: https://adventofcode.com/2024/day/7
[AoC2024-08]: https://adventofcode.com/2024/day/8
[AoC2024-09]: https://adventofcode.com/2024/day/9
[AoC2024-10]: https://adventofcode.com/2024/day/10
[AoC2024-11]: https://adventofcode.com/2024/day/11
[AoC2024-12]: https://adventofcode.com/2024/day/12
[AoC2024-13]: https://adventofcode.com/2024/day/13
[AoC2024-14]: https://adventofcode.com/2024/day/14
[AoC2024-15]: https://adventofcode.com/2024/day/15
[AoC2024-16]: https://adventofcode.com/2024/day/16
[AoC2024-17]: https://adventofcode.com/2024/day/17
[AoC2024-18]: https://adventofcode.com/2024/day/18
[AoC2024-19]: https://adventofcode.com/2024/day/19
[AoC2024-20]: https://adventofcode.com/2024/day/20
[AoC2024-21]: https://adventofcode.com/2024/day/21
[AoC2024-22]: https://adventofcode.com/2024/day/22
[AoC2024-23]: https://adventofcode.com/2024/day/23
[AoC2024-24]: https://adventofcode.com/2024/day/24
[AoC2024-25]: https://adventofcode.com/2024/day/25
[Sol-AoC2024-01]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay1Spec.scala
[Sol-AoC2024-02]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay2Spec.scala
[Sol-AoC2024-03]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay3Spec.scala
[Sol-AoC2024-04]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay4Spec.scala
[Sol-AoC2024-05]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay5Spec.scala
[Sol-AoC2024-06]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay6Spec.scala
[Sol-AoC2024-07]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay7Spec.scala
[Sol-AoC2024-08]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay8Spec.scala
[Sol-AoC2024-09]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay9Spec.scala
[Sol-AoC2024-10]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay10Spec.scala
[Sol-AoC2024-11]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay11Spec.scala
[Sol-AoC2024-12]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay12Spec.scala
[Sol-AoC2024-13]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay13Spec.scala
[Sol-AoC2024-14]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay14Spec.scala
[Sol-AoC2024-15]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay15Spec.scala
[Sol-AoC2024-16]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay16Spec.scala
[Sol-AoC2024-17]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay17Spec.scala
[Sol-AoC2024-18]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay18Spec.scala
[Sol-AoC2024-19]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay19Spec.scala
[Sol-AoC2024-20]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay20Spec.scala
[Sol-AoC2024-21]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay21Spec.scala
[Sol-AoC2024-22]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay22Spec.scala
[Sol-AoC2024-23]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay23Spec.scala
[Sol-AoC2024-24]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay24Spec.scala
[Sol-AoC2024-25]: src/test/scala/com/skraba/byexample/scala/hack/advent2024/AdventOfCodeDay25Spec.scala

Advent of Code 2023
------------------------------------------------------------------------------

🟢 Easy / 🔶 Medium / 🟥 Hard / 💜️ Fun / 😱️ Frustrating / 🔵 Needed hints

| Day                                                  | Techniques and notes                                                                                                                                                                                                                                                                          |
|------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 🟢[Day 1][AoC2023-01] ([Solution][Sol-AoC2023-01])   | Extracting digits from a string with regular expressions.                                                                                                                                                                                                                                     |
| 🟢[Day 2][AoC2023-02] ([Solution][Sol-AoC2023-02])   | Simple math.                                                                                                                                                                                                                                                                                  |
| 🟢[Day 3][AoC2023-03] ([Solution][Sol-AoC2023-03])   | Searching for numbers in a two dimensional map of characters and symbols, represented internally as a single string.                                                                                                                                                                          |
| 🔶[Day 4][AoC2023-04] ([Solution][Sol-AoC2023-04])   | Math, counting and counting and counting with some tricky cases.                                                                                                                                                                                                                              |
| 🔶[Day 5][AoC2023-05] ([Solution][Sol-AoC2023-05])   | Working through ranges of numbers, implemented an Interval class.                                                                                                                                                                                                                             |
| 🟢[Day 6][AoC2023-06] ([Solution][Sol-AoC2023-06])   | Math, worked out on paper before starting.                                                                                                                                                                                                                                                    |
| 🔶[Day 7][AoC2023-07] ([Solution][Sol-AoC2023-07])   | 💜 Implementing a custom ordering for card hands.                                                                                                                                                                                                                                             |
| 🟥[Day 8][AoC2023-08] ([Solution][Sol-AoC2023-08])   | 😱️ Traversing a graph. Part 1 was simply following the instructions, but Part 2 needed us to deduce input characteristics that weren't obvious. The solution is special for the carefully crafted inputs.                                                                                    |
| 🟢[Day 9][AoC2023-09] ([Solution][Sol-AoC2023-09])   | 💜 ️ Math series, super simple but elegantly fun.                                                                                                                                                                                                                                             |
| 🟥[Day 10][AoC2023-10] ([Solution][Sol-AoC2023-10])️ | 💜 2D map of characters again: a long connected pipe shape.  Solved with regexes. A bit trickier but fell on the right approach early and had fun.                                                                                                                                            |
| 🟢[Day 11][AoC2023-11] ([Solution][Sol-AoC2023-11])  | 2D map of galaxies, converted into a sparse set of positions and doing the calculations around the spaces.                                                                                                                                                                                    |
| 🟥[Day 12][AoC2023-12] ([Solution][Sol-AoC2023-12])  | 💜 Fun! Started with a recursive search for Part 1, and added a memo (dynamic programming for part 2). It took some work to get the stopping conditions correct.                                                                                                                              |
| 🔶[Day 13][AoC2023-13] ([Solution][Sol-AoC2023-13])  | After getting the code to check for symmetries in a mirror, the second part was just brute force.                                                                                                                                                                                             |
| 🔶[Day 14][AoC2023-14] ([Solution][Sol-AoC2023-14])  | 💜 Another 2D map of characters.  Part 2 was to find and skip over the repeating cycle, and my `LazyList` implementation was pretty fun.                                                                                                                                                      |
| 🟢[Day 15][AoC2023-15] ([Solution][Sol-AoC2023-15])  | Nice simulation puzzle -- I did two solutions to Part two, because I thought of a preprocessing step to simplify the answer (twice as fast).                                                                                                                                                  |
| 🟢[Day 16][AoC2023-16] ([Solution][Sol-AoC2023-16])  | 💜️ A two-dimensional map of mirrors and splitters, counting how many squares can be energized by light.                                                                                                                                                                                      |
| 🟥[Day 17][AoC2023-17] ([Solution][Sol-AoC2023-17])  | BFS to find an optimal path through a 2D head map, with some constraints on movement. Part 1 had a maximum distance for any straight line, and Part 2 had a minimum and maximum.                                                                                                              |
| 🔶[Day 18][AoC2023-18] ([Solution][Sol-AoC2023-18])  | 💜 Fun! Part 1 was a building a 2D plan from directions, and then flood-fill, but I implemented Part 2 with a scan line.  It looks like a lot of people used Shoelace Formula to calculate the area in a polygon, and Pick's Theorem for points inside a polygon.                             |
| 🔶[Day 19][AoC2023-19] ([Solution][Sol-AoC2023-19])  | DFS tree traversal, where you pretty much need to explore the entire tree.                                                                                                                                                                                                                    |
| 🟥[Day 20][AoC2023-20] ([Solution][Sol-AoC2023-20])  | 😱️ Ugh. Implementing a graph and electronics-like gates, which was fun, but for Part 2 I just guessed at the answer, like I did on Day 8.  Really unsatisfactory.                                                                                                                            |
| 🟥[Day 21][AoC2023-21] ([Solution][Sol-AoC2023-21])  | 😱️ Not very fun -- Part 1 was a BFS-like search, but Part 2 relied on some fussy math solving a geometric progression.  I got the 🔵 hint from reddit, but still couldn't implement it correctly, and ended up reading and running some python code to check my answer until I got it right. |
| [Day 22][AoC2023-22] ([Solution][Sol-AoC2023-22])    |                                                                                                                                                                                                                                                                                               |
| [Day 23][AoC2023-23] ([Solution][Sol-AoC2023-23])    |                                                                                                                                                                                                                                                                                               |
| [Day 24][AoC2023-24] ([Solution][Sol-AoC2023-24])    |                                                                                                                                                                                                                                                                                               |
| [Day 25][AoC2023-25] ([Solution][Sol-AoC2023-25])    |                                                                                                                                                                                                                                                                                               |

[AoC2023-01]: https://adventofcode.com/2023/day/1
[AoC2023-02]: https://adventofcode.com/2023/day/2
[AoC2023-03]: https://adventofcode.com/2023/day/3
[AoC2023-04]: https://adventofcode.com/2023/day/4
[AoC2023-05]: https://adventofcode.com/2023/day/5
[AoC2023-06]: https://adventofcode.com/2023/day/6
[AoC2023-07]: https://adventofcode.com/2023/day/7
[AoC2023-08]: https://adventofcode.com/2023/day/8
[AoC2023-09]: https://adventofcode.com/2023/day/9
[AoC2023-10]: https://adventofcode.com/2023/day/10
[AoC2023-11]: https://adventofcode.com/2023/day/11
[AoC2023-12]: https://adventofcode.com/2023/day/12
[AoC2023-13]: https://adventofcode.com/2023/day/13
[AoC2023-14]: https://adventofcode.com/2023/day/14
[AoC2023-15]: https://adventofcode.com/2023/day/15
[AoC2023-16]: https://adventofcode.com/2023/day/16
[AoC2023-17]: https://adventofcode.com/2023/day/17
[AoC2023-18]: https://adventofcode.com/2023/day/18
[AoC2023-19]: https://adventofcode.com/2023/day/19
[AoC2023-20]: https://adventofcode.com/2023/day/20
[AoC2023-21]: https://adventofcode.com/2023/day/21
[AoC2023-22]: https://adventofcode.com/2023/day/22
[AoC2023-23]: https://adventofcode.com/2023/day/23
[AoC2023-24]: https://adventofcode.com/2023/day/24
[AoC2023-25]: https://adventofcode.com/2023/day/25
[Sol-AoC2023-01]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay1Spec.scala
[Sol-AoC2023-02]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay2Spec.scala
[Sol-AoC2023-03]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay3Spec.scala
[Sol-AoC2023-04]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay4Spec.scala
[Sol-AoC2023-05]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay5Spec.scala
[Sol-AoC2023-06]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay6Spec.scala
[Sol-AoC2023-07]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay7Spec.scala
[Sol-AoC2023-08]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay8Spec.scala
[Sol-AoC2023-09]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay9Spec.scala
[Sol-AoC2023-10]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay10Spec.scala
[Sol-AoC2023-11]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay11Spec.scala
[Sol-AoC2023-12]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay12Spec.scala
[Sol-AoC2023-13]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay13Spec.scala
[Sol-AoC2023-14]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay14Spec.scala
[Sol-AoC2023-15]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay15Spec.scala
[Sol-AoC2023-16]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay16Spec.scala
[Sol-AoC2023-17]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay17Spec.scala
[Sol-AoC2023-18]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay18Spec.scala
[Sol-AoC2023-19]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay19Spec.scala
[Sol-AoC2023-20]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay20Spec.scala
[Sol-AoC2023-21]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay21Spec.scala
[Sol-AoC2023-22]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay22Spec.scala
[Sol-AoC2023-23]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay23Spec.scala
[Sol-AoC2023-24]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay24Spec.scala
[Sol-AoC2023-25]: src/test/scala/com/skraba/byexample/scala/hack/advent2023/AdventOfCodeDay25Spec.scala

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
[Sol-AoC2022-11]: src/test/scala/com/skraba/byexample/scala/hack/advent2022/AdventOfCodeDay11Spec.scala
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

Running Advent of Code
------------------------------------------------------------------------------

The Advent of Code challenges are all run as unit tests:

```
mvn clean verify

```

By default, tests tagged `Slow` are not run.  See [scala-by-example](../scala-by-example/) for running the slow tests from the command line or from an IDE.

The author of [Advent of Code](https://adventofcode.com/about) asks us not to check in or share our personalized inputs or answers to the puzzles (although the example inputs and answer are shareable). Although they are checked in here, they are only readable when the `ADVENT_OF_CODE_KEY` environment variable is set to _**my**_ private 256-bit AES key (Base64-encoded). If this environment variable is not set, these tests should be automatically ignored.
