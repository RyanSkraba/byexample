package com.skraba.byexample.scala.algorithms

import scala.collection.mutable

class GraphFloydWarshall {

  def floydWarshall[T](
      vs: Iterable[T],
      edgeF: T => Iterator[T]
  ): Map[(T, T), Long] = {
    // Set up the distances between all vertices
    // If there's an initial edge, the distance is 1
    // The distance to itself is 0
    val dv = mutable.Map[(T, T), Long]().withDefaultValue(Long.MaxValue) ++
      vs.map(src => (src -> src, 0L)) ++
      (for (src <- vs; dst <- edgeF(src)) yield (src -> dst) -> 1L)

    // Apply all intermediate paths in all combinations
    for (mid <- vs; src <- vs; dst <- vs)
      if (
        dv(src -> mid) != Long.MaxValue &&
        dv(mid -> dst) != Long.MaxValue &&
        dv(src -> mid) + dv(mid -> dst) < dv(src -> dst)
      ) dv += (src -> dst) -> (dv(src -> mid) + dv(mid -> dst))

    dv.toMap
  }

  def floydWarshallWithPath[T](
      vs: Iterable[T],
      edgeF: T => Iterator[T]
  ): Map[(T, T), (Long, Option[T])] = {
    // Set up the distances between all vertices
    // If there's an initial edge, the distance is 1
    // The distance to itself is 0
    val dv: mutable.Map[(T, T), (Long, Option[T])] = mutable
      .Map[(T, T), (Long, Option[T])]()
      .withDefaultValue((Long.MaxValue, None)) ++
      vs.map(src => (src -> src, (0L, None))) ++
      (for (src <- vs; dst <- edgeF(src))
        yield (src -> dst) -> (1L, Some(src)))

    // Apply all intermediate paths in all combinations
    for (mid <- vs; src <- vs; dst <- vs)
      if (
        dv(src -> mid)._1 != Long.MaxValue &&
        dv(mid -> dst)._1 != Long.MaxValue &&
        dv(src -> mid)._1 + dv(mid -> dst)._1 < dv(src -> dst)._1
      )
        dv += (src -> dst) -> (dv(src -> mid)._1 + dv(mid -> dst)._1, Some(
          mid
        ))

    dv.toMap
  }

  def floydWarshallWithEdgeWeights[T](
      vs: Iterable[T],
      edgeFn: T => Iterator[(T, Long)]
  ): Map[(T, T), Long] = {
    // Set up the distances between all vertices
    // The distance to itself is 0
    val dv = mutable.Map[(T, T), Long]().withDefaultValue(Long.MaxValue) ++
      vs.map(src => (src -> src, 0L)) ++
      (for (src <- vs; (dst, weight) <- edgeFn(src))
        yield (src -> dst) -> weight)

    // Apply all intermediate paths in all combinations
    for (mid <- vs; src <- vs; dst <- vs)
      if (
        dv(src -> mid) != Long.MaxValue &&
        dv(mid -> dst) != Long.MaxValue &&
        dv(src -> mid) + dv(mid -> dst) < dv(src -> dst)
      ) dv += (src -> dst) -> (dv(src -> mid) + dv(mid -> dst))

    dv.toMap
  }
}
