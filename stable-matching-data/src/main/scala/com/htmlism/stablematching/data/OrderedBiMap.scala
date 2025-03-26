package com.htmlism.stablematching.data

import cats.Eq

object OrderedBiMap:
  /**
    * Used when the population is split in half, type A for proposers, and type B for acceptors.
    *
    * Every member in the population is paired with someone else.
    *
    * @tparam A
    *   Proposer type
    * @tparam B
    *   Acceptor type
    */
  case class Total[A: Eq, B: Eq](populationA: List[A], populationB: List[B], ab: Map[A, B], ba: Map[B, A]):
    assert(populationA.size == populationB.size)

    private val uniquePopulationA =
      populationA.toSet

    private val uniquePopulationB =
      populationB.toSet

    assert(uniquePopulationA == ab.keySet)
    assert(uniquePopulationA == ba.values.toSet)

    assert(uniquePopulationB == ab.keySet)
    assert(uniquePopulationB == ab.values.toSet)

  object Total:
    def empty[A: Eq, B: Eq](populationA: List[A], populationB: List[B]): Total[A, B] =
      Total(populationA, populationB, Map.empty, Map.empty)

  /**
    * Used when the population is split in half, type A for proposers, and type B for acceptors.
    *
    * Unlike its total counterpart, not every member in the population may be linked to a another.
    *
    * @tparam A
    *   Proposer type
    * @tparam B
    *   Acceptor type
    */
  case class Partial[A: Eq, B: Eq](ab: Map[A, B], ba: Map[B, A]):
    def withMatching(x: A, y: B): Partial[A, B] =
      copy(
        ab = ab.updated(x, y),
        ba = ba.updated(y, x)
      )

  object Partial:
    def empty[A: Eq, B: Eq]: Partial[A, B] =
      Partial(Map.empty, Map.empty)

case class OrderedBiMap[A: Eq, B: Eq]()
