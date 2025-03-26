package com.htmlism.stablematching.data

import scala.collection.immutable.ListSet

import cats.*
import cats.syntax.all.*

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
  // TODO add method to have matches as ordered output
  case class Total[A: Eq, B: Eq](populationA: ListSet[A], populationB: ListSet[B], ab: Map[A, B], ba: Map[B, A]):
    assert(populationA.size === populationB.size, "proposers and acceptors must be the same size")
    assert(ab.size === ba.size, "mapping must be bijective")

    assert(populationA.toSet === ab.keySet, "proposer mapping must be total")
    assert(populationA.toSet === ba.values.toSet, "acceptor values must be total")

    assert(populationB.toSet === ba.keySet, "acceptor mapping must be total")
    assert(populationB.toSet === ab.values.toSet, "proposer values must be total")

  object Total:
    def empty[A: Eq, B: Eq](populationA: ListSet[A], populationB: ListSet[B]): Total[A, B] =
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
