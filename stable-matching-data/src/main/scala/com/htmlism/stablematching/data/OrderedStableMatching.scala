package com.htmlism.stablematching.data

import cats.Eq
import cats.syntax.all.*

object OrderedStableMatching:
  /**
    * Used when the population matches to itself, as in the stable roommates problem
    *
    * Every member in the population is paired with someone else.
    *
    * @tparam A
    *   The roommate type
    */
  // TODO add method to have matches as ordered output
  case class Total[A: Eq](population: List[A], mapping: Map[A, A]):
    private val uniquePopulation =
      population.toSet

    assert(uniquePopulation == mapping.keySet)
    assert(uniquePopulation == mapping.values.toSet)

  /**
    * Used when the population matches to itself, as in the stable roommates problem
    *
    * Unlike its total counterpart, not every member in the population may be linked to a another.
    *
    * @tparam A
    *   The roommate type
    */
  case class Partial[A: Eq](mapping: Map[A, A]):
    /**
      * Adds a matching between two members of the population
      *
      * Asserts that the two members are not equal
      */
    def withMatching(x: A, y: A): Partial[A] =
      assert(x =!= y)

      copy(
        mapping = mapping
          .updated(x, y)
          .updated(y, x)
      )

  object Partial:
    def empty[A: Eq]: Partial[A] =
      Partial(Map.empty)
