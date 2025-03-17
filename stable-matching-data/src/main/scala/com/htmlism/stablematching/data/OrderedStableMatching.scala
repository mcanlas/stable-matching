package com.htmlism.stablematching.data

import cats.Eq
import cats.syntax.all.*

/**
  * Used when the population matches to itself, as in the stable roommates problem
  *
  * @tparam A
  *   The roommate type
  */
case class OrderedStableMatching[A: Eq]()

object OrderedStableMatching:
  case class Total[A: Eq](population: List[A], mapping: Map[A, A]):
    assert(population.size == mapping.size)

  case class Partial[A: Eq](population: List[A], mapping: Map[A, A]):
    def withMatching(x: A, y: A): Either[String, Partial[A]] =
      Either
        .cond(
          x =!= y,
          copy(
            mapping = mapping
              .updated(x, y)
              .updated(y, x)
          ),
          "x cannot equal y in a matching",
        )

  object Partial:
    def empty[A: Eq](population: List[A]): Partial[A] =
      Partial(population, Map.empty)
