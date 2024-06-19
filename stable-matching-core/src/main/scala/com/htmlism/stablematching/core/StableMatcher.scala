package com.htmlism.stablematching.core

import cats.*
import cats.data.*

object StableMatcher:
  object Monopartite:
    def createMatches[A: Eq](
        population: Set[A],
        preferences: Map[A, NonEmptyList[A]],
        order: Order[A]
    ): Either[Error, List[String]] =
      def validatePopulationSize(population: Set[A]) =
        Either.cond(
          population.size % 2 == 0,
          population,
          Error.UnsupportedPopulationNumber(population.size)
        )

      for _ <- validatePopulationSize(population)
      yield Nil

    // TODO validate preference exists as valid nec

    enum Error:
      case UnsupportedPopulationNumber(n: Int)
      case MissingPreferences(member: String)

  object Bipartite:
    def createMatches[A: Eq, B: Eq](
        proposerPopulation: Set[A],
        acceptorPopulation: Set[B],
        proposerPreferences: Map[A, NonEmptyList[B]],
        acceptorPreferences: Map[B, NonEmptyList[A]],
        proposerOrder: Order[A],
        acceptorOrder: Order[B]
    ): Either[Error, List[String]] =
      def validatePopulationSizes(xs: Set[A], ys: Set[B]) =
        Either.cond(
          xs.size == ys.size,
          xs -> ys,
          Error.MismatchedPopulationSizes(xs.size, ys.size)
        )

      for (xs, ys) <- validatePopulationSizes(proposerPopulation, acceptorPopulation)
      yield Nil

    enum Error:
      case MismatchedPopulationSizes(proposers: Int, acceptors: Int)
      case MissingPreferences(member: String)
