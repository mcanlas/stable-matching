package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import cats.syntax.all.*

/**
  * Also referred to as the "stable roommates problem" https://en.wikipedia.org/wiki/Stable_roommates_problem
  */
object MonopartiteMatcher:
  def createMatches[A: Eq](
      population: Set[A],
      preferences: Map[A, NonEmptyList[A]],
      order: Order[A]
  ): Either[Error, List[String]] =
    def validatePopulationSize(population: Set[A]) =
      Either.cond(
        population.size % 2 == 0,
        population,
        Error.UnsupportedPopulationSize(population.size)
      )

    def validatePreferenceExists(xs: Map[A, NonEmptyList[A]])(x: A) =
      if xs.contains(x) then ().validNec
      else x.toString.invalidNec

    for
      pop <- validatePopulationSize(population)

      _ <- pop
        .toList
        .traverse(validatePreferenceExists(preferences))
        .as(preferences)
        .toEither
        .leftMap(Error.MissingPreferenceList(_))
    yield Nil

  // TODO apply algorithm is a list/recursive

  enum Error:
    case UnsupportedPopulationSize(n: Int)
    case MissingPreferenceList(xs: NonEmptyChain[String])
    case IncompletePreferenceList()
