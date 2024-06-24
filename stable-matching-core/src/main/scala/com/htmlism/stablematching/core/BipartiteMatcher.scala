package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import cats.syntax.all.*

object BipartiteMatcher:
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
    case MissingPreferenceList(member: String)
    case IncompletePreferenceList()
