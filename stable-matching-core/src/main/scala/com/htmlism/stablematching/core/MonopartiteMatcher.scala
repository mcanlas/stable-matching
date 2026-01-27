package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import cats.syntax.all.*

import com.htmlism.stablematching.data.*

/**
  * Also referred to as the "stable roommates problem" https://en.wikipedia.org/wiki/Stable_roommates_problem
  */
object MonopartiteMatcher:
  type Result[A] =
    Either[Error, A]

  /**
    * Attempts to find a stable matching for a population of members. In the monopartite or "roommates" formulation, a
    * stable matching is not guaranteed to exist.
    *
    * @param members
    *   An ordered, unique list of members. Ordering affects the results of the matching
    */
  def createMatches[A: Order](
      members: ListSet[A],
      preferences: Map[A, NonEmptyList[A]]
  ): WriterT[Result, Chain[String], OrderedStableMatching.Total[A]] =
    def validatePopulationSize(population: Set[A]) =
      Either.cond(
        population.size % 2 == 0,
        population,
        Error.UnsupportedPopulationSize(population.size)
      )

    def validatePreferenceExists(xs: Map[A, NonEmptyList[A]])(x: A) =
      if xs.contains(x) then ().validNec
      else x.toString.invalidNec

    def validateMemberIsInPreferences =
      (members.toList, preferences.toList)
        .tupled
        .traverse:
          case (p, (k, xs)) =>
            if k == p then ().asRight
            else
              Either.cond(
                xs.contains_(p),
                (),
                Error.IncompletePreferenceList(p.toString, k.toString)
              )

    for
      pop <- WriterT.liftF(validatePopulationSize(members))

      _ <- WriterT
        .liftF:
          pop
            .toList
            .traverse(validatePreferenceExists(preferences))
            .as(preferences)
            .toEither
            .leftMap(Error.MissingPreferenceList(_))

      _ <- WriterT.liftF(validateMemberIsInPreferences)

      noMatches = State[A](
        members     = members,
        preferences = preferences,
        matching    = OrderedStableMatching.Partial.empty
      )

      matchesComplete = Id(noMatches)
        .tailRecM(acc => Functor[Id].map(acc)(_.applyOne.swap))
    yield matchesComplete

  case class State[A: Eq](
      members: ListSet[A],
      preferences: Map[A, NonEmptyList[A]],
      matching: OrderedStableMatching.Partial[A]
  ):
    /**
      * Assign a matching to the next proposer that isn't already matched
      *
      * @return
      *   Right if a match was applied, left if no match was applied
      */
    def applyOne: Either[OrderedStableMatching.Total[A], State[A]] =
      // TODO this should be ordered
      val nextProposer =
        members
          .iterator
          .find(!matching.mapping.contains(_))

      nextProposer match
        case Some(p) =>
          Right:
            copy(
              matching    = matching.withMatching(p, preferences(p).head),
              preferences = preferences.updated(p, preferences(p))
            )
        case None =>
          Left:
            OrderedStableMatching.Total(
              population = members,
              mapping    = matching.mapping
            )

  enum Error:
    case UnsupportedPopulationSize(n: Int)
    case MissingPreferenceList(xs: NonEmptyChain[String])

    /**
      * Member $member is not reflected in preference list of member $preferenceKey
      */
    case IncompletePreferenceList(member: String, preferenceKey: String)

    /**
      * Unlike the bipartite problem, a stable matching is not guaranteed to exist
      */
    case StableMatchingNotPossible
