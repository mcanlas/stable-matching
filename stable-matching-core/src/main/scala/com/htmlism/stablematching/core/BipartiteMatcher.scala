package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import cats.syntax.all.*

import com.htmlism.stablematching
import com.htmlism.stablematching.data.*

object BipartiteMatcher:
  type Result[A] =
    Either[Error, A]

  /**
    * @param proposers
    *   An ordered, unique list of proposers. Ordering affects the results of the matching
    * @param acceptors
    *   An ordered, unique list of acceptors. Ordering affects the results of the matching
    * @return
    *   One stable matching, potentially of many others
    */
  def createMatches[A: Order, B: Order](
      proposers: ListSet[A],
      acceptors: ListSet[B],
      proposerPreferences: Map[A, NonEmptyList[B]],
      acceptorPreferences: Map[B, NonEmptyList[A]]
  ): WriterT[Result, Chain[String], OrderedBiMap.Total[A, B]] =
    def validatePopulationSizes(xs: Set[A], ys: Set[B]) =
      Either.cond(
        xs.size == ys.size,
        (),
        Error.MismatchedPopulationSizes(xs.size, ys.size)
      )

    def validateProposersAreInPreferences =
      (proposers.toList, acceptorPreferences.toList)
        .tupled
        .traverse:
          case (p, (k, xs)) =>
            Either.cond(
              xs.contains_(p),
              (),
              Error.IncompleteAcceptorsPreferenceList(p.toString, k.toString)
            )
        .void

    def validateAcceptorsAreInPreferences =
      (acceptors.toList, proposerPreferences.toList)
        .tupled
        .traverse:
          case (a, (k, xs)) =>
            Either.cond(
              xs.contains_(a),
              (),
              Error.IncompleteProposersPreferenceList(a.toString, k.toString)
            )
        .void

    for
      _ <- WriterT.liftF(validatePopulationSizes(proposers, acceptors))

      _ <- WriterT
        .liftF:
          proposers
            .toList
            .traverse: p =>
              if proposerPreferences.contains(p) then ().validNec
              else p.toString.invalidNec
            .toEither
            .leftMap(Error.MissingProposerPreferenceList(_))

      _ <- WriterT
        .liftF:
          acceptors
            .toList
            .traverse: p =>
              if acceptorPreferences.contains(p) then ().validNec
              else p.toString.invalidNec
            .toEither
            .leftMap(Error.MissingAcceptorPreferenceList(_))

      _ <- WriterT.liftF(validateProposersAreInPreferences)

      _ <- WriterT.liftF(validateAcceptorsAreInPreferences)

      state = State[A, B](
        proposers           = proposers,
        acceptors           = acceptors,
        proposerPreferences = proposerPreferences,
        acceptorPreferences = acceptorPreferences,
        matching            = OrderedBiMap.Partial.empty
      )

      _ <- WriterT.tell(Chain(state.toString))

      res <- WriterT.put(
        OrderedBiMap.Total(
          populationA = proposers,
          populationB = acceptors,
          ab          = Map.empty,
          ba          = Map.empty
        )
      )(Chain("nil"))
    yield res

  case class State[A, B](
      proposers: ListSet[A],
      acceptors: ListSet[B],
      proposerPreferences: Map[A, NonEmptyList[B]],
      acceptorPreferences: Map[B, NonEmptyList[A]],
      matching: OrderedBiMap.Partial[A, B]
  ):
    /**
      * Assign a matching to the next proposer that isn't already matched
      *
      * @return
      *   Right if a match was applied, left if no match was applied
      */
    def applyOne: Either[OrderedBiMap.Total[A, B], State[A, B]] =
      if true then
        Right:
          copy()
      else Left(???)

  enum Error:
    case MismatchedPopulationSizes(proposers: Int, acceptors: Int)
    case MissingProposerPreferenceList(xs: NonEmptyChain[String])
    case MissingAcceptorPreferenceList(xs: NonEmptyChain[String])
    case IncompleteAcceptorsPreferenceList(proposer: String, acceptorKey: String)
    case IncompleteProposersPreferenceList(acceptor: String, proposerKey: String)
