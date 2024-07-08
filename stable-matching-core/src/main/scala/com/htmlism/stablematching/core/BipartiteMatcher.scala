package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import cats.syntax.all.*

object BipartiteMatcher:
  def createMatches[A: Eq, B: Eq](
      proposers: Set[A],
      acceptors: Set[B],
      proposerPreferences: Map[A, NonEmptyList[B]],
      acceptorPreferences: Map[B, NonEmptyList[A]],
      proposerOrder: Order[A],
      acceptorOrder: Order[B]
  ): Either[Error, List[String]] =
    def validatePopulationSizes(xs: Set[A], ys: Set[B]) =
      Either.cond(
        xs.size == ys.size,
        (),
        Error.MismatchedPopulationSizes(xs.size, ys.size)
      )

    def validateProposersAreInPreferences =
      ().asRight

    def validateAcceptorsAreInPreferences =
      ().asRight

    for
      _ <- validatePopulationSizes(proposers, acceptors)

      _ <- proposers
        .toList
        .traverse: p =>
          if proposerPreferences.contains(p) then ().validNec
          else p.toString.invalidNec
        .toEither
        .leftMap(Error.MissingProposerPreferenceList(_))

      _ <- acceptors
        .toList
        .traverse: p =>
          if acceptorPreferences.contains(p) then ().validNec
          else p.toString.invalidNec
        .toEither
        .leftMap(Error.MissingAcceptorPreferenceList(_))

      _ <- validateProposersAreInPreferences

      _ <- validateAcceptorsAreInPreferences
    yield Nil

  enum Error:
    case MismatchedPopulationSizes(proposers: Int, acceptors: Int)
    case MissingProposerPreferenceList(xs: NonEmptyChain[String])
    case MissingAcceptorPreferenceList(xs: NonEmptyChain[String])
    case IncompleteAcceptorsPreferenceList(proposer: String, acceptorKey: String)
    case IncompleteProposersPreferenceList(acceptor: String, proposerKey: String)
