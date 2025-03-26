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
  ): WriterT[[X] =>> Either[Error, X], Chain[String], List[String]] =
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

      res <- WriterT.put(Nil)(Chain("nil"))
    yield res

  enum Error:
    case MismatchedPopulationSizes(proposers: Int, acceptors: Int)
    case MissingProposerPreferenceList(xs: NonEmptyChain[String])
    case MissingAcceptorPreferenceList(xs: NonEmptyChain[String])
    case IncompleteAcceptorsPreferenceList(proposer: String, acceptorKey: String)
    case IncompleteProposersPreferenceList(acceptor: String, proposerKey: String)
