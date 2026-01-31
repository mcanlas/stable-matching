package com.htmlism.stablematching.core

import scala.collection.immutable.ListMap

import cats.data.NonEmptyList
import cats.syntax.all.*

final case class BipartiteStatefulTable[A, B](
    proposerPreferences: ListMap[A, NonEmptyList[B]],
    acceptorPreferences: ListMap[B, NonEmptyList[A]],
    proposerStates: Map[(A, B), BipartiteStatefulTable.State],
    acceptorStates: Map[(B, A), BipartiteStatefulTable.State]
):
  def findProposerAbleToPropose: Option[A] =
    proposerPreferences
      .foldLeft(Option.empty[A]):
        case (acc, (p, as)) =>
          acc match
            case Some(x) =>
              x.some

            case None =>
              val hasProposal =
                as
                  .filter(a => proposerStates((p, a)) == BipartiteStatefulTable.State.ProposesTo)
                  .nonEmpty

              if hasProposal then None
              else p.some

object BipartiteStatefulTable:
  enum State:
    case Free
    case ProposesTo
    case ProposedBy
    case Rejects
    case RejectedBy
