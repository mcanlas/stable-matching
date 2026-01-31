package com.htmlism.stablematching.core

import scala.collection.immutable.ListMap
import scala.util.chaining.*

import cats.data.NonEmptyList
import cats.syntax.all.*

final case class BipartiteStatefulTable[P, A](
    proposerPreferences: ListMap[P, NonEmptyList[A]],
    acceptorPreferences: ListMap[A, NonEmptyList[P]],
    proposerStates: Map[(P, A), BipartiteStatefulTable.State],
    acceptorStates: Map[(A, P), BipartiteStatefulTable.State]
):
  def findProposerAbleToPropose: Option[P] =
    proposerPreferences
      .foldLeft(Option.empty[P]):
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
  given [A, B]: Tabular[BipartiteStatefulTable[A, B]] with
    def headers(table: BipartiteStatefulTable[A, B]): List[String] =
      val numCols =
        (1 to table.proposerPreferences.size)
          .map(n => s"#$n")

      (Iterator("Proposers") ++ numCols ++ Iterator("Acceptors") ++ numCols).toList

    def rows(table: BipartiteStatefulTable[A, B]): List[List[String]] =
      val zippedKeys =
        table
          .proposerPreferences
          .keys
          .zip(table.acceptorPreferences.keys)
          .toList

      zippedKeys
        .map: (p, a) =>
          val proposerCells =
            table
              .proposerPreferences(p)
              .map: a =>
                s"$a " + table.proposerStates(p -> a).pipe(stateToString)
              .toList

          val acceptorCells =
            table
              .acceptorPreferences(a)
              .map: p =>
                s"$p " + table.acceptorStates(a -> p).pipe(stateToString)
              .toList

          List(p.toString) ++
            proposerCells ++
            List(a.toString) ++
            acceptorCells

  private def stateToString(state: State): String =
    state match
      case State.Free =>
        "➖"
      case State.ProposesTo =>
        "😸"
      case State.ProposedBy =>
        "🤔"
      case State.Rejects =>
        "❌"
      case State.RejectedBy =>
        "👻"

  enum State:
    case Free
    case ProposesTo
    case ProposedBy
    case Rejects
    case RejectedBy
