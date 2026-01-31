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
  import BipartiteStatefulTable.*

  def findProposalPair: Option[(P, A)] =
    proposerPreferences
      .foldLeft(Option.empty[(P, A)]):
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
              else
                val firstAcceptorFree =
                  as
                    .find(a => proposerStates((p, a)) == BipartiteStatefulTable.State.Free)

                firstAcceptorFree
                  .tupleLeft(p)

  /**
    * Applies a symmetric proposal between two members
    */
  def applySymmetricProposal(proposer: P, acceptor: A): BipartiteStatefulTable[P, A] =
    val updatedProposerStates =
      proposerStates
        .updated((proposer, acceptor), State.ProposesTo)

    val updatedAcceptorStates =
      acceptorStates
        .updated((acceptor, proposer), State.ProposedBy)

    copy(
      proposerStates = updatedProposerStates,
      acceptorStates = updatedAcceptorStates
    )
      .rejectDuplicateProposalsAt(acceptor)

  private def rejectDuplicateProposalsAt(a: A): BipartiteStatefulTable[P, A] =
    val proposals =
      acceptorPreferences(a)
        .filter(p => acceptorStates((a, p)) == State.ProposedBy)

    assert(proposals.size <= 2, s"More than two proposals found for acceptor $a")

    proposals match
      case keeper :: dropped :: Nil =>
        val updatedProposerStates =
          proposerStates
            .updated((dropped, a), State.RejectedBy)

        val updatedAcceptorStates =
          acceptorStates
            .updated((a, dropped), State.Rejects)

        copy(
          proposerStates = updatedProposerStates,
          acceptorStates = updatedAcceptorStates
        )

      // zero or one proposals
      case _ =>
        this

  def applyProposalStep: BipartiteStatefulTable[P, A] =
    findProposalPair match
      case None =>
        this

      case Some((p, a)) =>
        applySymmetricProposal(p, a)

//  /**
//   * Applies a symmetric rejection between two members
//   */
//  def applySymmetricRejection(proposer: P, acceptor: A): BipartiteStatefulTable[P, A] =
//    val updatedCells =
//      cells
//        .updated((proposer, acceptor), BipartiteStatefulTable.State.Rejects)
//        .updated((acceptor, proposer), BipartiteStatefulTable.State.RejectedBy)
//
//    this.copy(cells = updatedCells)

object BipartiteStatefulTable:
  given [P, A]: Tabular[BipartiteStatefulTable[P, A]] with
    def headers(table: BipartiteStatefulTable[P, A]): List[String] =
      val numCols =
        (1 to table.proposerPreferences.size)
          .map(n => s"#$n")

      (Iterator("Proposers") ++ numCols ++ Iterator("Acceptors") ++ numCols).toList

    def rows(table: BipartiteStatefulTable[P, A]): List[List[String]] =
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

  def applyProposalStepId[P, A](
      x: BipartiteStatefulTable[P, A]
  ): Either[BipartiteStatefulTable[P, A], BipartiteStatefulTable[P, A]] =
    x.findProposalPair match
      case None =>
        x.asRight

      case Some((p, a)) =>
        x
          .applySymmetricProposal(p, a)
          .asLeft
