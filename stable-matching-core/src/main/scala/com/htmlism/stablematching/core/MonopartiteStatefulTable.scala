package com.htmlism.stablematching.core

import scala.annotation.tailrec
import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import cats.syntax.all.*

final case class MonopartiteStatefulTable[A](
    members: ListSet[A],
    preferences: Map[A, NonEmptyList[A]],
    cells: Map[(A, A), MonopartiteStatefulTable.State]
):
  private def acceptorsAndStatesFor(proposer: A): NonEmptyList[(A, MonopartiteStatefulTable.State)] =
    preferences(proposer)
      .fproduct(a => cells((proposer, a)))

  /**
    * Finds the first member able to propose to another free member
    */
  def findMemberAbleToProposeFirstDate: Option[(A, A)] =
    members
      .foldLeft(Option.empty[(A, A)]):
        case (None, proposer) =>
          val acceptorsAndStates =
            acceptorsAndStatesFor(proposer)

          val hasFirstDate =
            acceptorsAndStates
              .exists: (_, state) =>
                state == MonopartiteStatefulTable.State.ProposesTo

          if hasFirstDate then None
          else
            val maybeAcceptor =
              acceptorsAndStates
                .find: (_, state) =>
                  state == MonopartiteStatefulTable.State.Free
                .map(_._1)

            maybeAcceptor
              .map(a => proposer -> a)

        case (foundPair @ Some(_), _) =>
          foundPair

  /**
    * Applies a symmetric proposal between two members
    */
  def applySymmetricProposal(proposer: A, acceptor: A): MonopartiteStatefulTable[A] =
    val updatedCells =
      cells
        .updated((proposer, acceptor), MonopartiteStatefulTable.State.ProposesTo)
        .updated((acceptor, proposer), MonopartiteStatefulTable.State.ProposedBy)

    val newTableWithDuplicateProposals =
      this.copy(cells = updatedCells)

    MonopartiteStatefulTable
      .trimDuplicateProposalsRecursively(newTableWithDuplicateProposals)

  /**
    * Applies a symmetric rejection between two members
    */
  def applySymmetricRejection(proposer: A, acceptor: A): MonopartiteStatefulTable[A] =
    val updatedCells =
      cells
        .updated((proposer, acceptor), MonopartiteStatefulTable.State.Rejects)
        .updated((acceptor, proposer), MonopartiteStatefulTable.State.RejectedBy)

    this.copy(cells = updatedCells)

  /**
    * Gets the state between a proposer and an acceptor
    */
  def getState(proposer: A, acceptor: A): MonopartiteStatefulTable.State =
    cells((proposer, acceptor))

object MonopartiteStatefulTable:
  def build[A: Order](
      members: ListSet[A],
      preferences: Map[A, NonEmptyList[A]]
  ): Either[ValidationError, MonopartiteStatefulTable[A]] =
    def validatePopulationSize =
      Either.cond(
        members.size % 2 == 0,
        members,
        ValidationError.UnsupportedPopulationSize(members.size)
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
                ValidationError.IncompletePreferenceList(p.toString, k.toString)
              )

    def genKeySide =
      for
        x <- members.iterator
        y <- preferences(x).iterator
      yield x -> y

    for
      pop <- validatePopulationSize

      _ <-
        pop
          .toList
          .traverse(validatePreferenceExists(preferences))
          .as(preferences)
          .toEither
          .leftMap(ValidationError.MissingPreferenceList(_))

      _ <- validateMemberIsInPreferences
    yield MonopartiteStatefulTable(
      members,
      preferences,
      genKeySide.map(k => k -> State.Free).toMap
    )

  @tailrec
  def trimDuplicateProposalsRecursively[A](table: MonopartiteStatefulTable[A]): MonopartiteStatefulTable[A] =
    val rejectionPairMaybe =
      table
        .members
        .foldLeft(Option.empty[(A, A)]):
          case (None, proposer) =>
            val proposals =
              table
                .acceptorsAndStatesFor(proposer)
                .collect:
                  case (acceptor, MonopartiteStatefulTable.State.ProposedBy) =>
                    acceptor

            proposals match
              case keeper :: firstRejection :: _ =>
                Some(proposer -> firstRejection)

              case _ =>
                None

          case (res @ Some(_), _) =>
            res

    rejectionPairMaybe match
      case Some(rejector, rejected) =>
        val newTable =
          table
            .applySymmetricRejection(rejector, rejected)

        trimDuplicateProposalsRecursively(newTable)

      case None =>
        table

  @tailrec
  def trimLessDesirableMatchesRecursively[A](table: MonopartiteStatefulTable[A]): MonopartiteStatefulTable[A] =
    val rejectionPairMaybe =
      table
        .members
        .foldLeft(Option.empty[(A, A)]): (state, p) =>
          state match
            case None =>
              val acceptorsAndStates =
                table
                  .acceptorsAndStatesFor(p)

              val potentialRejected =
                acceptorsAndStates
                  .foldLeft(Option.empty[A]):
                    case (acc, (acceptor, state)) =>
                      state match
                        case MonopartiteStatefulTable.State.ProposesTo =>
                          None

                        case MonopartiteStatefulTable.State.ProposedBy =>
                          None

                        case MonopartiteStatefulTable.State.Free =>
                          acc match
                            case currentRejection @ Some(_) =>
                              currentRejection

                            case None =>
                              acceptor.some

                        case MonopartiteStatefulTable.State.Rejects =>
                          acc

                        case MonopartiteStatefulTable.State.RejectedBy =>
                          acc

              potentialRejected
                .tupleLeft(p)

            case res @ Some(_) =>
              res

    rejectionPairMaybe match
      case Some(rejector, rejected) =>
//        println(s"$rejector rejects $rejected")

        val newTable =
          table
            .applySymmetricRejection(rejector, rejected)

        trimLessDesirableMatchesRecursively(newTable)

      case None =>
        table

  enum ValidationError:
    case UnsupportedPopulationSize(n: Int)
    case MissingPreferenceList(xs: NonEmptyChain[String])

    /**
      * Member $member is not reflected in preference list of member $preferenceKey
      */
    case IncompletePreferenceList(member: String, preferenceKey: String)

  enum State:
    case Free
    case ProposesTo
    case ProposedBy
    case Rejects
    case RejectedBy

  object State:
    given Eq[State] =
      Eq.fromUniversalEquals
