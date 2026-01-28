package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import cats.syntax.all.*

case class MonopartiteMatchingTable[A: Order](
    members: ListSet[A],
    preferences: Map[A, NonEmptyList[A]],
    cells: Map[(A, A), MonopartiteMatchingTable.State]
):
  /**
    * Finds the first member able to propose to another free member
    */
  def findMemberAbleToProposeFirstDate: Option[(A, A)] =
    members
      .foldLeft(Option.empty[(A, A)]):
        case (None, proposer) =>
          val acceptorsAndStates =
            preferences(proposer)
              .fproduct(a => cells((proposer, a)))

          val hasFirstDate =
            acceptorsAndStates
              .exists: (_, state) =>
                state == MonopartiteMatchingTable.State.ProposesTo

          if hasFirstDate then None
          else
            val maybeAcceptor =
              acceptorsAndStates
                .find: (_, state) =>
                  state == MonopartiteMatchingTable.State.Free
                .map(_._1)

            maybeAcceptor
              .map(a => proposer -> a)

        case (foundPair @ Some(_), _) =>
          foundPair

  /**
    * Applies a symmetric proposal between two members
    */
  def applySymmetricProposal(proposer: A, acceptor: A): MonopartiteMatchingTable[A] =
    val updatedCells =
      cells
        .updated((proposer, acceptor), MonopartiteMatchingTable.State.ProposesTo)
        .updated((acceptor, proposer), MonopartiteMatchingTable.State.ProposedBy)

    this.copy(cells = updatedCells)

  /**
    * Gets the state between a proposer and an acceptor
    */
  def getState(proposer: A, acceptor: A): MonopartiteMatchingTable.State =
    cells((proposer, acceptor))

// TODO next: method for find first unproposed
// TODO next: functional white loops
object MonopartiteMatchingTable:
  def build[A: Order](
      members: ListSet[A],
      preferences: Map[A, NonEmptyList[A]]
  ): Either[ValidationError, MonopartiteMatchingTable[A]] =
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
    yield MonopartiteMatchingTable(
      members,
      preferences,
      genKeySide.map(k => k -> State.Free).toMap
    )

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
