package com.htmlism.stablematching.core

import scala.collection.immutable.SortedMap
import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.syntax.all.*

final case class MonopartiteMatchTable[A](
    members: NonEmptyList[A],
    matches: NonEmptyMap[A, NonEmptyList[A]]
):
  import MonopartiteMatchTable.*

  def applySymmetricRejection(x: A, y: A): Res[MonopartiteMatchTable[A]] =
    for
      xMatches <-
        matches(x)
          .pipe(Either.fromOption(_, s"No matches for member $x"))

      yMatches <-
        matches(y)
          .pipe(Either.fromOption(_, s"No matches for member $x"))

      xMatchesWithoutY <-
        xMatches
          .filterNot(_ == y)
          .pipe(NonEmptyList.fromList)
          .pipe(Either.fromOption(_, s"Removing $y left no matches for $x"))

      yMatchesWithoutX <-
        yMatches
          .filterNot(_ == x)
          .pipe(NonEmptyList.fromList)
          .pipe(Either.fromOption(_, s"Removing $x left no matches for $y"))

      updatedMatches =
        matches
          .add(x, xMatchesWithoutY)
          .add(y, yMatchesWithoutX)
    yield this.copy(matches = updatedMatches)

  def findCycle: Either[String, RemoveCycleState.ReverseCycle[A]] =
    FlatMap[Res]
      .tailRecM(Option.empty[NonEmptyList[A]])(findReverseCycleStep)
      .map(xs => RemoveCycleState.ReverseCycle(xs))

  private def findReverseCycleStep(
      acc: Option[NonEmptyList[A]]
  ): Res[Either[Option[NonEmptyList[A]], NonEmptyList[A]]] =
    acc match
      case None =>
        // left means keep going
        findCycleSeed
          .map(x => NonEmptyList.one(x).some.asLeft)

      case Some(xs) =>
        // cycle is complete
        if xs.head == xs.last && xs.length > 1 then
          xs
            .asRight // means stop processing
            .asRight // means error free
        else
          val lastMatchMaybe =
            for matchesAtHead <-
                matches(xs.head)
                  .pipe(Either.fromOption(_, s"No matches for head ${xs.head}"))
            yield matchesAtHead.last

          lastMatchMaybe
            .map(sm => (sm :: xs).some.asLeft)

  private def findCycleSeed: Res[A] =
    for
      membersWithMatchesList <- members
        .traverse: p =>
          matches(p)
            .pipe(Either.fromOption(_, s"No matches for member $p"))
            .tupleLeft(p)

      cycleCandidates =
        membersWithMatchesList
          .filter(_._2.size > 1)

      proposerWithCycle <-
        cycleCandidates match
          case Nil =>
            Left("No cycle candidates")

          case (proposer, _) :: _ =>
            proposer.asRight
    yield proposerWithCycle

object MonopartiteMatchTable:
  private type Res[A] =
    Either[String, A]

  def build[A: Ordering](
      statefulTable: MonopartiteStatefulTable[A]
  ): Res[MonopartiteMatchTable[A]] =
    val memberMapRes =
      statefulTable
        .members
        .toList
        .traverse: p =>
          val validAcceptors =
            statefulTable
              .preferences(p)
              .fproduct(a => statefulTable.cells((p, a)))
              .filter: (_, state) =>
                state != MonopartiteStatefulTable.State.Rejects &&
                  state != MonopartiteStatefulTable.State.RejectedBy
              .map(_._1)

          val nelValidAcceptorsRes =
            NonEmptyList
              .fromList(validAcceptors)
              .pipe(Either.fromOption(_, s"No matches for member $p"))

          nelValidAcceptorsRes
            .tupleLeft(p)

    for
      memberMap <- memberMapRes

      nonEmptyMemberMap <- NonEmptyMap
        .fromMap(SortedMap.from(memberMap))
        .pipe(Either.fromOption(_, "Input map was non-empty"))

      nelMembers <-
        NonEmptyList
          .fromList(statefulTable.members.toList)
          .pipe(Either.fromOption(_, "Member list was empty"))
    yield MonopartiteMatchTable(
      nelMembers,
      nonEmptyMemberMap
    )

  enum RemoveCycleState[A]:
    case object SearchingForCycle
    case ReverseCycle(xs: NonEmptyList[A])
    case object NoCycleFound

  def rejectReverseCycle[A](
      table: MonopartiteMatchTable[A],
      cycle: RemoveCycleState.ReverseCycle[A]
  ): Res[MonopartiteMatchTable[A]] =
    for
      _ <- Either.cond(
        cycle.xs.size % 2 != 0,
        (),
        "Reverse cycle size must be odd"
      )

      newTable <-
        FlatMap[Res]
          .tailRecM((table, cycle.xs))(rejectReverseCycleStep)
    yield newTable

  private type RejectState[A] =
    (MonopartiteMatchTable[A], NonEmptyList[A])

  private def rejectReverseCycleStep[A](acc: RejectState[A]): Res[Either[RejectState[A], MonopartiteMatchTable[A]]] =
    val (table, rejects) = acc

    rejects match
      case NonEmptyList(_, Nil) =>
        table.asRight.asRight

      case NonEmptyList(one, two :: remaining) =>
        for
          newTable <-
            table
              .applySymmetricRejection(one, two)

          remainingNel <-
            NonEmptyList
              .fromList(remaining)
              .pipe(Either.fromOption(_, "No remaining members in cycle rejection"))
        yield (newTable -> remainingNel).asLeft // keep going
