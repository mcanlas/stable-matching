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
          .pipe(Either.fromOption(_, s"No matches for first member $x"))

      yMatches <-
        matches(y)
          .pipe(Either.fromOption(_, s"No matches for second member $y"))

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

  def findCycle: Either[String, ReverseCycle[A]] =
    FlatMap[Res]
      .tailRecM(Option.empty[NonEmptyList[A]])(findReverseCycleStep)
      .map(xs => ReverseCycle(xs))

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
  given [A]: Tabular[MonopartiteMatchTable[A]] with
    def headers(x: MonopartiteMatchTable[A]): List[String] =
      List("Roommate", "Matches")

    def rows(table: MonopartiteMatchTable[A]): List[List[String]] =
      table
        .members
        .iterator
        .map: m =>
          val matchesStr =
            table
              .matches(m)
              .map: nel =>
                nel
                  .map(_.toString)
                  .mkString_(", ")
              .getOrElse("")

          List(m.toString, matchesStr)
        .toList

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
              .pipe(
                Either.fromOption(_, s"Member $p has no matches in stateful table during match table build process")
              )

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

  final case class ReverseCycle[A](xs: NonEmptyList[A])

  def rejectReverseCycle[A](
      table: MonopartiteMatchTable[A],
      cycle: ReverseCycle[A]
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

  def findAndDeleteCycleStep[A](
      table: MonopartiteMatchTable[A]
  ): Res[Either[MonopartiteMatchTable[A], MonopartiteMatchTable[A]]] =
    table.findCycle match
      case Right(cycle) =>
        rejectReverseCycle(table, cycle)
          .map(_.asLeft) // means keep going

      case Left(_) =>
        table
          .asRight // means stop
          .asRight // means error free

  def findAndDeleteCycles[A](
      table: MonopartiteMatchTable[A]
  ): Res[MonopartiteMatchTable[A]] =
    FlatMap[Res]
      .tailRecM(table)(findAndDeleteCycleStep)
