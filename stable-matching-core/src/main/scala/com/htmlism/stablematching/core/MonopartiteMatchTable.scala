package com.htmlism.stablematching.core

import scala.collection.immutable.SortedMap
import scala.util.chaining.*

import cats.data.*
import cats.syntax.all.*

final case class MonopartiteMatchTable[A](
    members: NonEmptyList[A],
    matches: NonEmptyMap[A, NonEmptyList[A]]
)

object MonopartiteMatchTable:
  def build[A: Ordering](
      statefulTable: MonopartiteStatefulTable[A]
  ): Either[String, MonopartiteMatchTable[A]] =
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
