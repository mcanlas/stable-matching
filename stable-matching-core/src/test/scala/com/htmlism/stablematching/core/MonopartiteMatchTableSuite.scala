package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import weaver.*

object MonopartiteMatchTableSuite extends FunSuite:
  private def buildFixture =
    // https://www.youtube.com/watch?v=5QLxAp8mRKo

    val population =
      ListSet("a", "b", "c", "d", "e", "f")

    val preferences =
      Map(
        "a" -> NonEmptyList.of("b", "d", "f", "c", "e"),
        "b" -> NonEmptyList.of("d", "e", "f", "a", "c"),
        "c" -> NonEmptyList.of("d", "e", "f", "a", "b"),
        "d" -> NonEmptyList.of("f", "c", "a", "e", "b"),
        "e" -> NonEmptyList.of("f", "c", "d", "b", "a"),
        "f" -> NonEmptyList.of("a", "b", "d", "c", "e")
      )

    MonopartiteStatefulTable
      .build(
        population,
        preferences
      )

  test("Can build from a stateful table"):
    matches(buildFixture):
      case Right(table) =>
        val tableAfterIterations =
          (0 until 8)
            .foldLeft(table): (acc, _) =>
              acc.findMemberAbleToProposeFirstDate match
                case Some((proposer, acceptor)) =>
                  acc.applySymmetricProposal(proposer, acceptor)

                case None =>
                  acc

        val tableAfterRejections =
          MonopartiteStatefulTable
            .trimLessDesirableMatchesRecursively(tableAfterIterations)

        println:
          MonopartiteStatefulTablePrinter
            .generateMarkdown(tableAfterRejections)

        val matchTableRes =
          MonopartiteMatchTable
            .build(tableAfterRejections)

        matches(matchTableRes):
          case Right(matchTable) =>
            println:
              MonopartiteMatchTablePrinter
                .generateMarkdown(matchTable)

            expect(true)

  test("Can find a cycle"):
    val prog =
      for
        statefulTable <- buildFixture

        tableAfterIterations =
          (0 until 8)
            .foldLeft(statefulTable): (acc, _) =>
              acc.findMemberAbleToProposeFirstDate match
                case Some((proposer, acceptor)) =>
                  acc.applySymmetricProposal(proposer, acceptor)

                case None =>
                  acc

        tableAfterRejections =
          MonopartiteStatefulTable
            .trimLessDesirableMatchesRecursively(tableAfterIterations)

        matchTable <-
          MonopartiteMatchTable
            .build(tableAfterRejections)

        _ = println:
          MonopartiteMatchTablePrinter
            .generateMarkdown(matchTable)

        ret <-
          matchTable.findCycle

        _ = println:
          ret
      yield ()

    matches(prog):
      case Right(_) =>
        success

  test("Can delete a cycle"):
    val prog =
      for
        statefulTable <- buildFixture

        tableAfterIterations =
          (0 until 8)
            .foldLeft(statefulTable): (acc, _) =>
              acc.findMemberAbleToProposeFirstDate match
                case Some((proposer, acceptor)) =>
                  acc.applySymmetricProposal(proposer, acceptor)

                case None =>
                  acc

        tableAfterRejections =
          MonopartiteStatefulTable
            .trimLessDesirableMatchesRecursively(tableAfterIterations)

        matchTable <-
          MonopartiteMatchTable
            .build(tableAfterRejections)

        _ = println:
          MonopartiteMatchTablePrinter
            .generateMarkdown(matchTable)

        cycle <-
          matchTable.findCycle

        tableAfterCycleRemoval <-
          MonopartiteMatchTable
            .rejectReverseCycle(matchTable, cycle)

        _ = println:
          MonopartiteMatchTablePrinter
            .generateMarkdown(tableAfterCycleRemoval)
      yield ()

    matches(prog):
      case Right(_) =>
        success

  test("Can find and delete a cycle"):
    val prog =
      for
        statefulTable <- buildFixture

        tableAfterIterations =
          (0 until 8)
            .foldLeft(statefulTable): (acc, _) =>
              acc.findMemberAbleToProposeFirstDate match
                case Some((proposer, acceptor)) =>
                  acc.applySymmetricProposal(proposer, acceptor)

                case None =>
                  acc

        tableAfterRejections =
          MonopartiteStatefulTable
            .trimLessDesirableMatchesRecursively(tableAfterIterations)

        matchTable <-
          MonopartiteMatchTable
            .build(tableAfterRejections)

        cycle <-
          matchTable.findCycle

        tableAfterCycleRemovalStep <-
          MonopartiteMatchTable
            .findAndDeleteCycleStep(matchTable)

        tableAfterCycleRemoval <-
          tableAfterCycleRemovalStep match
            case Right(t)      => Left("too early, not correct")
            case Left(oneEval) => Right(oneEval)

        _ = println:
          MonopartiteMatchTablePrinter
            .generateMarkdown(tableAfterCycleRemoval)
      yield ()

    matches(prog):
      case Right(_) =>
        success

  test("Can find and delete two cycles recursively"):
    val prog =
      for
        statefulTable <- buildFixture

        tableAfterIterations =
          (0 until 8)
            .foldLeft(statefulTable): (acc, _) =>
              acc.findMemberAbleToProposeFirstDate match
                case Some((proposer, acceptor)) =>
                  acc.applySymmetricProposal(proposer, acceptor)

                case None =>
                  acc

        tableAfterRejections =
          MonopartiteStatefulTable
            .trimLessDesirableMatchesRecursively(tableAfterIterations)

        matchTable <-
          MonopartiteMatchTable
            .build(tableAfterRejections)

        cycle <-
          matchTable.findCycle

        tableAfterCycleRemovalStep <-
          MonopartiteMatchTable
            .findAndDeleteCycleStep(matchTable)

        tableAfterCycleRemovals <-
          MonopartiteMatchTable
            .findAndDeleteCycles(matchTable)

        _ = println:
          MonopartiteMatchTablePrinter
            .generateMarkdown(tableAfterCycleRemovals)
      yield ()

    matches(prog):
      case Right(_) =>
        success
