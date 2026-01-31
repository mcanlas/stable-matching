package com.htmlism.stablematching.core

import cats.*
import weaver.*

object MonopartiteMatchTableSuite extends FunSuite:
  test("Can build from a stateful table"):
    matches(MonoFixtures.buildPopSixEmptyTable):
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
        statefulTable <- MonoFixtures.buildPopSixEmptyTable

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
        statefulTable <- MonoFixtures.buildPopSixEmptyTable

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
        statefulTable <- MonoFixtures.buildPopSixEmptyTable

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
        statefulTable <- MonoFixtures.buildPopSixEmptyTable

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

  test("Unstable"):
    val prog =
      for
        statefulTable <- MonoFixtures.buildUnstablePopFourEmptyTable

        tableAfterIterations =
          (0 until 4)
            .foldLeft(statefulTable): (acc, _) =>
              acc.findMemberAbleToProposeFirstDate match
                case Some((proposer, acceptor)) =>
                  acc.applySymmetricProposal(proposer, acceptor)

                case None =>
                  acc

        tableAfterRejections =
          MonopartiteStatefulTable
            .trimLessDesirableMatchesRecursively(tableAfterIterations)

        _ = println:
          MonopartiteStatefulTablePrinter
            .generateMarkdown(tableAfterRejections)

        matchTable <-
          MonopartiteMatchTable
            .build(tableAfterRejections)

        _ = println:
          MonopartiteMatchTablePrinter
            .generateMarkdown(matchTable)
      yield ()

    matches(prog):
      case Left(_) =>
        success
