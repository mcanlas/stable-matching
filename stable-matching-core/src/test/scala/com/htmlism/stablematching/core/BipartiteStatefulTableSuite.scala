package com.htmlism.stablematching.core

import cats.*
import cats.syntax.all.*
import weaver.*

object BipartiteStatefulTableSuite extends FunSuite:
//  test("Matcher requires an even population"):
//    val oddPopulation =
//      ListSet("a")
//
//    val res =
//      MonopartiteStatefulTable
//        .build(oddPopulation, Map.empty[String, NonEmptyList[String]])
//
//    matches(res):
//      case Left(MonopartiteStatefulTable.ValidationError.UnsupportedPopulationSize(n)) =>
//        expect.eql(oddPopulation.size, n)
//
//  test("Matcher requires a preference list for every member"):
//    val population =
//      ListSet("a", "b", "c", "d")
//
//    val res =
//      MonopartiteStatefulTable
//        .build(population, Map("a" -> NonEmptyList.of("b")))
//
//    matches(res):
//      case Left(MonopartiteStatefulTable.ValidationError.MissingPreferenceList(xs)) =>
//        // error list is non-deterministic from input set
//        expect.eql(Set("b", "c", "d"), xs.iterator.toSet)
//
//  test("Matcher requires every member to be in every other preference list"):
//    val population =
//      ListSet("a", "b")
//
//    val res =
//      MonopartiteStatefulTable
//        .build(
//          population,
//          Map(
//            "a" -> NonEmptyList.of("b"),
//            "b" -> NonEmptyList.of("c")
//          )
//        )
//
//    matches(res):
//      case Left(MonopartiteStatefulTable.ValidationError.IncompletePreferenceList(p, k)) =>
//        expect.eql("a", p) and
//          expect.eql("b", k)
//
//  test("Builds a matching table from valid input"):
//    matches(Fixtures.buildPopSixEmptyTable):
//      case Right(table) =>
//        expect.eql(6, table.members.size) and
//          expect.eql(6, table.preferences.size)

  test("Can find an available proposer/acceptor pair"):
    val prog =
      for
        table <- BiFixtures.buildFiveAndFive.asRight

        _ = println(MarkdownTablePrinter.generateMarkdown(table))

        pair <- table
          .findProposalPair
          .toRight("no member able to propose found")
      yield pair

    matches(prog):
      case Right((p, a)) =>
        expect.eql("A", p) and
          expect.eql("O", a)

  test("Can apply a symmetric proposal"):
    val prog =
      for
        table <- BiFixtures.buildFiveAndFive.asRight

        (p, a) <- table
          .findProposalPair
          .toRight("no member able to propose found")

        tableAfterProposal =
          table.applySymmetricProposal(p, a)

        _ = println(MarkdownTablePrinter.generateMarkdown(tableAfterProposal))
      yield (p, a, tableAfterProposal)

    matches(prog):
      case Right((p, a, tbl)) =>
        expect.same(BipartiteStatefulTable.State.ProposesTo, tbl.proposerStates(p -> a)) and
          expect.same(BipartiteStatefulTable.State.ProposedBy, tbl.acceptorStates(a -> p))

  test("Can do multiple iterations, with last rejection"):
    val prog =
      for
        table <- BiFixtures.buildFiveAndFive.asRight

        tableAfterFourProposals =
          TestUtils
            .applyTimes[Id, BipartiteStatefulTable[String, String]](table)(4)(_.applyProposalStep)

        _ = println(MarkdownTablePrinter.generateMarkdown(tableAfterFourProposals))
      yield ()

    matches(prog):
      case Right(_) =>
        success

  test("Can run recursively until stable"):
    val prog =
      for
        table <- BiFixtures.buildFiveAndFive.asRight

        stableMatch =
          table.applyUntilStable

        _ = println(MarkdownTablePrinter.generateMarkdown(stableMatch))
      yield ()

    matches(prog):
      case Right(_) =>
        success

  test("Results are different when roles are reversed"):
    val prog =
      for
        table <- BiFixtures.buildFiveAndFiveReverse.asRight

        stableMatch =
          table.applyUntilStable

        _ = println(MarkdownTablePrinter.generateMarkdown(stableMatch))
      yield ()

    matches(prog):
      case Right(_) =>
        success
