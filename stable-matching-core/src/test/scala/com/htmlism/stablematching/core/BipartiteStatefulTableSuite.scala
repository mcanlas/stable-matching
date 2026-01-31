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

  test("Can find a member able to propose"):
    val prog =
      for
        table <- BiFixtures.buildFiveAndFive.asRight

        _ = println(MarkdownTablePrinter.generateMarkdown(table))

        firstProposer <- table
          .findProposerAbleToPropose
          .toRight("no member able to propose found")
      yield firstProposer

    matches(prog):
      case Right(p) =>
        expect.eql("A", p)

  test("Can apply a symmetric proposal"):
    matches(MonoFixtures.buildPopSixEmptyTable):
      case Right(table) =>
        matches(table.findMemberAbleToProposeFirstDate):
          case Some((proposer, acceptor)) =>
            val newTable =
              table
                .applySymmetricProposal(proposer, acceptor)

            expect.eql(MonopartiteStatefulTable.State.ProposesTo, newTable.getState(proposer, acceptor)) and
              expect.eql(MonopartiteStatefulTable.State.ProposedBy, newTable.getState(acceptor, proposer))

  test("Can do multiple iterations"):
    // https://www.youtube.com/watch?v=5QLxAp8mRKo
    val journey = List(
      ("a", "b"),
      ("b", "d"),
      ("c", "d"),
      ("b", "e"), // this is after duplicate proposals are trimmed
      ("d", "f"),
      ("e", "f"), // e also proposes to f but immediately gets trimmed after
      ("e", "c"),
      ("f", "a")
    )

    matches(MonoFixtures.buildPopSixEmptyTable):
      case Right(table) =>
        journey
          .foldLeft(table -> success):
            case ((tbl, ret), (expectedProposer, expectedAcceptor)) =>
              println(s"Expecting: $expectedProposer -> $expectedAcceptor")

              tbl.findMemberAbleToProposeFirstDate match
                case Some((foundProposer, foundAcceptor)) =>
                  println(s"Found: $foundProposer -> $foundAcceptor")

                  val newRet =
                    ret and
                      expect.eql(expectedProposer, foundProposer) and
                      expect.eql(expectedAcceptor, foundAcceptor)

                  val newTable =
                    tbl.applySymmetricProposal(foundProposer, foundAcceptor)

                  println(MonopartiteStatefulTablePrinter.generateMarkdown(newTable))

                  newTable -> newRet

                case None =>
                  tbl -> failure("could not find member able to propose")
          ._2

  test("Can reject less desirable matches"):
    matches(MonoFixtures.buildPopSixEmptyTable):
      case Right(table) =>
        val tableAfterIterations =
          // TODO replace with loop until
          (0 until 8)
            .foldLeft(table): (acc, _) =>
              acc.findMemberAbleToProposeFirstDate match
                case Some((proposer, acceptor)) =>
                  acc.applySymmetricProposal(proposer, acceptor)

                case None =>
                  acc

        println:
          MonopartiteStatefulTablePrinter
            .generateMarkdown(tableAfterIterations)

        val tableAfterRejections =
          MonopartiteStatefulTable
            .trimLessDesirableMatchesRecursively(tableAfterIterations)

        println:
          MonopartiteStatefulTablePrinter
            .generateMarkdown(tableAfterRejections)

        expect.eql(MonopartiteStatefulTable.State.Rejects, tableAfterRejections.getState("a", "c")) and
          expect.eql(MonopartiteStatefulTable.State.Rejects, tableAfterRejections.getState("a", "e")) and
          expect.eql(MonopartiteStatefulTable.State.RejectedBy, tableAfterRejections.getState("a", "d"))
