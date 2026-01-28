package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import weaver.*

object MonopartiteMatchingTableSuite extends FunSuite:
  test("Matcher requires an even population"):
    val oddPopulation =
      ListSet("a")

    val res =
      MonopartiteMatchingTable
        .build(oddPopulation, Map.empty[String, NonEmptyList[String]])

    matches(res):
      case Left(MonopartiteMatchingTable.ValidationError.UnsupportedPopulationSize(n)) =>
        expect.eql(oddPopulation.size, n)

  test("Matcher requires a preference list for every member"):
    val population =
      ListSet("a", "b", "c", "d")

    val res =
      MonopartiteMatchingTable
        .build(population, Map("a" -> NonEmptyList.of("b")))

    matches(res):
      case Left(MonopartiteMatchingTable.ValidationError.MissingPreferenceList(xs)) =>
        // error list is non-deterministic from input set
        expect.eql(Set("b", "c", "d"), xs.iterator.toSet)

  test("Matcher requires every member to be in every other preference list"):
    val population =
      ListSet("a", "b")

    val res =
      MonopartiteMatchingTable
        .build(
          population,
          Map(
            "a" -> NonEmptyList.of("b"),
            "b" -> NonEmptyList.of("c")
          )
        )

    matches(res):
      case Left(MonopartiteMatchingTable.ValidationError.IncompletePreferenceList(p, k)) =>
        expect.eql("a", p) and
          expect.eql("b", k)

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

    MonopartiteMatchingTable
      .build(
        population,
        preferences
      )

  test("Builds a matching table from valid input"):
    matches(buildFixture):
      case Right(table) =>
        expect.eql(6, table.members.size) and
          expect.eql(6, table.preferences.size)

  test("Can find a member able to propose"):
    matches(buildFixture):
      case Right(table) =>
        val res =
          table.findMemberAbleToPropose

        matches(res):
          case Some((proposer, acceptor)) =>
            expect.eql("a", proposer) and
              expect.eql("b", acceptor)
