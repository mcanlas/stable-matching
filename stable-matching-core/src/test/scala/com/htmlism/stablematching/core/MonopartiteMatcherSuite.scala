package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import weaver.*

object MonopartiteMatcherSuite extends FunSuite:
  test("Matcher requires an even population"):
    val oddPopulation =
      ListSet("a")

    val res =
      MonopartiteMatcher
        .createMatches(oddPopulation, Map.empty[String, NonEmptyList[String]])
        .run

    matches(res):
      case Left(MonopartiteMatcher.Error.UnsupportedPopulationSize(n)) =>
        expect.eql(oddPopulation.size, n)

  test("Matcher requires a preference list for every member"):
    val population =
      ListSet("a", "b", "c", "d")

    val res =
      MonopartiteMatcher
        .createMatches(population, Map("a" -> NonEmptyList.of("b")))
        .run

    matches(res):
      case Left(MonopartiteMatcher.Error.MissingPreferenceList(xs)) =>
        // error list is non-deterministic from input set
        expect.eql(Set("b", "c", "d"), xs.iterator.toSet)

  test("Matcher requires every member to be in every other preference list"):
    val population =
      ListSet("a", "b")

    val res =
      MonopartiteMatcher
        .createMatches(
          population,
          Map(
            "a" -> NonEmptyList.of("b"),
            "b" -> NonEmptyList.of("c")
          )
        )
        .run

    matches(res):
      case Left(MonopartiteMatcher.Error.IncompletePreferenceList(p, k)) =>
        expect.eql("a", p) and
          expect.eql("b", k)

  test("Empty input yields empty output"):
    val population =
      ListSet.empty[String]

    val res =
      MonopartiteMatcher
        .createMatches(population, Map.empty[String, NonEmptyList[String]])
        .run

    whenSuccess(res): (_, matching) =>
      expect(matching.mapping.isEmpty)

  test("A trivial population of two has a trivial answer"):
    val population =
      ListSet("a", "b")

    val preferences =
      Map(
        "a" -> NonEmptyList.of("b"),
        "b" -> NonEmptyList.of("a")
      )

    val res =
      MonopartiteMatcher
        .createMatches(population, preferences)
        .run

    whenSuccess(res): (_, matching) =>
      expect(matching.mapping.size == 2)

  test("A population of four is stable".ignore):
    val population =
      ListSet("a", "b", "c", "d")

    val preferences =
      Map(
        "a" -> NonEmptyList.of("b", "c", "d"),
        "b" -> NonEmptyList.of("a", "c", "d"),
        "c" -> NonEmptyList.of("a", "b", "d"),
        "d" -> NonEmptyList.of("a", "b", "c"),
      )

    val res =
      MonopartiteMatcher
        .createMatches(population, preferences)
        .run

    whenSuccess(res): (_, matching) =>
      expect(matching.mapping.size == 4)
