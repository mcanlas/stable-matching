package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import weaver.*

object MonopartiteMatcherSuite extends FunSuite:
  test("Matcher requires an even population"):
    val oddPopulation =
      Set("a")

    val res =
      MonopartiteMatcher
        .createMatches(oddPopulation, Map.empty[String, NonEmptyList[String]], Order[String])

    matches(res):
      case Left(MonopartiteMatcher.Error.UnsupportedPopulationSize(n)) =>
        expect.eql(oddPopulation.size, n)

  test("Matcher requires a preference list for every member"):
    val population =
      Set("a", "b", "c", "d")

    val res =
      MonopartiteMatcher
        .createMatches(population, Map("a" -> NonEmptyList.one("b")), Order[String])

    matches(res):
      case Left(MonopartiteMatcher.Error.MissingPreferenceList(xs)) =>
        // error list is non-deterministic from input set
        expect.eql(Set("b", "c", "d"), xs.iterator.toSet)
