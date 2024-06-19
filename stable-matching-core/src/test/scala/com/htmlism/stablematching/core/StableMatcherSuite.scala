package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import weaver.*

object StableMatcherSuite extends FunSuite:
  test("Monopartite API design"):
    val expected =
      Nil

    val res =
      StableMatcher.Monopartite.createMatches(Set.empty[String], Map.empty[String, NonEmptyList[String]], Order[String])

    whenSuccess(res): found =>
      expect.eql(expected, found)

  test("Bipartite API design"):
    val expected =
      Nil

    val res =
      StableMatcher
        .Bipartite
        .createMatches(
          Set.empty[String],
          Set.empty[String],
          Map.empty[String, NonEmptyList[String]],
          Map.empty[String, NonEmptyList[String]],
          Order[String],
          Order[String]
        )

    whenSuccess(res): found =>
      expect.eql(expected, found)
