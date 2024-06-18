package com.htmlism.stablematching.core

import cats.*
import weaver.*

object StableMatcherSuite extends FunSuite:
  test("Monopartite API design"):
    val expected =
      Nil

    val found =
      StableMatcher.Monopartite.createMatches(Set.empty[String], Map.empty[String, List[String]], Order[String])

    expect.eql(expected, found)

  test("Bipartite API design"):
    val expected =
      Nil

    val found =
      StableMatcher
        .Bipartite
        .createMatches(
          Set.empty[String],
          Set.empty[String],
          Map.empty[String, List[String]],
          Map.empty[String, List[String]],
          Order[String],
          Order[String]
        )

    expect.eql(expected, found)
