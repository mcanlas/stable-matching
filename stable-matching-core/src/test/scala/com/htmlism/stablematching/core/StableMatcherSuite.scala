package com.htmlism.stablematching.core

import weaver.*

object StableMatcherSuite extends FunSuite:
  test("Monopartite API design"):
    val expected =
      Nil

    val found =
      StableMatcher.createMatches

    expect.eql(expected, found)

  test("Bipartite API design"):
    val expected =
      Nil

    val found =
      StableMatcher.createMatches

    expect.eql(expected, found)
