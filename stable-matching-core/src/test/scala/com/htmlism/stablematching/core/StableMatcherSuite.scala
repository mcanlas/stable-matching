package com.htmlism.stablematching.core

import weaver.*

object StableMatcherSuite extends FunSuite:
  test("API design"):
    val expected =
      Nil

    val found =
      StableMatcher.createMatches

    expect.eql(expected, found)
