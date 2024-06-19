package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import weaver.*

object MonopartiteMatcherSuite extends FunSuite:
  test("Monopartite API design"):
    val expected =
      Nil

    val res =
      StableMatcher.Monopartite.createMatches(Set.empty[String], Map.empty[String, NonEmptyList[String]], Order[String])

    whenSuccess(res): found =>
      expect.eql(expected, found)
