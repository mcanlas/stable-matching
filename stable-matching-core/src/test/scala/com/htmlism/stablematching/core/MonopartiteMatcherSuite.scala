package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import weaver.*

object MonopartiteMatcherSuite extends FunSuite:
  test("Matcher requires an even population".ignore):
    val expected =
      Nil

    val res =
      MonopartiteMatcher.createMatches(Set.empty[String], Map.empty[String, NonEmptyList[String]], Order[String])

    whenSuccess(res): found =>
      expect.eql(expected, found)
