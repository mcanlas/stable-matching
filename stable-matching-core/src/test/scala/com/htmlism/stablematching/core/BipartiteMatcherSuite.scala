package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import weaver.*

object BipartiteMatcherSuite extends FunSuite:
  test("Matcher requires proposers and acceptors of the same size".ignore):
    val expected =
      Nil

    val res =
      BipartiteMatcher
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
