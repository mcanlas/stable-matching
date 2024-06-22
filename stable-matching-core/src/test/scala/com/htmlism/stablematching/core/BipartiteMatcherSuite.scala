package com.htmlism.stablematching.core

import cats.*
import cats.data.*
import weaver.*

object BipartiteMatcherSuite extends FunSuite:
  test("Matcher requires proposers and acceptors of the same size"):
    val proposers =
      Set("a")

    val acceptors =
      Set(1, 2)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map.empty[String, NonEmptyList[Int]],
          Map.empty[Int, NonEmptyList[String]],
          Order[String],
          Order[Int]
        )

    matches(res):
      case Left(BipartiteMatcher.Error.MismatchedPopulationSizes(x, y)) =>
        expect.eql(proposers.size, x) and
          expect.eql(acceptors.size, y)
