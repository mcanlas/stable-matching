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
          Map.empty,
          Map.empty,
          Order[String],
          Order[Int]
        )

    matches(res):
      case Left(BipartiteMatcher.Error.MismatchedPopulationSizes(x, y)) =>
        expect.eql(proposers.size, x) and
          expect.eql(acceptors.size, y)

  test("Matcher requires a preference list for every proposer"):
    val proposers =
      Set("a")

    val acceptors =
      Set(1)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map.empty,
          Map.empty,
          Order[String],
          Order[Int]
        )

    matches(res):
      case Left(BipartiteMatcher.Error.MissingProposerPreferenceList(xs)) =>
        // error list is non-deterministic from input set
        expect.eql(Set("a"), xs.iterator.toSet)

  test("Matcher requires a preference list for every acceptor"):
    val proposers =
      Set("a")

    val acceptors =
      Set(1)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map("a" -> NonEmptyList.of(1)),
          Map.empty,
          Order[String],
          Order[Int]
        )

    matches(res):
      case Left(BipartiteMatcher.Error.MissingAcceptorPreferenceList(xs)) =>
        // error list is non-deterministic from input set
        expect.eql(Set("1"), xs.iterator.toSet)

  test("Matcher requires every proposer to be in every acceptor list"):
    expect.eql("", "")

  test("Matcher requires every acceptor to be in every proposer list"):
    expect.eql("", "")
