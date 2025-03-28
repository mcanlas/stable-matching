package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.*
import cats.data.*
import weaver.*

object BipartiteMatcherSuite extends FunSuite:
  test("Matcher requires proposers and acceptors of the same size"):
    val proposers =
      ListSet("a")

    val acceptors =
      ListSet(1, 2)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map.empty,
          Map.empty
        )
        .run

    matches(res):
      case Left(BipartiteMatcher.Error.MismatchedPopulationSizes(x, y)) =>
        expect.eql(proposers.size, x) and
          expect.eql(acceptors.size, y)

  test("Matcher requires a preference list for every proposer"):
    val proposers =
      ListSet("a")

    val acceptors =
      ListSet(1)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map.empty,
          Map.empty
        )
        .run

    matches(res):
      case Left(BipartiteMatcher.Error.MissingProposerPreferenceList(xs)) =>
        // error list is non-deterministic from input set
        expect.eql(ListSet("a"), xs.iterator.toSet)

  test("Matcher requires a preference list for every acceptor"):
    val proposers =
      ListSet("a")

    val acceptors =
      ListSet(1)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map("a" -> NonEmptyList.of(1)),
          Map.empty
        )
        .run

    matches(res):
      case Left(BipartiteMatcher.Error.MissingAcceptorPreferenceList(xs)) =>
        // error list is non-deterministic from input set
        expect.eql(ListSet("1"), xs.iterator.toSet)

  test("Matcher requires every proposer to be in every acceptor list"):
    val proposers =
      ListSet("a")

    val acceptors =
      ListSet(1)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map("a" -> NonEmptyList.of(1)),
          Map(1   -> NonEmptyList.of("invalid"))
        )
        .run

    matches(res):
      case Left(BipartiteMatcher.Error.IncompleteAcceptorsPreferenceList(p, k)) =>
        expect.eql("a", p) and
          expect.eql("1", k)

  test("Matcher requires every acceptor to be in every proposer list"):
    val proposers =
      ListSet("a")

    val acceptors =
      ListSet(1)

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map("a" -> NonEmptyList.of(12345)),
          Map(1   -> NonEmptyList.of("a"))
        )
        .run

    matches(res):
      case Left(BipartiteMatcher.Error.IncompleteProposersPreferenceList(p, k)) =>
        expect.eql("1", p) and
          expect.eql("a", k)

  test("Empty input yields empty output"):
    val proposers =
      ListSet.empty[String]

    val acceptors =
      ListSet.empty[Int]

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map.empty,
          Map.empty
        )
        .run

    whenSuccess(res): (_, matching) =>
      expect(matching.ab.isEmpty)

  test("A trivial population of one pair has a trivial answer".ignore):
    val proposers =
      ListSet.empty[String]

    val acceptors =
      ListSet.empty[Int]

    val res =
      BipartiteMatcher
        .createMatches(
          proposers,
          acceptors,
          Map.empty,
          Map.empty
        )
        .run

    whenSuccess(res): (_, matching) =>
      expect(matching.ab.isEmpty)
