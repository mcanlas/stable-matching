package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.data.NonEmptyList
import weaver.*

object MonopartiteMatchingTablePrinterSuite extends FunSuite:
  private def buildFixture =
    // https://www.youtube.com/watch?v=5QLxAp8mRKo

    val population =
      ListSet("a", "b", "c", "d", "e", "f")

    val preferences =
      Map(
        "a" -> NonEmptyList.of("b", "d", "f", "c", "e"),
        "b" -> NonEmptyList.of("d", "e", "f", "a", "c"),
        "c" -> NonEmptyList.of("d", "e", "f", "a", "b"),
        "d" -> NonEmptyList.of("f", "c", "a", "e", "b"),
        "e" -> NonEmptyList.of("f", "c", "d", "b", "a"),
        "f" -> NonEmptyList.of("a", "b", "d", "c", "e")
      )

    MonopartiteMatchingTable
      .build(
        population,
        preferences
      )

  test("prints"):
    matches(buildFixture):
      case Right(table) =>
        println:
          MonopartiteMatchingTablePrinter
            .generateMarkdown(table)

        expect(true)
