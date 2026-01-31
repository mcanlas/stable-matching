package com.htmlism.stablematching.core

import scala.collection.immutable.ListMap

import cats.data.NonEmptyList

object BiFixtures:
  // https://www.youtube.com/watch?v=0m_YW1zVs-Q
  def buildFiveAndFive: BipartiteStatefulTable[String, String] =
    val proposerPreferences =
      ListMap(
        "A" -> NonEmptyList.of("O", "M", "N", "L", "P"),
        "B" -> NonEmptyList.of("P", "N", "M", "L", "O"),
        "C" -> NonEmptyList.of("M", "P", "L", "O", "N"),
        "D" -> NonEmptyList.of("P", "M", "O", "N", "L"),
        "E" -> NonEmptyList.of("O", "L", "M", "N", "P")
      )

    val acceptorPreferences =
      ListMap(
        "L" -> NonEmptyList.of("D", "B", "E", "C", "A"),
        "M" -> NonEmptyList.of("B", "A", "D", "C", "E"),
        "N" -> NonEmptyList.of("A", "C", "E", "D", "B"),
        "O" -> NonEmptyList.of("D", "A", "C", "B", "E"),
        "P" -> NonEmptyList.of("B", "E", "A", "C", "D")
      )

    val proposerStates =
      for
        p <- proposerPreferences.keys.toList
        a <- proposerPreferences(p).toList
      yield (p, a) -> BipartiteStatefulTable.State.Free

    val acceptorStates =
      for
        a <- acceptorPreferences.keys.toList
        p <- acceptorPreferences(a).toList
      yield (a, p) -> BipartiteStatefulTable.State.Free

    BipartiteStatefulTable(
      proposerPreferences,
      acceptorPreferences,
      ListMap.from(proposerStates),
      ListMap.from(acceptorStates)
    )

  def buildFiveAndFiveReverse: BipartiteStatefulTable[String, String] =
    val tbl =
      buildFiveAndFive

    BipartiteStatefulTable(
      tbl.acceptorPreferences,
      tbl.proposerPreferences,
      tbl.acceptorStates,
      tbl.proposerStates
    )
