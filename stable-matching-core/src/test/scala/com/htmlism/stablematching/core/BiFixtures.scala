package com.htmlism.stablematching.core

import cats.data.NonEmptyList

object BiFixtures:
  // https://www.youtube.com/watch?v=0m_YW1zVs-Q
  def buildFiveAndFive: BipartiteStatefulTable[String, String] =
    val proposerPreferences =
      List(
        "A" -> NonEmptyList.of("O", "M", "N", "L", "P"),
        "B" -> NonEmptyList.of("P", "N", "M", "L", "O"),
        "C" -> NonEmptyList.of("M", "P", "L", "O", "N"),
        "D" -> NonEmptyList.of("P", "M", "O", "N", "L"),
        "E" -> NonEmptyList.of("O", "L", "M", "N", "P")
      )

    val acceptorPreferences =
      List(
        "L" -> NonEmptyList.of("D", "B", "E", "C", "A"),
        "M" -> NonEmptyList.of("B", "A", "D", "C", "E"),
        "N" -> NonEmptyList.of("A", "C", "E", "D", "B"),
        "O" -> NonEmptyList.of("D", "A", "C", "B", "E"),
        "P" -> NonEmptyList.of("B", "E", "A", "C", "D")
      )

    BipartiteStatefulTable.build(
      proposerPreferences,
      acceptorPreferences
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
