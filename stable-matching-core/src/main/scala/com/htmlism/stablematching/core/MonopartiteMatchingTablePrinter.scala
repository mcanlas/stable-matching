package com.htmlism.stablematching.core

import scala.util.chaining.*

import cats.Show
import cats.syntax.all.*

object MonopartiteMatchingTablePrinter:
  def generateMarkdown[A: Show](table: MonopartiteMatchingTable[A]): String =
    val length =
      table.members.size

    val headerRow =
      (1 until length)
        .map(n => s"#$n")
        .toList
        .prepended("Roommate")
        .mkString(" | ")
        .pipe(surround("| ", " |"))

    val lineRow =
      List
        .fill(length)("---")
        .mkString(" | ")
        .pipe(surround("| ", " |"))

    val bodyRows =
      table
        .members
        .iterator
        .map: m =>
          val preferredRoommatesShowCells =
            table
              .preferences(m)
              .iterator
              .map: pr =>
                val stateStr =
                  stateToString(table.cells((m, pr)))

                s"$pr $stateStr"

          preferredRoommatesShowCells
            .toList
            .prepended(m.show)
            .mkString(" | ")
            .pipe(surround("| ", " |"))

    (List(headerRow, lineRow) ++ bodyRows.toList)
      .mkString("\n")

  private def surround(left: String, right: String)(s: String): String =
    s"$left$s$right"

  private def stateToString[A: Show](state: MonopartiteMatchingTable.State): String =
    state match
      case MonopartiteMatchingTable.State.Free =>
        ""
      case MonopartiteMatchingTable.State.ProposesTo =>
        "😸"
      case MonopartiteMatchingTable.State.ProposedBy =>
        "🤔"
      case MonopartiteMatchingTable.State.Rejects =>
        "❌"
      case MonopartiteMatchingTable.State.RejectedBy =>
        "👻"
