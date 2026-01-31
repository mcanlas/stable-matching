package com.htmlism.stablematching.core

import scala.util.chaining.*

import cats.Show
import cats.syntax.all.*

object MonopartiteStatefulTablePrinter:
  def generateMarkdown[A: Show](table: MonopartiteStatefulTable[A]): String =
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

  private def stateToString(state: MonopartiteStatefulTable.State): String =
    state match
      case MonopartiteStatefulTable.State.Free =>
        "➖"
      case MonopartiteStatefulTable.State.ProposesTo =>
        "😸"
      case MonopartiteStatefulTable.State.ProposedBy =>
        "🤔"
      case MonopartiteStatefulTable.State.Rejects =>
        "❌"
      case MonopartiteStatefulTable.State.RejectedBy =>
        "👻"
