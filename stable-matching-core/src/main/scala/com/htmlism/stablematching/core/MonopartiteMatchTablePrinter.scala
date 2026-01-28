package com.htmlism.stablematching.core

import scala.util.chaining.*

import cats.Show
import cats.syntax.all.*

object MonopartiteMatchTablePrinter:
  def generateMarkdown[A: Show](table: MonopartiteMatchTable[A]): String =
    val headerRow =
      List("Roommate", "Matches")
        .mkString(" | ")
        .pipe(surround("| ", " |"))

    val lineRow =
      List
        .fill(2)("---")
        .mkString(" | ")
        .pipe(surround("| ", " |"))

    val bodyRows =
      table
        .members
        .iterator
        .map: m =>
          val matchesStr =
            table
              .matches(m)
              .map: nel =>
                nel
                  .map(_.show)
                  .mkString_(", ")
              .getOrElse("")

          List(m.show, matchesStr)
            .mkString(" | ")
            .pipe(surround("| ", " |"))

    (List(headerRow, lineRow) ++ bodyRows.toList)
      .mkString("\n")

  private def surround(left: String, right: String)(s: String): String =
    s"$left$s$right"
