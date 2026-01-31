package com.htmlism.stablematching.core

import scala.util.chaining.*

object MarkdownTablePrinter:
  def generateMarkdown[A: MarkdownTable](table: A): String =
    val ev = summon[MarkdownTable[A]]

    val headerRow =
      ev
        .headers
        .mkString(" | ")
        .pipe(surround("| ", " |"))

    val lineRow =
      List
        .fill(ev.width)("---")
        .mkString(" | ")
        .pipe(surround("| ", " |"))

    val bodyRows =
      ev
        .rows(table)
        .map: xs =>
          xs
            .mkString(" | ")
            .pipe(surround("| ", " |"))

    (List(headerRow, lineRow) ++ bodyRows)
      .mkString("\n")

  private def surround(left: String, right: String)(s: String): String =
    s"$left$s$right"
