package com.htmlism.stablematching.core

trait MarkdownTable[A]:
  def width: Int

  def headers: List[String]

  def rows(x: A): List[List[String]]
