package com.htmlism.stablematching.core

trait Tabular[A]:
  def width(x: A): Int =
    headers(x).size

  def headers(x: A): List[String]

  def rows(x: A): List[List[String]]
