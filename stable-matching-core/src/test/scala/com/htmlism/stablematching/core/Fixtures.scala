package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.data.NonEmptyList

object Fixtures:
  // https://www.youtube.com/watch?v=5QLxAp8mRKo
  def buildPopSixEmptyTable: Either[MonopartiteStatefulTable.ValidationError, MonopartiteStatefulTable[String]] =
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

    MonopartiteStatefulTable
      .build(
        population,
        preferences
      )
