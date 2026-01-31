package com.htmlism.stablematching.core

import scala.collection.immutable.ListSet

import cats.data.NonEmptyList

object MonoFixtures:
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

  def buildUnstablePopFourEmptyTable
      : Either[MonopartiteStatefulTable.ValidationError, MonopartiteStatefulTable[String]] =
    val population =
      ListSet("a", "b", "c", "d")

    val preferences =
      Map(
        "a" -> NonEmptyList.of("b", "c", "d"),
        "b" -> NonEmptyList.of("c", "a", "d"),
        "c" -> NonEmptyList.of("a", "b", "d"),
        "d" -> NonEmptyList.of("a", "b", "c"),
      )

    MonopartiteStatefulTable
      .build(
        population,
        preferences
      )
