package com.htmlism.stablematching.core

import cats.*

object StableMatcher:
  object Monopartite:
    def createMatches[A: Eq](population: Set[A], preferences: Map[A, List[A]], order: Order[A]): List[String] =
      Nil

  object Bipartite:
    def createMatches[A: Eq, B: Eq](
        proposerPopulation: Set[A],
        acceptorPopulation: Set[B],
        proposerPreferences: Map[A, List[B]],
        acceptorPreferences: Map[B, List[A]],
        proposerOrder: Order[A],
        acceptorOrder: Order[B]
    ): List[String] =
      Nil
