package com.htmlism.stablematching.data

import cats.Eq

/**
 * Used when the population is split in half, type A for proposers, and type B for acceptors.
 *
 * @tparam A Proposer type
 * @tparam B Acceptor type
 */
case class OrderedBiMap[A : Eq, B : Eq]()
