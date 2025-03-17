package com.htmlism.stablematching.data

import cats.Eq

/**
 * Used when the population matches to itself, as in the stable roommates problem
 *
 * @tparam A The roommate type
 */
case class OrderedStableMatching[A : Eq]()
