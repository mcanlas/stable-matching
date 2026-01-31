package com.htmlism.stablematching.core

/**
  * Helper type to cut down on nested [[Either]]s
  */
private type Res[A] =
  Either[String, A]
