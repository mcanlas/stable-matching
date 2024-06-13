lazy val root =
  Project("stable-matching", file("."))

lazy val core =
  module("core")
    .withCats
    .withTesting
