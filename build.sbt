lazy val root =
  Project("stable-matching", file("."))
    .aggregate(core)

lazy val core =
  module("core")
    .withCats
    .withTesting
