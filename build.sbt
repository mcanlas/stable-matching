lazy val root =
  Project("stable-matching", file("."))
    .aggregate(core, data)

lazy val core =
  module("core")
    .dependsOn(data)
    .withCats
    .withTesting

lazy val data =
  module("data")
    .withCats
    .withTesting
