name := "CSVParser"

version := "1.0"

scalaVersion := "2.12.10"

resolvers += "Typesafe Repo" at "https://repo.typesafe.com/typesafe/releases/"

libraryDependencies ++=  Seq(
  "com.typesafe.play" %% "play-json" % "2.7.3",
  "org.scalactic" %% "scalactic" % "3.1.0",
  "org.scalatest" %% "scalatest" % "3.1.0" % "test",
)
