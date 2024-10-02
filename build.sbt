ThisBuild / scalaVersion := "3.5.1"
ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / semanticdbEnabled := true

ThisBuild / scalacOptions ++= Seq(
  "-no-indent",
  "-Wunused:imports"
)

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / libraryDependencies ++= Seq(
  "org.scalameta" %% "munit" % "1.0.0" % Test
)

lazy val parser = project
  .in(file("modules/parser"))
  .withId("parser")
  .settings(
    libraryDependencies ++= Seq(
      "org.antlr" % "antlr4-runtime" % "4.13.2"
    )
  )

lazy val compiler = project
  .in(file("modules/compiler"))
  .withId("compiler")
  .dependsOn(parser)

lazy val vm = project
  .in(file("modules/vm"))
  .withId("vm")
  .dependsOn(compiler)

lazy val root = project
  .in(file("."))
  .withId("simplelang")
  .aggregate(parser, compiler, vm)
