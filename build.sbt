scalaVersion := "2.12.7"

name := "gulon"

lazy val root = (project in file("."))
  .aggregate(core, bench)

lazy val core = project

lazy val bench = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
