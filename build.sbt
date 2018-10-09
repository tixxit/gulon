scalaVersion := "2.12.7"

name := "gulon"

lazy val root = (project in file("."))
  .aggregate(core, bench)

lazy val core = project
  .settings(
    libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0"
  )


lazy val bench = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
