scalaVersion := "2.12.7"

name := "gulon"

lazy val root = (project in file("."))
  .aggregate(core, bench)

lazy val core = project
  .settings(
    libraryDependencies += "org.typelevel" %% "cats-effect" % "1.0.0",
    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value
    ),
    PB.protoSources in Compile := Seq(file("core/src/main/protobuf")),
    libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
  )


lazy val bench = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
