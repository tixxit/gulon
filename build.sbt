scalaVersion := "2.12.7"

name := "gulon"

val catsEffectVersion = "1.0.0"
val declineVersion = "0.5.0"

lazy val root = (project in file("."))
  .aggregate(core, bench)

lazy val core = project
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "com.monovore" %% "decline" % declineVersion
    ),
    PB.targets in Compile := Seq(
      scalapb.gen() -> (sourceManaged in Compile).value
    ),
    PB.protoSources in Compile := Seq(file("core/src/main/protobuf")),
    libraryDependencies += "com.thesamet.scalapb" %% "scalapb-runtime" % scalapb.compiler.Version.scalapbVersion % "protobuf"
  )


lazy val bench = project
  .dependsOn(core)
  .enablePlugins(JmhPlugin)
