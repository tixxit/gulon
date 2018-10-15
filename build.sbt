scalaVersion := "2.12.7"

name := "gulon"

val catsEffectVersion = "1.0.0"
val declineVersion = "0.5.0"
val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.14.0"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
  )
)

lazy val root = (project in file("."))
  .aggregate(core, bench)

lazy val core = project
  .settings(
    commonSettings,
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
