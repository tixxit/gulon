organization in ThisBuild := "net.tixxit"
name in ThisBuild := "gulon"

scalaVersion in ThisBuild := "2.12.7"
crossScalaVersions in ThisBuild := Seq("2.11.12", "2.12.7")

scalacOptions in ThisBuild ++= Seq("-deprecation", "-feature", "-unchecked", "-language:higherKinds")

val catsEffectVersion = "1.4.0"
val declineVersion = "0.5.0"
val scalaTestVersion = "3.0.5"
val scalaCheckVersion = "1.14.0"
val fastparseVersion = "2.1.3"
val jline3Version = "3.9.0"

val commonSettings = Seq(
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % scalaTestVersion % "test",
    "org.scalacheck" %% "scalacheck" % scalaCheckVersion % "test"
  ),
  parallelExecution in Test := false
)

lazy val root = (project in file("."))
  .aggregate(core, bench)

lazy val core = project
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect" % catsEffectVersion,
      "com.monovore" %% "decline" % declineVersion,
      "com.lihaoyi" %% "fastparse" % fastparseVersion,
      "org.jline" % "jline" % jline3Version
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
