Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / organization := "com.github"
ThisBuild / scalaVersion := "2.13.8"

val circeVersion = "0.14.1"

lazy val commonDependencies = Seq(
  "org.typelevel" %% "cats-effect" % "3.4.10",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion
)

lazy val commonSettings = Seq(
  libraryDependencies ++= commonDependencies,
  nativeImageOptions += "--no-fallback",
  nativeImageVersion := "22.1.0" // It should be at least version 21.0.0
)

lazy val echo = (project in file("modules/flying-cats-echo"))
  .enablePlugins(NativeImagePlugin)
  .settings(
    Compile / mainClass := Some("com.github.flyingcats.echo.Main"),
    name := "flying-cats-echo",
    commonSettings
  )
