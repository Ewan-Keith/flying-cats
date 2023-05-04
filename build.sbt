Global / onChangedBuildSource := ReloadOnSourceChanges
ThisBuild / organization := "com.github"
ThisBuild / scalaVersion := "2.13.8"

val circeVersion = "0.14.1"
val fs2Version = "3.6.1"

lazy val commonDependencies = Seq(
  "org.typelevel" %% "cats-effect" % "3.4.10",
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,
  "co.fs2" %% "fs2-core" % fs2Version,
  "co.fs2" %% "fs2-io" % fs2Version
)

lazy val commonSettings = Seq(
  libraryDependencies ++= commonDependencies
)

lazy val nativeImageSettings = Seq(
  nativeImageOptions += "--no-fallback",
  nativeImageVersion := "22.1.0" // It should be at least version 21.0.0
)

lazy val common = (project in file("modules/flying-cats-common"))
  .settings(
    name := "flying-cats-common",
    commonSettings
  )

lazy val echo = (project in file("modules/flying-cats-echo"))
  .enablePlugins(NativeImagePlugin)
  .dependsOn(common)
  .settings(
    Compile / mainClass := Some("com.github.flyingcats.echo.Main"),
    name := "flying-cats-echo",
    commonSettings,
    nativeImageSettings
  )

lazy val uid = (project in file("modules/flying-cats-uid"))
  .enablePlugins(NativeImagePlugin)
  .dependsOn(common)
  .settings(
    Compile / mainClass := Some("com.github.flyingcats.uid.Main"),
    name := "flying-cats-uid",
    commonSettings,
    nativeImageSettings
  )

lazy val broadcast = (project in file("modules/flying-cats-broadcast"))
  .enablePlugins(NativeImagePlugin)
  .dependsOn(common)
  .settings(
    Compile / mainClass := Some("com.github.flyingcats.broadcast.Main"),
    name := "flying-cats-broadcast",
    commonSettings,
    nativeImageSettings
  )
