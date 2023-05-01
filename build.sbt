ThisBuild / organization := "com.github"
ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file(".")).enablePlugins(NativeImagePlugin).settings(
  name                := "flying-cats",
  libraryDependencies += "org.typelevel" %% "cats-effect" % "3.4.10",
  Compile / mainClass := Some("com.github.flyingcats.Main"),
  nativeImageOptions  += "--no-fallback",
  nativeImageVersion  := "22.1.0" // It should be at least version 21.0.0
)

