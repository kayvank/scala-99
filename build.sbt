

ThisBuild / organization := "q2"
ThisBuild / version      := "1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.11"
ThisBuild / scalafmtOnCompile := true

libraryDependencies ++= {
  object V {
    val specs2 = "4.10.1"
  }
    Seq(
      "org.specs2" %% "specs2-core" % V.specs2 % "test"
    )
  }


scalacOptions ++= Seq(
  "-Ywarn-unused:imports",
  "-Xfatal-warnings",
  "-deprecation",
  "-feature",
  "-Xlint:infer-any",
  "-Xlint:adapted-args",
  "-Ywarn-adapted-args",
  "-Xlint:delayedinit-select",
  "-unchecked"
)

