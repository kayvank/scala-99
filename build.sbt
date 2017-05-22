scalaVersion := "2.11.7"

val scalazVersion = "7+"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.specs2" %% "specs2-core" % "3.8.9" % "test",
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")

scalacOptions ++= Seq(
  "-feature"
)

initialCommands in console := "import scalaz._, Scalaz._"
