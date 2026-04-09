val scala3Version = "3.8.3"

lazy val root = project
  .in(file("."))
  .settings( name                 := "uuid"
           , version              := "0.1.0"
           , scalaVersion         := scala3Version
           , libraryDependencies ++=
               Seq("org.scalacheck" %% "scalacheck" % "1.19.0" % "test")
           )

ThisBuild / scalacOptions ++= Seq(
  "-encoding", "utf8",
  "-feature",
  "-language:implicitConversions",
  "-language:existentials",
  "-language:strictEquality",
  "-unchecked",
  "-Werror",
  "-deprecation"
)
