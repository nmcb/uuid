val scala3Version = "3.6.4"

lazy val root = project
  .in(file("."))
  .settings( name                 := "uuid"
           , version              := "0.1.0"
           , scalaVersion         := scala3Version
           , libraryDependencies ++=
               Seq("org.scalacheck" %% "scalacheck" % "1.18.1" % "test")
           )
