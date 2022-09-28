val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings( name                 := "uuid"
           , version              := "0.1.0"
           , scalaVersion         := scala3Version
           , libraryDependencies ++=
               Seq( "org.typelevel"  %% "cats-effect" % "3.3.12"
                  , "org.scalacheck" %% "scalacheck"  % "1.16.0" % "test"
                  )
           )
