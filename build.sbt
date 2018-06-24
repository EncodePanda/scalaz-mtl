name := "scalaz-mtl"

scalaVersion := "2.12.6"

lazy val root = Project("root", file("."))
    .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.24")

