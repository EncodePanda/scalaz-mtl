name := "scalaz-mtl"

scalaVersion := "2.12.6"

resolvers += Resolver.sonatypeRepo("releases")
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")
scalacOptions ++= Seq("-language:higherKinds")

lazy val root = Project("root", file("."))
    .settings(libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.24")

