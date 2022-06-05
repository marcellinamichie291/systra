import Dependencies._

lazy val root = project
  .in(file("."))
  .settings(
    name                := projectName,
    version             := "0.1.0-SNAPSHOT",
    scalaVersion        :=  systraScala,
    libraryDependencies ++= systraDependencies,
    resolvers           ++= systraResolver,
    scalacOptions       ++= systraOptions,
    Test / testOptions  ++= systraTestKey
  )