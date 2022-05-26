import sbt._
import sbt.Keys.testOptions
import scala.language.postfixOps

object Dependencies {
  val projectName = "systra"
  val systraOrganization = "com.github.imomushi8"
  val systraScala = "3.1.0"
    
  val catsVersion = "2.6.1"
  val fs2Version = "3.2.2"
  val circeVersion = "0.14.1"
  val sttpVersion = "3.5.2"
  val http4sVersion = "1.0.0-M32"

  val systraDependencies = Seq(
    "org.typelevel" %% "cats-core"            % catsVersion,
    "org.typelevel" %% "cats-effect"          % "3.2.9",
    "co.fs2"        %% "fs2-core"             % fs2Version,
    "co.fs2"        %% "fs2-io"               % fs2Version,
    "co.fs2"        %% "fs2-reactive-streams" % fs2Version,
    "co.fs2"        %% "fs2-scodec"           % fs2Version,

    "org.http4s" %% "http4s-dsl"          % http4sVersion,
    "org.http4s" %% "http4s-ember-server" % http4sVersion,
    "org.http4s" %% "http4s-ember-client" % http4sVersion,

    "org.typelevel" %% "shapeless3-deriving" % "3.0.1",

    "io.circe" %% "circe-core"    % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser"  % circeVersion,

    "is.cir" %% "ciris" % "2.3.2",
    
    "com.github.gekomad"          %% "itto-csv"         % "2.0.0",

    "com.softwaremill.sttp.client3" %% "core"                          % sttpVersion,
    "com.softwaremill.sttp.client3" %% "fs2"                           % sttpVersion,
    "com.softwaremill.sttp.client3" %% "async-http-client-backend-fs2" % sttpVersion,
    "com.softwaremill.sttp.client3" %% "circe"                         % sttpVersion,

    "ch.qos.logback"              %  "logback-classic"  % "1.2.6",
    "com.typesafe.scala-logging"  %% "scala-logging"    % "3.9.4",
    
    // hmacsha256用
    "commons-codec" % "commons-codec" % "1.15",

    // openmoleなし、breezeありでimport後、breezeなし、openmoleありで再度importする。
    // openmoleの中にbreezeが入ってるので、おそらくそれを使っている
    //"org.scalanlp" %% "breeze" % "2.0.1-RC2", // これを削除すればbreezeダウンロードが上手くいった
    "org.openmole" %% "mgo" % "3.55",

    "org.scalatest"     %% "scalatest"        % "3.2.10"    % Test,
    "org.scalatestplus" %% "scalacheck-1-15"  % "3.2.10.0"  % Test,
    "org.scalacheck"    %% "scalacheck"       % "1.15.4"    % Test,
    "org.typelevel"     %% "discipline-munit" % "1.0.9"     % Test,
    "org.scalameta"     %% "munit"            % "0.7.29"    % Test
  )
    
  val systraResolver = Seq(
    Classpaths.sbtPluginReleases
  )
    
  val systraOptions = Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:_",
    "-Ykind-projector:underscores",
    "-Ywarn-unused-import"
  )
    
  val systraTestKey = Seq(
    Tests.Argument(TestFrameworks.ScalaTest, "-fWDT", s"target/test-report_$projectName.txt", "-eNDXEHLO")
  )
}