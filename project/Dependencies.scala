import sbt._
import sbt.Keys.testOptions
import scala.language.postfixOps

object Dependencies {
    val projectName = "systra"
    val systraOrganization = "com.github.imomushi8"
    val systraScala = "3.1.0"
    
    val catsVersion = "2.7.0"
    val fs2Version = "3.2.4"

    val systraDependencies = Seq(
        "org.typelevel" %% "cats-core"            % catsVersion,
        "org.typelevel" %% "cats-free"            % catsVersion,
        "org.typelevel" %% "cats-effect"          % "3.3.4",
        "org.typelevel" %% "cats-mtl"             % "1.2.1",
        "org.scalactic" %% "scalactic"            % "3.2.10",
        "co.fs2"        %% "fs2-core"             % fs2Version,
        "co.fs2"        %% "fs2-io"               % fs2Version,
        "co.fs2"        %% "fs2-reactive-streams" % fs2Version,
        "co.fs2"        %% "fs2-scodec"           % fs2Version,

        "org.scalanlp" %% "breeze" % "2.0.1-RC2",
        "org.openmole" %% "mgo" % "3.55",

        "com.github.gekomad" %% "itto-csv"        % "2.0.0",
        "org.atnos"          %% "eff"             % "5.22.0",

        "ch.qos.logback"              %  "logback-classic"  % "1.2.6",
        "com.typesafe.scala-logging"  %% "scala-logging"    % "3.9.4",

        "org.scalatest"     %% "scalatest"                      % "3.2.10"    % Test,
        "org.scalatestplus" %% "scalacheck-1-15"                % "3.2.10.0"  % Test,
        "org.scalacheck"    %% "scalacheck"                     % "1.15.4"    % Test,
        "org.typelevel"     %% "cats-laws"                      % catsVersion % Test,
        "org.typelevel"     %% "discipline-munit"               % "1.0.9"     % Test,
        "org.scalameta"     %% "munit"                          % "0.7.29"    % Test
    )
    
    val systraResolver = Seq(
        Classpaths.sbtPluginReleases
    )
    
    val systraOptions = Seq(
        "-deprecation",
        "-encoding", "UTF-8",
        "-feature",
        "-language:_",
        "-Ykind-projector:underscores"
    )
    
    val systraTestKey = Seq(
        Tests.Argument(TestFrameworks.ScalaTest, "-fWDT", s"target/test-report_$projectName.txt", "-eNDXEHLO")
    )
}