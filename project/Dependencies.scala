import sbt._
import sbt.Keys.testOptions
import scala.language.postfixOps

object Dependencies {
    val projectName = "systra"
    val systraOrganization = "com.github.imomushi8"
    val systraScala = "3.1.0"
    
    val catsVersion = "2.6.1"
    val fs2Version = "3.2.2"
    val fs2_213_Version = "3.2.2"
    val systraDependencies = Seq(
        "org.typelevel" %% "cats-core"            % catsVersion,
        "org.typelevel" %% "cats-effect"          % "3.2.9",
        "co.fs2"        %% "fs2-core"             % fs2Version,
        "co.fs2"        %% "fs2-io"               % fs2Version,
        "co.fs2"        %% "fs2-reactive-streams" % fs2Version,
        "co.fs2"        %% "fs2-scodec"           % fs2Version,

        "io.circe" %% "circe-core" % "0.14.1",
        "io.circe" %% "circe-generic" % "0.14.1",
        "io.circe" %% "circe-parser" % "0.14.1",

        "is.cir" %% "ciris" % "2.3.2",
        
        "com.github.gekomad"          %% "itto-csv"         % "2.0.0",
        "ch.qos.logback"              %  "logback-classic"  % "1.2.6",
        "com.typesafe.scala-logging"  %% "scala-logging"    % "3.9.4",

        "com.softwaremill.sttp.client3" %% "core" % "3.5.2",
        "com.softwaremill.sttp.client3" %% "fs2" % "3.5.2",
        "com.softwaremill.sttp.client3" %% "async-http-client-backend-fs2" % "3.5.2",
        "com.softwaremill.sttp.client3" %% "circe" % "3.5.2",

        // hmacsha256用
        "commons-codec" % "commons-codec" % "1.15",

        // breezeをコメントアウトしてimport buildする
        //"org.scalanlp" %% "breeze" % "2.0.1-RC2", // これを削除すればbreezeダウンロードが上手くいった
        "org.openmole" %% "mgo" % "3.55",

        // 奇跡的な采配でこの状態がビルド可能な状態になっている
        //"org.apache.spark" % "spark-core_2.13" % "3.2.0",
        //"org.apache.spark" % "spark-mllib_2.13" % "3.2.0" % "provided",
        //"org.apache.spark" % "spark-sql_2.13" % "3.2.0" % "provided",

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
        "-Ykind-projector:underscores"
    )
    
    val systraTestKey = Seq(
        Tests.Argument(TestFrameworks.ScalaTest, "-fWDT", s"target/test-report_$projectName.txt", "-eNDXEHLO")
    )
}