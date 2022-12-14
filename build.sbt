val zioVersion = "2.0.4"

scalaVersion := "3.2.1"

libraryDependencies ++= Seq(
  "dev.zio"                %% "zio"                        % zioVersion,
  "dev.zio"                %% "zio-test"                   % zioVersion % Test,
  "dev.zio"                %% "zio-test-sbt"               % zioVersion % Test,
  "dev.zio"                %% "zio-streams"                % zioVersion % Test,
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4"
)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.14"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.14" % "test"

libraryDependencies += "com.typesafe.play" %% "play-json" % "2.10.0-RC7"

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

scalacOptions ++= Seq(
  "-no-indent"
)
