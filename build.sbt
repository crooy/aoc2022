val zioVersion = "2.0.4"

scalaVersion := "3.2.1"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,
  "dev.zio" %% "zio-test"     % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
  "dev.zio" %% "zio-streams"  % zioVersion % Test
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
