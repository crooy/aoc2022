val zioVersion = "2.0.0-M6-2"

scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"          % zioVersion,
  "dev.zio" %% "zio-test"     % zioVersion % Test,
  "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
