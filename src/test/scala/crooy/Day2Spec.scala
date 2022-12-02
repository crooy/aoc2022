package crooy

import zio.ZLayer
import zio.test.*
import zio.test.Assertion.*
import zio.test.Assertion.Render.*

object Day2Spec extends DefaultRunnableSpec {

  val testInput = FileListInput("test", List("A Y","B X","C Z"))
  val output = 15

  val parseInput = test("parseInput") {
    for
      strategy <- Day2.parseInput
    yield assert(strategy.length)(equalTo(3))
  }

  val calculateScore = test("calculateScore") {
    for
      strategy <- Day2.parseInput
      score <- Day2.calculateScore(strategy)
    yield assert(score)(equalTo(output))
  }

  def spec = suite("Day2")(calculateScore, parseInput).provideShared( ZLayer.succeed(testInput) )
}
