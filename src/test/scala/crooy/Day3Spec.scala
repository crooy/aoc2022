package crooy

import zio.ZLayer
import zio.test.*
import zio.test.Assertion.*

object Day3Spec extends ZIOSpecDefault {

  val testInput = FileListInput("test", List(
      "vJrwpWtwJgWrhcsFMMfFFhFp",
  "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
  "PmmdzqPrVvPwwTWBwg",
  "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
  "ttgJtRGJQctTZtZT",
  "CrZsJsPPZsGzwwsLwLmpwMDw"
  ))

  val test1 = test("test1") {
    for
      result <- Day3.part1
    yield assert(result)(equalTo(157))
  }

  val test2 = test("test2") {
    for
      result <- Day3.part2
    yield assert(result)(equalTo(70))
  }

  def spec = suite("Day3")(test1, test2).provideShared( ZLayer.succeed(testInput) )
}
