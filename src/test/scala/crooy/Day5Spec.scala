package crooy

import zio.ZLayer
import zio.test.*
import zio.test.Assertion.*

object Day5Spec extends ZIOSpecDefault {
  val testInput = FileListInput(
    "test",
    """    [D]    
      |[N] [C]    
      |[Z] [M] [P]
      | 1   2   3 
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2""".stripMargin.split('\n').toList
  )

  val test1 = test("test1") {
    for result <- Day5.part1
    yield assert(result)(equalTo("CMZ"))
  }

  val test2 = test("test1") {
    for result <- Day5.part2
    yield assert(result)(equalTo("MCD"))
  }

  def spec = suite("Day5")(test1, test2).provideShared(ZLayer.succeed(testInput))
}
