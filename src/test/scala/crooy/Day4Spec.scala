package crooy

import zio.ZLayer
import zio.test.*
import zio.test.Assertion.*

object Day4Spec extends ZIOSpecDefault {
  val testInput = FileListInput(
    "test",
    """2-4,6-8
      |2-3,4-5
      |5-7,7-9
      |2-8,3-7
      |6-6,4-6
      |2-6,4-8""".stripMargin.split('\n').toList
  )

  val test1 = test("test1") {
    for result <- Day4.part1
    yield assert(result)(equalTo(2))
  }

  val test2 = test("test2") {
    for result <- Day4.part2
    yield assert(result)(equalTo(4))
  }

  def spec = suite("Day4")(test1, test2).provideShared(ZLayer.succeed(testInput))
}
