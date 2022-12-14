package crooy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*
class Day13Test extends AnyFlatSpec with should.Matchers {

  behavior of "Day13Test"

  it should "parseInput" in {
    val output = Day13.parseInput(testInput)
    output.length should be(8)
  }

  val answers = List(true, true, false, true, false, true, false, false)
  it should "part1 parts" in {
    val output = Day13.parseInput(testInput)
    output.map(_.isInOrder) should be(answers)
  }

  it should "part1.full" in {
    val output = Day13.parseInput(testInput)
    Day13.part1(output) should be(13)
  }

  it should "part2.full" in {
    val output = Day13.parseInput(testInput)
    Day13.part2(output) should be(140)
  }

  val testInput = """[1,1,3,1,1]
                    |[1,1,5,1,1]
                    |
                    |[[1],[2,3,4]]
                    |[[1],4]
                    |
                    |[9]
                    |[[8,7,6]]
                    |
                    |[[4,4],4,4]
                    |[[4,4],4,4,4]
                    |
                    |[7,7,7,7]
                    |[7,7,7]
                    |
                    |[]
                    |[3]
                    |
                    |[[[]]]
                    |[[]]
                    |
                    |[1,[2,[3,[4,[5,6,7]]]],8,9]
                    |[1,[2,[3,[4,[5,6,0]]]],8,9]""".stripMargin.split("\n").toList
}
