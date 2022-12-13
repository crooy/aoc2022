package crooy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*

class Day12Test extends AnyFlatSpec with should.Matchers {

  lazy val testInput: List[String] = """Sabqponm
                    |abcryxxl
                    |accszExk
                    |acctuvwj
                    |abdefghi""".stripMargin.split("\n").toList.map(_.trim).filterNot(_.isEmpty)

  behavior of "Day12Test"

  it should "part1" in {
    val parsed = Day12.parseInput(testInput)
    Day12.part1(parsed) should be(31)
  }

  it should "part2" in {
    val parsed = Day12.parseInput(testInput)
    Day12.part2(parsed) should be(29)
  }

}
