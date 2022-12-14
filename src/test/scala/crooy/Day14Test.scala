package crooy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.matchers.should
import org.scalatest.matchers.should
import org.scalatest.*
import flatspec.*
import matchers.*

class Day14Test extends AnyFlatSpec with should.Matchers {

  val testInput = """498,4 -> 498,6 -> 496,6
                    |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin.split("\n").toList

  behavior of "Day14Test"

  it should "parseInput" in {
    val cave = Day14.Cave.parse(testInput)
    println(cave.asString)
    cave.size should be(21)
  }

  it should "part1" in {
    val cave = Day14.Cave.parse(testInput)
    Day14.simulateSand(cave) should be(24)
  }

  it should "part2" in {
    val cave = Day14.Cave.parse(testInput)
    Day14.simulateSand(cave.withFloor) should be(92)
  }

}
