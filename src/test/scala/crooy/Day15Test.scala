package crooy

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class Day15Test extends AnyFlatSpec with should.Matchers {

  val testInput = """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
                    |Sensor at x=9, y=16: closest beacon is at x=10, y=16
                    |Sensor at x=13, y=2: closest beacon is at x=15, y=3
                    |Sensor at x=12, y=14: closest beacon is at x=10, y=16
                    |Sensor at x=10, y=20: closest beacon is at x=10, y=16
                    |Sensor at x=14, y=17: closest beacon is at x=10, y=16
                    |Sensor at x=8, y=7: closest beacon is at x=2, y=10
                    |Sensor at x=2, y=0: closest beacon is at x=2, y=10
                    |Sensor at x=0, y=11: closest beacon is at x=2, y=10
                    |Sensor at x=20, y=14: closest beacon is at x=25, y=17
                    |Sensor at x=17, y=20: closest beacon is at x=21, y=22
                    |Sensor at x=16, y=7: closest beacon is at x=15, y=3
                    |Sensor at x=14, y=3: closest beacon is at x=15, y=3
                    |Sensor at x=20, y=1: closest beacon is at x=15, y=3""".stripMargin
    .split("\n")
    .toList

  behavior of "Day15Test"

  it should "part1" in {
    val grid = Day15.SensorGrid.parse(testInput)
    grid.size should be(14)
    grid.find(s => s.x == 2 && s.y == 18).get.value.beacon.x should be(-2)
    Day15.part1(grid, 10) should be(26)
  }

  it should "part2" in {
    val grid = Day15.SensorGrid.parse(testInput)
    Matrix.Point(0, 11, ()).distance(Matrix.Point(14, 11, ())) should be(14)
    Matrix.Point(0, 11, ()).distance(Matrix.Point(2, 10, ())) should be(3)
    Day15.part2(grid, 20) should be(56000011)
  }

}
