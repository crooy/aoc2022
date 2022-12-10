package crooy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.*
import flatspec.*
import matchers.*

class Day10Test extends AnyFlatSpec with should.Matchers {

  behavior of "Day10Test"

  val smallInput =
    """noop
      |addx 3
      |addx -5""".stripMargin.split("\n").toList

  val part2Output = """##..##..##..##..##..##..##..##..##..##..
                      |###...###...###...###...###...###...###.
                      |####....####....####....####....####....
                      |#####.....#####.....#####.....#####.....
                      |######......######......######......####
                      |#######.......#######.......#######.....""".stripMargin

  it should "parseInput" in {
    val parsed = Day10.parseInput(smallInput)
    parsed.length should be(3)
    parsed.last should be(Day10.Operation.AddX(-5))
  }

  it should "run" in {
    val parsed  = Day10.parseInput(smallInput)
    val outcome = Day10.run(parsed)
    println(outcome)
    outcome.last.registerX should be(-1)
    outcome.last.cycle should be(6)
  }

  it should "part1" in {

    val expected    = 13140
    val states      = Day10.run(Day10.parseInput(largeInput))
    val part1Answer = Day10.part1(states)
    part1Answer should be(expected)
  }

  it should "part2" in {

    val states = Day10.run(Day10.parseInput(largeInput))
    println("running:")
    val part2Answer = Day10.part2(states)
    println("actual:")
    println(part2Answer)
    println("expecting:")
    println(part2Output)
    part2Answer should be(part2Output)

  }

  val largeInput = """addx 15
                     |addx -11
                     |addx 6
                     |addx -3
                     |addx 5
                     |addx -1
                     |addx -8
                     |addx 13
                     |addx 4
                     |noop
                     |addx -1
                     |addx 5
                     |addx -1
                     |addx 5
                     |addx -1
                     |addx 5
                     |addx -1
                     |addx 5
                     |addx -1
                     |addx -35
                     |addx 1
                     |addx 24
                     |addx -19
                     |addx 1
                     |addx 16
                     |addx -11
                     |noop
                     |noop
                     |addx 21
                     |addx -15
                     |noop
                     |noop
                     |addx -3
                     |addx 9
                     |addx 1
                     |addx -3
                     |addx 8
                     |addx 1
                     |addx 5
                     |noop
                     |noop
                     |noop
                     |noop
                     |noop
                     |addx -36
                     |noop
                     |addx 1
                     |addx 7
                     |noop
                     |noop
                     |noop
                     |addx 2
                     |addx 6
                     |noop
                     |noop
                     |noop
                     |noop
                     |noop
                     |addx 1
                     |noop
                     |noop
                     |addx 7
                     |addx 1
                     |noop
                     |addx -13
                     |addx 13
                     |addx 7
                     |noop
                     |addx 1
                     |addx -33
                     |noop
                     |noop
                     |noop
                     |addx 2
                     |noop
                     |noop
                     |noop
                     |addx 8
                     |noop
                     |addx -1
                     |addx 2
                     |addx 1
                     |noop
                     |addx 17
                     |addx -9
                     |addx 1
                     |addx 1
                     |addx -3
                     |addx 11
                     |noop
                     |noop
                     |addx 1
                     |noop
                     |addx 1
                     |noop
                     |noop
                     |addx -13
                     |addx -19
                     |addx 1
                     |addx 3
                     |addx 26
                     |addx -30
                     |addx 12
                     |addx -1
                     |addx 3
                     |addx 1
                     |noop
                     |noop
                     |noop
                     |addx -9
                     |addx 18
                     |addx 1
                     |addx 2
                     |noop
                     |noop
                     |addx 9
                     |noop
                     |noop
                     |noop
                     |addx -1
                     |addx 2
                     |addx -37
                     |addx 1
                     |addx 3
                     |noop
                     |addx 15
                     |addx -21
                     |addx 22
                     |addx -6
                     |addx 1
                     |noop
                     |addx 2
                     |addx 1
                     |noop
                     |addx -10
                     |noop
                     |noop
                     |addx 20
                     |addx 1
                     |addx 2
                     |addx 2
                     |addx -6
                     |addx -11
                     |noop
                     |noop
                     |noop""".stripMargin.split("\n").toList

}
