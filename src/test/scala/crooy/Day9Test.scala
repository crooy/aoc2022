package crooy

import crooy.Matrix.Point

import collection.mutable.Stack
import org.scalatest.*
import flatspec.*
import matchers.*

class Day9Test extends AnyFlatSpec with should.Matchers {

  val expectedParsedOutput = List(
    Matrix.Vector.right(4),
    Matrix.Vector.up(4),
    Matrix.Vector.left(3),
    Matrix.Vector.down(1),
    Matrix.Vector.right(4),
    Matrix.Vector.down(1),
    Matrix.Vector.left(5),
    Matrix.Vector.right(2)
  )

  val testInput = """R 4
                    |U 4
                    |L 3
                    |D 1
                    |R 4
                    |D 1
                    |L 5
                    |R 2""".stripMargin.split("\n").toList
  val parsedInput = Day9.parseInput(testInput)

  val testInput2 = """R 5
                     |U 8
                     |L 8
                     |D 3
                     |R 17
                     |D 10
                     |L 25
                     |U 20""".stripMargin.stripMargin.split("\n").toList
  val parsedInput2 = Day9.parseInput(testInput2)

  "The parser" should "parse input" in {
    parsedInput should be(expectedParsedOutput)
  }

  "walkhead" should "end up in the right place" in {
    val route = Day9.walkHead(Point(0, 0), expectedParsedOutput, List(Point(0, 0)))
    route.last should be(Point(2, 2))
  }

  "walktail" should "end up in the right place" in {
    val headRoute = Day9.walkHead(Point(0, 0), expectedParsedOutput, List(Point(0, 0)))
    println(s"route of head $headRoute")
    val tailRoute = Day9.walkTail(Point(0, 0), headRoute, List(Point(0, 0)))
    println(s"route of tail $tailRoute")
    tailRoute.last should be(Point(1, 2))
    tailRoute.distinct.length should be(13)

    Matrix
      .containing(tailRoute ++ headRoute)
      .print(tailRoute)
  }

  "walktail" should "end up in the right place part 2" in {
    val headRoute = Day9.walkHead(Point(0, 0), parsedInput2, List(Point(0, 0)))
    println(s"route of head $headRoute")
    val answer = Day9.part2(headRoute)
    answer should be(36)
  }
}
