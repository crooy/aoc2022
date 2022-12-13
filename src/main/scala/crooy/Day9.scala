package crooy

import Matrix.*

import scala.annotation.tailrec

object Day9 {

  val live = FileListInput.read("/day9.txt")
  def main(args: Array[String]): Unit = {

    val route         = parseInput(live.list)
    val start         = Point(0, 0)
    val headTravelled = walkHead(start, route, List(Point(0, 0)))
    val tailTravelled = walkTail(start, headTravelled, List(Point(0, 0)))
    println(s"answer 1 is ${tailTravelled.distinct.length}")

    part2(headTravelled)
  }

  def part2(headTravelled: List[Point[_]]) = {
    val tailRoute1 = Day9.walkTail(Point(0, 0), headTravelled, List(Point(0, 0)))
    val tailRoute2 = Day9.walkTail(Point(0, 0), tailRoute1, List(Point(0, 0)))
    val tailRoute3 = Day9.walkTail(Point(0, 0), tailRoute2, List(Point(0, 0)))
    val tailRoute4 = Day9.walkTail(Point(0, 0), tailRoute3, List(Point(0, 0)))
    val tailRoute5 = Day9.walkTail(Point(0, 0), tailRoute4, List(Point(0, 0)))
    val tailRoute6 = Day9.walkTail(Point(0, 0), tailRoute5, List(Point(0, 0)))
    val tailRoute7 = Day9.walkTail(Point(0, 0), tailRoute6, List(Point(0, 0)))
    val tailRoute8 = Day9.walkTail(Point(0, 0), tailRoute7, List(Point(0, 0)))
    val tailRoute9 = Day9.walkTail(Point(0, 0), tailRoute8, List(Point(0, 0)))
    println(s"route of tail $tailRoute9")

    Matrix
      .containing(tailRoute9 ++ headTravelled)
      .print(tailRoute9)

    println(s"positions in tailroute part 2 is ${tailRoute9.distinct.length}")
    tailRoute9.distinct.length
  }

  def parseInput(input: List[String]): List[Matrix.Vector] = {
    val r = "R (\\d+)".r
    val u = "U (\\d+)".r
    val d = "D (\\d+)".r
    val l = "L (\\d+)".r
    input.map {
      case r(amount) => Matrix.Vector.right(amount.toInt)
      case l(amount) => Matrix.Vector.left(amount.toInt)
      case u(amount) => Matrix.Vector.up(amount.toInt)
      case d(amount) => Matrix.Vector.down(amount.toInt)
    }
  }

  @tailrec
  def walkHead(from: Point[_], route: List[Vector], headRoute: List[Point[_]]): List[Point[_]] =
    route match {
      case current :: next => {
        val travelled = current.walk(from)
        val endPoint  = current.applyTo(from)
        println(s"head walks from $from to $endPoint via $travelled")
        walkHead(endPoint, next, headRoute ++ travelled)
      }
      case Nil => headRoute
    }

  @tailrec
  def walkTail(
      from: Point[_],
      stepsOfHead: List[Point[_]],
      routeOfTail: List[Point[_]]
  ): List[Point[_]] =
    stepsOfHead match {
      case headLocation :: next if (headLocation.isNeighbour(from)) || headLocation == from =>
        walkTail(from, next, routeOfTail)

      case headLocation :: next =>
        val possibleSteps = tailSteps.map(_.applyTo(from))
        val tailStep = possibleSteps
          .sortBy(_.distance(headLocation))
          .find(x => x.isNeighbour(headLocation) || headLocation == x)

        tailStep match {
          case Some(value) =>
            println(s"tail step $value for $headLocation")
            val appendedRoute: List[Point[_]] = routeOfTail :+ value
            walkTail(value, next, appendedRoute)

          case None =>
            throw RuntimeException(s"no path from $from to $headLocation")
        }

      case Nil => routeOfTail
    }

  private val tailSteps = Matrix.Vector.up(1) ::
    Matrix.Vector.down(1) :: Matrix.Vector.left(1) ::
    Matrix.Vector.right(1) :: Matrix.Vector(1, 1) ::
    Matrix.Vector(-1, 1) :: Matrix.Vector(1, -1) ::
    Matrix.Vector(-1, -1) :: Nil

}
