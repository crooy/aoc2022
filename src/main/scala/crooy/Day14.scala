package crooy
import scala.collection.parallel.CollectionConverters.*
import crooy.Day14.CaveThing
import crooy.Matrix.Point

import scala.collection.parallel.mutable.ParSet

object Day14 {

  lazy val live = FileListInput.read("/day14.txt").list
  def main(args: Array[String]): Unit = {

    val cave = Cave.parse(live)
    println(cave.asString)

    val answer1 = simulateSand(cave)
    println(s"answer1 is $answer1")

    val answer2 = simulateSand(cave.withFloor)
    println(s"answer2 is $answer2")
  }

  def simulateSand(input: Cave): Int = {
    var cave = input
    try {
      var counter = 0
      println(cave.asString)
      while (true) {
        cave = cave.dropSand
        if (counter % 1000 == 0) {
          println(cave.asString)
          println("")
        }
        counter += 1
      }
      Int.MaxValue
    } catch {
      case ReachedAbyssException(sand) =>
        println(cave.asString)
        sand.value.nr
      case SourceBlockedException(sand) =>
        println(cave.asString)
        println(s"o's  = ${cave.asString.count(_ == 'o')}")
        sand.value.nr
      case _: Throwable => Int.MaxValue
    }
  }

  private val pointRegex = "(\\d+),(\\d+)".r

  sealed trait CaveElement

  case object Rock  extends CaveElement
  case object Floor extends CaveElement

  case class Sand(nr: Int) extends CaveElement

  case object SandSource extends CaveElement {
    def reset = {
      counter = 0
    }
    private var counter = 0
    def next: Int = {
      val result = counter
      counter += 1
      result
    }
  }

  type CaveThing  = Matrix.Point[CaveElement]
  type CaveSource = Matrix.Point[SandSource.type]
  type Cave       = ParSet[Matrix.Point[CaveElement]]
  implicit class CavePointOps(cave: Cave) {

    private lazy val floor = cave.collect { case p @ Point(_, _, _ @Floor) =>
      p
    }
    private def isOnFloor(point: Point[_]) = floor.exists(f => f.y == point.y)

    def withFloor: Cave = {
      source.value.reset
      cave.addOne(Matrix.Point[CaveElement](500, matrix.top + 2, Floor))
    }

    private lazy val matrix = Matrix.containing(cave, 0)

    def asString: String = {
      def printer = (p: CaveThing) =>
        p.value match {
          case Sand(_)           => "o"
          case Rock              => "#"
          case SandSource        => "+"
          case _ if isOnFloor(p) => "#"
          case _                 => "."
        }

      matrix.asString(cave, printer, ".")
    }
    def isInAbyss(point: Point[_]): Boolean = matrix.top < point.y

    private lazy val source: CaveSource =
      cave.find(_.value.isInstanceOf[SandSource.type]).get.asInstanceOf[CaveSource]

    private val directions =
      Matrix.Vector(0, 1) :: Matrix.Vector(-1, 1) :: Matrix.Vector(1, 1) :: Nil

    def dropSand: Cave = {
      val sand = this.fall(Point(source.x, source.y, Sand(source.value.next)))
      cave.addOne(sand)
    }
    def isBlocked(point: Point[_]): Boolean = {
      cave.exists(p => p.x == point.x && p.y == point.y) || this.isOnFloor(point)
    }

    private def fall(sand: Point[Sand]): Point[Sand] =
      directions.map(_.applyTo(sand)).find(!this.isBlocked(_)) match {
        case Some(value) if (this.isInAbyss(value)) =>
          throw ReachedAbyssException(value)
        case Some(value) => fall(value)
        case None if (sand.x == source.x && sand.y == source.y) =>
          throw SourceBlockedException(sand)
        case None => sand
      }

  }

  case class ReachedAbyssException(sand: Point[Sand])  extends Throwable
  case class SourceBlockedException(sand: Point[Sand]) extends Throwable

  object Cave {

    def parse(input: List[String]): Cave = {
      val rocks: Set[Point[CaveElement]] = input
        .map(s =>
          s.split(" -> ")
            .map(_.trim)
            .map { case pointRegex(x, y) =>
              Point(x.toInt, y.toInt, Rock)
            }
            .toList
        )
        .map(renderLines)
        .flatten
        .toSet

      scala.collection.parallel.mutable.ParSet().addAll(Set(source) ++ rocks)
    }
  }

  private val source: Point[CaveElement] = Point(500, 0, SandSource)

  private def renderLines[T](points: List[Point[T]]): List[Point[T]] = {
    points match {
      case a :: Nil => List(a)
      case a :: b :: tail =>
        val line = Matrix.Vector.create(a, b).walk(a)
        line ++ renderLines(b :: tail)
      case Nil => Nil
    }
  }
}
