package crooy

import java.util

// x,y 0,0 == bottom left
case class Matrix(left: Int, right: Int, top: Int, bottom: Int)

object Matrix {
  def containing(points: IterableOnce[Point[_]], padding: Int = 1): Matrix =
    Matrix(
      points.map(_.x).min - padding,
      padding + points.map(_.x).max,
      padding + points.map(_.y).max,
      points.map(_.y).min - padding
    )

  implicit class MatrixOps(matrix: Matrix) {

    def asString[T](
        points: IterableOnce[Point[T]],
        printer: (Point[T]) => String,
        space: String = ""
    ): String = {
      Range
        .inclusive(matrix.top, matrix.bottom, -1)
        .map { y =>
          val line = Range.inclusive(matrix.left, matrix.right).map { x =>
            points.find(p => p.x == x && p.y == y) match {
              case Some(value) => printer(value)
              case None        => space
            }
          }
          line.mkString("")
        }
        .reverse
        .mkString("\n")
    }

    def print(
        points: List[Point[_]],
        start: String = "s",
        step: String = "x",
        end: String = "e",
        empty: String = "."
    ): Unit = {
      Range.inclusive(matrix.top, matrix.bottom, -1).map { y =>
        val line = Range.inclusive(matrix.left, matrix.right).map { x =>
          points.indexWhere(p => p.x == x && p.y == y) match {
            case 0                               => start
            case n if (n == (points.length - 1)) => end
            case -1                              => empty
            case _                               => step
          }
        }
        println(line.mkString(""))
      }
    }
  }

  case class Point[+T](x: Int, y: Int, value: T = 0)

  case class Vector(x: Int, y: Int)
  object Vector {
    def create(from: Point[_], to: Point[_]): Vector = Vector(to.x - from.x, to.y - from.y)
    def left(amount: Int)                            = Vector(0 - amount, 0)
    def down(amount: Int)                            = Vector(0, 0 - amount)
    def right(amount: Int)                           = Vector(amount, 0)
    def up(amount: Int)                              = Vector(0, amount)

  }

  implicit class VectorOps(vector: Vector) {

    def shorten(max: Int) = Vector(
      Math.max(max, vector.x),
      Math.max(max, vector.y)
    )
    def applyTo[T](point: Point[T]): Point[T] =
      Point(point.x + vector.x, point.y + vector.y, point.value)

    def walk[T](point: Point[T]): List[Point[T]] =
      (for {
        x <- Range.inclusive(0, vector.x, if (vector.x > 0) 1 else -1)
        y <- Range.inclusive(0, vector.y, if (vector.y > 0) 1 else -1)
      } yield Point(point.x + x, point.y + y, point.value)).toList

  }

  implicit class PointOps[T](point: Point[T]) {
    def applyTo(vector: Vector): Point[T] =
      point.copy(x = point.x + vector.x, y = point.y + vector.y)

    def distance(other: Point[_]): Int =
      Math.abs(point.x - other.x) + Math.abs(point.y - other.y)

    def isNeighbour(other: Point[_]) =
      distance(other) < 3 &&
        Math.abs(point.x - other.x) < 2 &&
        Math.abs(point.y - other.y) < 2

    def isNeighbourIgnoreDiagonal(other: Point[_]) =
      distance(other) < 2 &&
        Math.abs(point.x - other.x) < 2 &&
        Math.abs(point.y - other.y) < 2
  }

}
