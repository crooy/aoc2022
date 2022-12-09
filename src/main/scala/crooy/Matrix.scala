package crooy

// x,y 0,0 == bottom left
case class Matrix(left: Int, right: Int, top: Int, bottom: Int)

object Matrix {
  def containing(points: List[Point], padding: Int = 1): Matrix =
    Matrix(
      points.map(_.x).min - padding,
      padding + points.map(_.x).max,
      padding + points.map(_.y).max,
      points.map(_.y).min - padding
    )

  implicit class MatrixOps(matrix: Matrix) {
    def print(
        points: List[Point],
        start: String = "s",
        step: String = "x",
        end: String = "e",
        empty: String = "."
    ): Unit = {
      Range.inclusive(matrix.top, matrix.bottom, -1).map { y =>
        val line = Range.inclusive(matrix.left, matrix.right).map { x =>
          points.indexOf(Point(x, y)) match {
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

  case class Point(x: Int, y: Int)

  case class Vector(x: Int, y: Int)
  object Vector {
    def create(from: Point, to: Point): Vector = Vector(to.x - from.x, to.y - from.y)
    def left(amount: Int)                      = Vector(0 - amount, 0)
    def down(amount: Int)                      = Vector(0, 0 - amount)
    def right(amount: Int)                     = Vector(amount, 0)
    def up(amount: Int)                        = Vector(0, amount)
  }

  implicit class VectorOps(vector: Vector) {

    def shorten(max: Int) = Vector(
      Math.max(max, vector.x),
      Math.max(max, vector.y)
    )
    def applyTo(point: Point): Point =
      Point(point.x + vector.x, point.y + vector.y)

    def walk(point: Point): List[Point] =
      (for {
        x <- Range.inclusive(0, vector.x, if (vector.x > 0) 1 else -1)
        y <- Range.inclusive(0, vector.y, if (vector.y > 0) 1 else -1)
      } yield Point(point.x + x, point.y + y)).toList

  }

  implicit class PointOps(point: Point) {
    def applyTo(vector: Vector): Point =
      Point(point.x + vector.x, point.y + vector.y)

    def distance(other: Point): Int =
      Math.abs(point.x - other.x) + Math.abs(point.y - other.y)

    def isNeighbour(other: Point) =
      distance(other) < 3 &&
        Math.abs(point.x - other.x) < 2 &&
        Math.abs(point.y - other.y) < 2

    def isNeighbourIgnoreDiagonal(other: Point) =
      distance(other) < 2 &&
        Math.abs(point.x - other.x) < 2 &&
        Math.abs(point.y - other.y) < 2
  }

}
