package crooy

import com.sun.org.apache.bcel.internal.generic.RETURN
import crooy.Matrix.*

import scala.collection.parallel.CollectionConverters.*
import scala.collection.parallel.mutable.ParSet
import zio.stream.ZStream
import zio.{FiberRefs, Runtime, RuntimeFlags, Unsafe, ZEnvironment}

import scala.collection.immutable.Nil
object Day15 {
  def main(args: Array[String]): Unit = {
    val grid    = SensorGrid.parse(live)
    val answer1 = part1(grid, 2000000)
    println(s"answer 1 is $answer1")
    assert(answer1 == 5878678, "wrong answer 1")

    val answer2 = part2(grid, 4000000)
    println(s"answer 2 is $answer2")
  }

  def part2(grid: SensorGrid, searchSize: Int): Long = {

    def overlap(biglist: (Long, Long), smalllist: (Long, Long)) = {
      (biglist._1 <= smalllist._1 && smalllist._1 <= biglist._2 && biglist._1 <= smalllist._2 && smalllist._2 <= biglist._2)
    }

    val beacon = Range.inclusive(0, searchSize).par.flatMap { y =>
      val ignore = grid.getImpossibleRange(y)
      //        val impossibleValues = ignore.map { case (a, b) => Math.abs(b - a) }.sum
      //        println(s"impossibleValues = $impossibleValues searchSize $searchSize")
      val fullyOverlap = ignore.exists(a => overlap(a, (0, searchSize)))
      if (fullyOverlap) {
//        println(s"y $y is skipped")
        Nil
      } else {
        println(s"found $y ")
        Range
          .inclusive(0, searchSize)
          .par
          .filterNot(x => ignore.exists { case (a, b) => a <= x && x <= b })
          .map { x =>
            Point(x, y, 0)
          }
          .find { p =>
            if (p.x % 10000 == 0 && p.y % 10000 == 0) {
              println(s"testing point $p")
            }
            !grid.exists { sensor =>
              sensor.couldNotHaveBeaconIncludingKnown(p)
            }
          }
      }

    }
//
//    val runtime = zio.Runtime(ZEnvironment.empty, FiberRefs.empty, RuntimeFlags.default)
//    val beacon1 = ZStream
//      .range(0, searchSize)
//      .flatMapPar(8, 32) { y =>
//
//        val ignore = grid.getImpossibleRange(y)
////        val impossibleValues = ignore.map { case (a, b) => Math.abs(b - a) }.sum
////        println(s"impossibleValues = $impossibleValues searchSize $searchSize")
//        val fullyOverlap = ignore.exists(a => overlap(a, (0, searchSize)))
//        if (fullyOverlap) {
//          println(s"y $y is skipped")
//          ZStream.empty
//        } else {
//          println(s"testing $y ")
//          ZStream
//
//        }
//      }
//      .runHead
//
//    val beacon = Unsafe.unsafe { implicit unsafe =>
//      runtime.unsafe.run(beacon1).getOrThrowFiberFailure()
//    }

//
//    val beacon = Range
//      .inclusive(0, searchSize)
//      .to(LazyList)
//      .flatMap(x => Range.inclusive(0, searchSize).to(LazyList).map(y => Point(x, y, 0)))
//      .par
//      .filter { p =>
//        println(s"testing point $p")
//      !grid.exists { sensor =>
//        sensor.couldNotHaveBeaconIncludingKnown(p)
//      }
//      }

    val answer = beacon.map(p => (p.x.toLong * 4000000) + p.y).head
    println(s"$beacon found")
    answer
  }

  def part1(grid: SensorGrid, rowNr: Int): Long = {
    val maxDistance = grid.map(s => s.distance(s.value.beacon)).max
    val world       = Matrix.containing(grid, maxDistance)

    val ranges = grid.getImpossibleRange(rowNr)
    println(s"ranges is ${ranges.toList}")
    ranges.map { case (a, b) => Math.abs(b - a) }.sum
//    println(s"test part 1 = $test")
//
//    Range
//      .inclusive(world.left, world.right)
//      .toList
//      .par
////      .filter(x => grid.cannotHaveBeacon(x, rowNr))
//      .map(x => Point(x, rowNr, 0))
//      .map { p =>
////        println(s"testing point $p")
//        p
//      }
//      .filter { p =>
////        println(s"testing point $p")
//        grid.exists { sensor =>
//          val result = sensor.couldNotHaveBeacon(p)
////          if (true) println(s"$p could not have beacon = $result, reported $sensor");
//          result
//        }
//      }
//      .size
  }

  case class Sensor(beacon: Point[Any])
  type SensorGrid = ParSet[Matrix.Point[Sensor]]
  object SensorGrid {
    private val regex =
      "Sensor at x=(-?[0-9]+), y=(-?[0-9]+): closest beacon is at x=(-?[0-9]+), y=(-?[0-9]+)".r
    def parse(input: List[String]): SensorGrid = ParSet().addAll(
      input.map { case regex(x, y, a, b) =>
        Point(x.toInt, y.toInt, Sensor(Point(a.toInt, b.toInt, 1)))
      }
    )
  }
  implicit class SensorGridOps(grid: SensorGrid) {
    def cannotHaveBeacon(x: Int, y: Int) =
      grid.getImpossibleRange(y).exists { case (a, b) => a <= x && x <= b }

    def getImpossibleRange(y: Int): Seq[(Long, Long)] = {

      val fullList = grid
        .flatMap(_.impossibleRange(y))
        .toList
        .distinct
        .sortBy(_._1)

      fullList.tail.foldLeft(List[(Long, Long)](fullList.head))((shortList, a) =>
        (shortList.flatMap {
          case b if (a._1 <= b._1 && b._1 <= a._2) =>
            (a._1, Math.max(a._2, b._2)) :: Nil
          case b if (b._1 <= a._1 && a._1 <= b._2) =>
            (b._1, Math.max(a._2, b._2)) :: Nil
          case b =>
            b :: a :: Nil
        }).distinct
      )
    }

  }

  implicit class SensorOps(sensor: Point[Sensor]) {

    private lazy val radius = sensor.distance(sensor.value.beacon)
    def impossibleRange(y: Int): Option[(Long, Long)] = {
      val length = Math.max(0, radius - Math.abs(y - sensor.y).toLong)
      if (length == 0) {
        None
      } else {
        val result = (
          sensor.x - length,
          sensor.x + length
        )

        assert(result._1 <= result._2)
        Some(result)
      }
    }
    def couldNotHaveBeaconIncludingKnown(point: Matrix.Point[_]) =
      (sensor.distance(point) <= radius ||
        (sensor.x == point.x && sensor.y == point.y)) ||
        (sensor.value.beacon.x == point.x && sensor.value.beacon.y == point.y)

    def couldNotHaveBeacon(point: Matrix.Point[_]) =
      (sensor.distance(point) <= radius ||
        (sensor.x == point.x && sensor.y == point.y)) &&
        !(sensor.value.beacon.x == point.x && sensor.value.beacon.y == point.y)
  }

  val live = """Sensor at x=489739, y=1144461: closest beacon is at x=-46516, y=554951
               |Sensor at x=2543342, y=3938: closest beacon is at x=2646619, y=229757
               |Sensor at x=3182359, y=3999986: closest beacon is at x=3142235, y=3956791
               |Sensor at x=3828004, y=1282262: closest beacon is at x=3199543, y=2310713
               |Sensor at x=871967, y=3962966: closest beacon is at x=-323662, y=4519876
               |Sensor at x=1323641, y=2986163: closest beacon is at x=2428372, y=3303736
               |Sensor at x=2911492, y=2576579: closest beacon is at x=3022758, y=2461675
               |Sensor at x=3030965, y=2469848: closest beacon is at x=3022758, y=2461675
               |Sensor at x=3299037, y=3402462: closest beacon is at x=3142235, y=3956791
               |Sensor at x=1975203, y=1672969: closest beacon is at x=1785046, y=2000000
               |Sensor at x=3048950, y=2452864: closest beacon is at x=3022758, y=2461675
               |Sensor at x=336773, y=2518242: closest beacon is at x=1785046, y=2000000
               |Sensor at x=1513936, y=574443: closest beacon is at x=2646619, y=229757
               |Sensor at x=3222440, y=2801189: closest beacon is at x=3199543, y=2310713
               |Sensor at x=2838327, y=2122421: closest beacon is at x=2630338, y=2304286
               |Sensor at x=2291940, y=2502068: closest beacon is at x=2630338, y=2304286
               |Sensor at x=2743173, y=3608337: closest beacon is at x=2428372, y=3303736
               |Sensor at x=3031202, y=2452943: closest beacon is at x=3022758, y=2461675
               |Sensor at x=3120226, y=3998439: closest beacon is at x=3142235, y=3956791
               |Sensor at x=2234247, y=3996367: closest beacon is at x=2428372, y=3303736
               |Sensor at x=593197, y=548: closest beacon is at x=-46516, y=554951
               |Sensor at x=2612034, y=2832157: closest beacon is at x=2630338, y=2304286
               |Sensor at x=3088807, y=3929947: closest beacon is at x=3142235, y=3956791
               |Sensor at x=2022834, y=2212455: closest beacon is at x=1785046, y=2000000
               |Sensor at x=3129783, y=3975610: closest beacon is at x=3142235, y=3956791
               |Sensor at x=3150025, y=2333166: closest beacon is at x=3199543, y=2310713
               |Sensor at x=3118715, y=2376161: closest beacon is at x=3199543, y=2310713
               |Sensor at x=3951193, y=3181929: closest beacon is at x=4344952, y=3106256
               |Sensor at x=2807831, y=2401551: closest beacon is at x=2630338, y=2304286
               |Sensor at x=3683864, y=2906786: closest beacon is at x=4344952, y=3106256
               |Sensor at x=2723234, y=3206978: closest beacon is at x=2428372, y=3303736
               |Sensor at x=3047123, y=3891244: closest beacon is at x=3142235, y=3956791
               |Sensor at x=3621967, y=3793314: closest beacon is at x=3142235, y=3956791
               |Sensor at x=2384506, y=1814055: closest beacon is at x=2630338, y=2304286
               |Sensor at x=83227, y=330275: closest beacon is at x=-46516, y=554951
               |Sensor at x=3343176, y=75114: closest beacon is at x=2646619, y=229757
               |""".stripMargin.split("\n").toList
}
