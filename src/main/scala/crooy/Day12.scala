package crooy
import scala.collection.parallel.CollectionConverters.*
import crooy.Day12.MapPoint

import scala.collection.immutable.{AbstractSet, SortedSet}

object Day12 {

  lazy val live = FileListInput.read("/day12.txt")
  def main(args: Array[String]): Unit = {
    val parsed = parseInput(live.list)
//    val answer1 = part1(parsed)
//    println(s"answer 1 is $answer1")

    val answer2 = part2(parsed)
    println(s"answer 2 is $answer2")
  }

  def part2(input: MapMatrix): Int = {
    val startingPoints =
      input.filter(x => x.value == MapValue.Start || x.value == MapValue.Point('a'))
    val eind = input.find(_.value == MapValue.End).get

    println(s"startingPoints $startingPoints")

    val results = startingPoints.map { start =>
      input.foreach(_.value.reset())
      start.value.testRoute(List())
      start.value.isDone = true
      dijkstra2(input)
      val length = eind.value.getShortestRoute.map(_.length).getOrElse(Int.MaxValue)

      println(s"$start length is $length")
      length
    }
    val shortest = results.min

    println(s"shortest is $shortest")

    shortest
  }

  def part1(input: MapMatrix): Int = {
    val start = input.find(_.value == MapValue.Start).get
    val eind  = input.find(_.value == MapValue.End).get

    start.value.testRoute(List())
    start.value.isDone = true
//    val answer = dijkstra(start, input, eind)
//    println(s"dijkstra says $answer")

    dijkstra2(input)
    println(s"eind says ${eind.value.getShortestRoute.get.length}")
    println(
      s"${input.filter(_.value.getShortestRoute.isEmpty)} points have no distance yet though"
    )

    println(input.routeString(eind.value.getShortestRoute.get))

    eind.value.getShortestRoute.get.length
  }

  def dijkstra2(nodes: MapMatrix): Unit = {
//    val all       = nodes.size
    var done      = nodes.par.filter(_.value.isDone)
    var remaining = nodes.par.filterNot(_.value.isDone)
    var next      = nodes.par.filter(t => done.exists(d => d.isViableRoute(t)))
    while (next.size > 0) {
      next.foreach(findShortestStepTo(done.toList.toSet))
      done = done ++ next.filter(_.value.isDone)
      remaining = remaining.filterNot(_.value.isDone)
      next = remaining.filter(t => done.exists(d => d.isViableRoute(t)))
//      println(s"[${done.size} / ${next.size} / $all]")
    }
  }

  def findShortestStepTo(from: MapMatrix)(node: MapPoint) = {
    from.par
      .filter(_.canBeTravelledTo(node))
      .foreach(f => {
        node.appendToRouteIfShorter(f)
      })
    node.value.isDone = true
  }

  def parseInput(input: List[String]): MapMatrix = {
    input.zipWithIndex.map { case (line, lineNr) =>
      line.toList.zipWithIndex.map {
        case ('S', charIndex)    => Matrix.Point(charIndex, lineNr, MapValue.Start)
        case ('E', charIndex)    => Matrix.Point(charIndex, lineNr, MapValue.End)
        case (letter, charIndex) => Matrix.Point(charIndex, lineNr, MapValue.Point(letter))
      }
    }
  }.flatten.toSet

  type MapMatrix = Set[MapPoint]
  implicit class MapMatrixOps(mapMatrix: MapMatrix) {
    def asString: String = Matrix
      .containing(mapMatrix.toList, 0)
      .asString[MapValue](
        mapMatrix.toList,
        point => {
          point.value.isDone match {
            case true  => point.value.toString
            case false => "."
          }
        }
      )

    def routeString(route: Route): String = Matrix
      .containing(mapMatrix.toList, 0)
      .asString[MapValue](
        mapMatrix.toList,
        point => {
          route.contains(point) match {
            case true  => point.value.toString
            case false => "."
          }
        }
      )
  }
  type Route    = List[MapPoint]
  type MapPoint = Matrix.Point[MapValue]
  implicit class MapPointOps(current: MapPoint) {

    def appendToRouteIfShorter(previous: MapPoint): MapPoint = if (
      previous.canBeTravelledTo(current) && previous.isViableRoute(current)
    ) {
      current.value.testRoute(previous.value.getShortestRoute.get :+ previous)
      current
    } else current

    def canBeTravelledTo(other: MapPoint): Boolean =
      current.isNeighbourIgnoreDiagonal(other) && current.value.canTravelTo(
        other.value
      ) && other != current

    def isViableRoute(toPoint: MapPoint): Boolean = current.canBeTravelledTo(toPoint) && ((
      toPoint.value.getShortestRoute,
      current.value.getShortestRoute
    ) match {
      case (None, None)    => false
      case (None, Some(_)) => true
      case (Some(x), None) => false
      case (Some(existingRoute), Some(newRoute)) =>
        (newRoute.length + 1) < existingRoute.length
    })

  }
  sealed trait MapValue {
    def reset(): Unit = {
      isDone = false
      route = None
      routesSeen.clear()
    }
    var isDone: Boolean              = false
    private var route: Option[Route] = None
    val routesSeen                   = scala.collection.mutable.Set[MapPoint]()
    def testRoute(other: Route): MapValue = {
      if (this.isShorter(other)) {
//        println(
//          s"found a faster route to $this,old ${route
//              .map(_.length)} new ${other.length}, last part is ${other.takeRight(5).map(_.value).mkString}"
//        )
        route = Some(other)
        routesSeen.addAll(other)
      }
      this
    }
    def isShorter(other: Route): Boolean = route.forall(_.length > other.length)
    def getShortestRoute: Option[Route]  = route
    def canTravelTo(neighbour: MapValue): Boolean
  }
  object MapValue {

    implicit object MapValueOrdering extends Ordering[MapValue] {
      override def compare(x: MapValue, y: MapValue): Int =
        (x, y) match {
          case (a, b) if a == b     => 0
          case (Start, _)           => -1
          case (_, End)             => -1
          case (End, _)             => 1
          case (_, Start)           => 1
          case (Point(a), Point(b)) => b.toInt - a.toInt
        }
    }

    case object Start extends MapValue {
      override def toString: String = "S"
      def canTravelTo(neighbour: MapValue): Boolean = neighbour match {
        case Start      => true
        case Point('a') => true
        case Point('b') => true
        case _          => false
      }

    }

    case object End extends MapValue {
      override def toString: String                 = "E"
      def canTravelTo(neighbour: MapValue): Boolean = false
    }
    case class Point(height: Char) extends MapValue {
      override def toString: String = height.toString
      def canTravelTo(neighbour: MapValue): Boolean = neighbour match {
        case Point(neighbourheight) => neighbourheight <= height + 1
        case End                    => height == 'z' || height == 'y'
        case _                      => false
      }
    }
  }
}
