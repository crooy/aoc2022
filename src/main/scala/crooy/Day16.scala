package crooy

import scala.annotation.tailrec
import scala.collection.immutable

object Day16 {
  lazy val live = FileListInput.read("/day16.txt")
  def main(array: Array[String]) = {
    val volcano = Volcano.parse(live.list)
    val answer  = optimalRoute("AA", volcano)
    println(s"\n\nanswer 1 $answer")
  }

  def optimalRoute(start: String, volcano: Volcano): (List[String], Int) = {
    // strategy, greedily pick the valve with the lowest value/distance ratio
    // aka. napsack problem
    val allRoutes = Routes.create(volcano)

    val routes = allRoutes.map { case (from, rout) =>
      from -> rout.filter { case (node, distance) =>
        volcano(node).flowRate > 0
      }
    }

//    println(s"routes from AA ${Routes.createFor(volcano("AA"), volcano, Set("AA"), true)}")
    println(s"valve total is ${volcano.size}")
    println(s"routes from AA count is  ${routes(start).size}")

    @tailrec
    def greedy(
        from: String,
        seen: List[String],
        timeRemaining: Int,
        totalValue: Int
    ): (List[String], Int) = {
      print(".")
      val options = routes(from)
        .filterNot(n => seen.contains(n._1))
        .collect {
          case (node, distance) if (distance + 1) <= timeRemaining =>
            val value = (timeRemaining - 1 - distance) * volcano(node).flowRate
            (node, value)
        }

      if (options.isEmpty) {
        (seen, totalValue)
      } else {

        val (next, value) = options
          .maxBy { case (_, v) =>
            v
          }
        val costExtra = routes(from)(next) + 1 // opening costs 1
        val flowTime  = timeRemaining - costExtra
        greedy(
          next,
          seen :+ next,
          timeRemaining - costExtra,
          totalValue + (volcano(next).flowRate * flowTime)
        )
      }
    }

    def bruteForce(
        from: String,
        seen: List[String],
        timeRemaining: Int,
        totalValue: Int
    ): List[(List[String], Int)] = {

//      print(".")

      val options = routes(from)
        .filterNot(n => seen.contains(n._1))
        .collect {
          case (node, distance) if (distance + 1) <= timeRemaining =>
            val flowRate  = volcano(node).flowRate
            val timeToUse = (timeRemaining - 1 - distance)
            val value     = timeToUse * flowRate
            (node, value)
        }

      if (options.isEmpty) {
        (seen, totalValue) :: Nil
      } else {
        options.toList.flatMap { case (next, value) =>
          val costExtra = routes(from)(next) + 1 // opening costs 1
          val flowTime  = timeRemaining - costExtra
          val flowRate  = volcano(next).flowRate
          bruteForce(
            next,
            seen :+ next,
            timeRemaining - costExtra,
            totalValue + flowRate * flowTime
          )
        }
      }
    }

    val faster = greedy(start, Nil, 30, 0)
    println(s"greedy guess was ${faster}")

    val all = bruteForce(start, Nil, 30, 0)
    all.maxBy(_._2)
  }

  case class Valve(name: String, flowRate: Int, leadsTo: Set[String])
  object Valve {
    def parse(input: String): (String, Valve) = input match {
      case regex(name, flow, valves) =>
        name -> Valve(name, flow.toInt, valves.split(",").map(_.trim).toSet)
    }
    private val regex =
      "Valve ([A-Z]+) has flow rate=(\\d+); tunnels? leads? to valves? (.+)".r

  }
  type Volcano = Map[String, Valve]
  object Volcano {
    def parse(input: List[String]): Volcano = input.map(Valve.parse).toMap
  }

  type Routes = Map[String, Map[String, Int]]
  object Routes {

    def create(volcano: Volcano): Routes = {
      volcano.values
        .filter(x => x.flowRate > 0 || x.name == "AA")
        .map(start => start.name -> createFor(start, volcano, Set(start.name)))
        .toMap
    }

    def createFor(
        from: Valve,
        volcano: Volcano,
        seen: Set[String],
        debug: Boolean = false
    ): Map[String, Int] = {
      if (debug) {
        println(s"node: $from, seen $seen")
      }

      // all direct unseen neighbours
      val direct: Set[Valve] = from.leadsTo.filterNot(seen.contains).map(volcano)

      val newSeen = seen.union(direct.map(_.name))
      // create BFS map for all neighbours, add + 1 to distances to include this route
      val indirect: Map[String, Int] =
        direct.toList
          .flatMap(n =>
            createFor(n, volcano, newSeen, debug).toList.map { case (o, v) =>
              o -> (v + 1)
            }
          )
          .toMap

      direct.map(v => v.name -> 1).toMap ++ indirect
    }
  }
}
