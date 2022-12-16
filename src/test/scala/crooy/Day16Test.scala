package crooy

import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

class Day16Test extends AnyFlatSpec with should.Matchers {

  val testInput =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
      |Valve BB has flow rate=13; tunnels lead to valves CC, AA
      |Valve CC has flow rate=2; tunnels lead to valves DD, BB
      |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
      |Valve EE has flow rate=3; tunnels lead to valves FF, DD
      |Valve FF has flow rate=0; tunnels lead to valves EE, GG
      |Valve GG has flow rate=0; tunnels lead to valves FF, HH
      |Valve HH has flow rate=22; tunnel leads to valve GG
      |Valve II has flow rate=0; tunnels lead to valves AA, JJ
      |Valve JJ has flow rate=21; tunnel leads to valve II""".stripMargin
      .split("\n")
      .toList

  behavior of "Day16Test"

  it should "part1" in {
    val volcano = Day16.Volcano.parse(testInput)
    val route   = Day16.optimalRoute("AA", volcano)
    println(s"route is $route")
    route._2 should be(1651)
  }

}
