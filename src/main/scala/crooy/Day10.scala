package crooy

import crooy.Day10.State

import scala.annotation.tailrec

object Day10 {

  lazy val live = FileListInput.read("/day10.txt").list
  val valuesAt  = List(20, 60, 100, 140, 180, 220)
  def main(args: Array[String]): Unit = {
    val program = parseInput(live)
    val states  = run(program)
    println(s"answer 1 is ${part1(states)}")
    println(part2(states))
  }

  def part2(states: List[State]): String = {

    states
      .dropRight(1)
      .map { s =>
        val pos       = ((s.cycle - 1) % 40)
        val spritePos = s.registerX
        (if (spritePos - 1 <= pos && pos <= spritePos + 1) {
//           print(pos % 10)
           "#"
         } else {
//           print(".")
           "."
         })
          + (if (s.cycle % 40 == 0) {
//             print("\n")
               "\n"
             } else {
               ""
             })
      }
      .mkString("")
      .trim

  }

  def part1(states: List[State]): Int = {
    val selected = valuesAt.flatMap(cycle => states.find(_.cycle == cycle))
//    println(selected)
    selected.map(_.signal).sum
  }

  def run(program: List[Operation]): List[State] = step(program, State.zero :: Nil)

  @tailrec
  private def step(program: List[Operation], states: List[State]): List[State] = program match {
    case head :: tail =>
      val current = states.last
      val updated = head.execute(current)
      step(tail, states ++ updated)
    case Nil => states
  }

  case class State(cycle: Int, registerX: Int) {
    // indicates value of register x during this cycle
    def signal = cycle * registerX
  }
  object State {
    def zero: State = State(1, 1)
  }

  def parseInput(input: List[String]): List[Operation] = input match {
    case "noop" :: tail                => Operation.Noop :: parseInput(tail)
    case Operation.addx(value) :: tail => Operation.AddX(value.toInt) :: parseInput(tail)
    case Nil                           => Nil
  }

  sealed trait Operation {
    def execute(state: State): List[State]
  }
  object Operation {
    val addx = "addx (-?\\d+)".r
    case object Noop extends Operation {
      def execute(state: State): List[State] =
        List(
          State(state.cycle + 1, state.registerX)
        )
    }
    case class AddX(value: Int) extends Operation {
      def execute(state: State): List[State] = List(
        State(state.cycle + 1, state.registerX),
        State(state.cycle + 2, state.registerX + value)
      )

    }
  }
}
