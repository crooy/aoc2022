package crooy

import crooy.Day2.Choice.Rock.intrinsicValue
import crooy.Day2.Outcome.Draw
import zio.*

object Day2 extends ZIOAppDefault{
  override def run = (for{
    _ <- ZIO.logInfo("day2 start")
    strategy <- parseInput
    score1 <- calculateScoreV1(strategy)
    score2 <- calculateScoreV2(strategy)
    _ <- ZIO.logInfo(s"day2 score1 $score1")
    _ <- ZIO.logInfo(s"day2 score2 $score2")
  } yield ()).provide(FileListInput.layer("/day2.txt"))

  def parseInput:ZIO[FileListInput, Exception, List[Round]] = ZIO.service[FileListInput].flatMap { input =>
    ZIO.succeed(input.list.map( Round.parse ))
  }
  def calculateScoreV1(strategy:List[Round]):ZIO[Any, Exception, Int] =
    ZIO.succeed( strategy.map( Round.score ).sum )

  def calculateScoreV2(strategy: List[Round]): ZIO[Any, Exception, Int] =
      ZIO.succeed(strategy.map(Round.v2Score).sum)

  sealed trait Choice{

    val intrinsicValue:Int
    val rules: Map[Choice,Outcome]
    def score(other: Choice): Int = intrinsicValue + rules(other).score
  }
  object Choice {
    val choices = Rock :: Paper :: Scissor :: Nil
    import Outcome._
    case object Rock extends Choice {
      val intrinsicValue: Int = 1

      val rules = Map(
         Rock -> Draw,
         Paper -> Lose,
         Scissor -> Win
       )
  }

    case object Paper extends Choice{
      val intrinsicValue:Int = 2
      val rules = Map(
        Rock -> Win,
        Paper -> Draw,
        Scissor -> Lose
      )
    }
    case object Scissor extends Choice{
      val intrinsicValue:Int = 3
      val rules = Map(
        Rock -> Lose,
        Paper -> Win,
        Scissor -> Draw
      )
    }
    def parse(input:String) = input match
      case "A"|"X" => Rock
      case "B"|"Y" => Paper
      case "C"|"Z" => Scissor
  }
  sealed trait Outcome{
    val score:Int
  }
  object Outcome{
    case object Lose extends Outcome{
      val score:Int = 0
    }
    case object Draw extends Outcome{
      val score:Int = 3
    }
    case object Win extends Outcome{
      val score:Int = 6
    }
    def parse(input:String):Outcome = input match
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
  }
  case class Round(opponents:Choice, ourChoice:Choice, goal:Outcome)
  object Round{
    def parse(input:String) = Round(
      Choice.parse(input.take(1)),
      Choice.parse(input.drop(2)),
      Outcome.parse(input.drop(2))
    )
    def score(round:Round) = round.ourChoice.score(round.opponents)
    def v2Score(input:Round):Int = {
      val ourPick = Choice.choices.find( _.rules(input.opponents) == input.goal).get
      val v2Round = Round(input.opponents, ourPick, input.goal)
      score(v2Round)
    }
  }
}
