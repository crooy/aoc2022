package crooy

import play.api.libs.json._

object Day13 {
  lazy val live = FileListInput.read("/day13.txt")

  def main(args: Array[String]): Unit = {

    val packets = parseInput(live.list)
    val answer1 = part1(packets)
    println(s"answer1 is $answer1")
    val answer2 = part2(packets)
    println(s"answer2 is $answer2")
  }

  def parseInput(input: List[String]): List[Packet] =
    input.grouped(3).toList.map { case left :: right :: _ =>
      (Json.parse(left).as[PacketValue], Json.parse(right).as[PacketValue])
    }

  def part1(parsedInput: List[Packet]): Int = {
    parsedInput.zipWithIndex.collect {
      case (packet, i) if packet.isInOrder => i + 1
    }.sum
  }

  def part2(parsedInput: List[Packet]): Int = {
    val packets: List[PacketValue] = parsedInput.flatMap { case (value1, value2) =>
      value1 :: value2 :: Nil
    }
    val dividerPackets: List[PacketValue] =
      ListValue(List(ListValue(List(IntValue(2))))) :: ListValue(
        List(ListValue(List(IntValue(6))))
      ) :: Nil

    val sorted = (packets ++ dividerPackets).sorted

    val indexes = sorted.zipWithIndex.collect {
      case (value, i) if dividerPackets.contains(value) => i + 1
    }
    indexes.reduce(_ * _)
  }

  sealed trait PacketValue {
    def isSmallerThan(other: PacketValue): Either[Unit, Boolean]
  }
  case class IntValue(value: Integer) extends PacketValue {
    override def toString: String = s"$value"
    def isSmallerThan(other: PacketValue): Either[Unit, Boolean] = {
      println(s"compare $this to $other")
      val result = other match {
        case IntValue(otherValue) if (value == otherValue) => Left(())
        case IntValue(otherValue)                          => Right(value < otherValue)
        case _: ListValue => ListValue(List(this)).isSmallerThan(other)
      }
      println(s"[$result] : $this <<<< $other ")
      result
    }
  }
  object IntValue {
    implicit val reads: Reads[IntValue] = _.validate[Int].map(IntValue(_))
  }
  case class ListValue(value: List[PacketValue]) extends PacketValue {
    override def toString: String = s"[${value.mkString(",")}]"
    def isSmallerThan(other: PacketValue): Either[Unit, Boolean] = {
      println(s"compare $this to $other")
      val result = other match {
        case _: IntValue =>
          this.isSmallerThan(ListValue(List(other)))
        case ListValue(otherValue) => {
          val length = Math.max(value.length, otherValue.length)

          val results = (Range(0, length)
            .map { i =>
              (value.drop(i).headOption, otherValue.drop(i).headOption) match {
                case (Some(l), Some(r)) => l.isSmallerThan(r)
                case (None, Some(_))    => Right(true)
                case (Some(_), None)    => Right(false)
                case (None, None)       => Right(false)
              }
            })

          val onlyRights = results
            .filter(_.isRight)
          val endResult = onlyRights.headOption.flatMap(_.toOption)
          endResult match {
            case Some(value) => Right(value)
            case None        => Left(())
          }

        }
      }
      println(s"[$result] : $this <<<< $other ")
      result
    }
  }
  object ListValue {
    def fromJson(list: List[JsValue]) = ListValue(list.map(_.as[PacketValue]))
    implicit val reads: Reads[ListValue] = (json: JsValue) => {
      json.validate[List[JsValue]].map(ListValue.fromJson)
    }

  }

  object PacketValue {
    implicit val reads: Reads[PacketValue] = (json: JsValue) => {
      json
        .validate[IntValue]
        .orElse(json.validate[ListValue])
    }
    implicit object PacketOrdering extends Ordering[PacketValue] {
      override def compare(x: PacketValue, y: PacketValue): Int =
        if (x == y) 0 else if (x.isSmallerThan(y).contains(true)) -1 else 1
    }
  }

  type Packet = (PacketValue, PacketValue)

  implicit class PacketOps(packet: Packet) {
    def isInOrder: Boolean = packet match {
      case (left, right) =>
        val result = left.isSmallerThan(right)
        println(s"$packet isInOrder = $result")
        result.contains(true)
    }

  }
}
