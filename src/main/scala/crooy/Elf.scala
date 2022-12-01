package crooy

case class Elf(index: Int, food: List[String]){
  lazy val calories:Int = food.map(_.toInt).sum
}

private object Elf {
  def next(current: Elf): Elf = Elf(current.index + 1, Nil)
  def zero: Elf = Elf(0, Nil)
  def addTo(elf:Elf, value:String): Elf = Elf(elf.index, elf.food :+ value)
}