import zio.*
import zio.Console.*

object Hello extends ZIOAppDefault:

  val app = print("hello, world")

  def run = app.exitCode
