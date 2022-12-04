import zio.test.*
import zio.test.Assertion.*

object HelloSpec extends ZIOSpecDefault:

  val hello = test("outputs hello, world") {
    for
      _ <- Hello.app
      output <- TestConsole.output
    yield assert(output)(equalTo(Vector("hello, world")))
  }

  def spec = suite("Hello")(hello)
