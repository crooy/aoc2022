import zio.test.*
import zio.test.Assertion.*
import zio.test.Assertion.Render.*

object HelloSpec extends DefaultRunnableSpec:

  val hello = test("outputs hello, world") {
    for
      _ <- Hello.app
      output <- TestConsole.output
    yield assert(output)(equalTo(Vector("hello, world")))
  }

  def spec = suite("Hello")(hello)
