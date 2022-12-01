package crooy
import zio.ZLayer
import zio.test.*
import zio.test.Assertion.*
import zio.test.Assertion.Render.*

object Day1Spec extends DefaultRunnableSpec {

  val input =
    "1000" :: "2000" :: "3000" :: "" ::
      "4000" :: "" ::
      "5000" :: "6000" :: "" ::
      "7000" :: "8000" :: "9000" :: "" ::
      "10000" :: Nil

  val output = Elf(0, "1000" :: "2000" :: "3000" :: Nil) ::
    Elf(1, "4000" :: Nil) :: Elf(2, "5000" :: "6000" :: Nil) ::
    Elf(3, "7000" :: "8000" :: "9000" :: Nil) :: Elf(4, "10000" :: Nil) :: Nil

  val readElves = test("reads elves correctly") {
    for
      elves <- Day1.splitElves
    yield assert(elves)(equalTo(output))
  }

  def spec = suite("Day1")(readElves).provideShared( ZLayer.succeed(FileListInput( "test", input)) )
}
