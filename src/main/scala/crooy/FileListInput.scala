package crooy

import zio.*

import scala.io.Source

case class FileListInput(name: String, list: List[String])

object FileListInput {

  private def create(name: String) = ZIO.succeed {
    read(name)
  }

  def read(name: String) = {
    val fileStream  = getClass.getResourceAsStream(name)
    val inputStream = Source.fromInputStream(fileStream)
    val result      = Source.fromInputStream(fileStream).getLines.toList
    inputStream.close()
    fileStream.close()
    FileListInput(name, result)
  }

  def layer(name: String): ZLayer[Any, Exception, FileListInput] =
    ZLayer.fromZIO(create(name))
}
