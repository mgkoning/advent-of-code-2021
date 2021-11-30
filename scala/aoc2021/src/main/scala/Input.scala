import scala.io.Source

object Input {
  private def getFile(day: Int): Source =
    Source.fromFile(f"../../input/day$day%02d.txt")

  def asString(day: Int) =
    getFile(day).mkString

  def asLines(day: Int) =
    getFile(day).getLines
}
