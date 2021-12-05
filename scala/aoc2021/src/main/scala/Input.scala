import scala.io.Source

object Input {
  def getFile(day: Int): Source =
    Source.fromFile(f"../../input/day$day%02d.txt")
}
