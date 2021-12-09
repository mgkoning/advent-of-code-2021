import scala.io.Source

object Input {
  def getFile(day: Int): Source =
    Source.fromFile(f"../../input/day$day%02d.txt")

  def readGrid[A, B](rows: Seq[String],
                     rowToChunks: String => Seq[A],
                     chunkMap: A => B): Map[Coord, B] =
    rows.zipWithIndex
      .flatMap(
        (row, y) => rowToChunks(row).zipWithIndex
          .map((c, x) => (Coord(x, y), chunkMap(c)))
      )
      .toMap
}
