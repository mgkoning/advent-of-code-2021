object Day05 extends PuzzleSolution {
  def title = "Hydrothermal Venture"

  def solve(input: String): Unit =
    val ventLines = parseInput(input).toList
    println("Part 1:")
    val horVert = ventLines.flatMap(horVertCoords).toList
    println(countDuplicates(horVert))
    println("Part 2:")
    val diagonals = ventLines.flatMap(diagonalCoords)
    println(countDuplicates(horVert.concat(diagonals)))

  def countDuplicates[A](values: Seq[A]): Int =
    values.groupBy(identity).filter(_._2.size > 1).size

  def horVertCoords(line: Line): Seq[Coord] =
    if !line.isHorizontalOrVertical
      then Seq.empty
      else for x <- line.xs; y <- line.ys yield Coord(x, y)

  def diagonalCoords(line: Line): Seq[Coord] =
    if !line.isDiagonal
      then Seq.empty
      else line.xs.zip(line.ys).map(Coord.ofTuple)

  def parseInput(input: String): List[Line] =
    input.linesIterator.map {
      case s"$x0,$y0 -> $x1,$y1" => Line(Coord.ofStrings(x0, y0), Coord.ofStrings(x1, y1)),
    }.toList
}
