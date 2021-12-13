object Day13 extends PuzzleSolution {
  def title = "Transparent Origami"

  sealed trait Fold
  case class Up(from: Int) extends Fold
  case class Left(from: Int) extends Fold

  def solve(input: scala.io.Source) =
    val (dots, folds) = readPaper(input.getLines.toList)
    println("Part 1:")
    println(foldPaper(dots, folds.head).size)
    println("Part 2:")
    val origami = folds.scanLeft(dots)(foldPaper)
    val result = origami.last
    println(paperToString(result))

  def foldPaper(dots: Set[Coord], fold: Fold) =
    dots.map(dot => fold match
      case Up(y) if dot.y >= y => dot.copy(y = y - dot.y + y)
      case Left(x) if dot.x >= x => dot.copy(x = x - dot.x + x)
      case _ => dot)

  def paperToString(paper: Set[Coord]): String =
    val bottomRight = paper.max
    val word = (0 to bottomRight.y)
      .map(y => (0 to bottomRight.x)
        .map(x => if paper.contains(Coord(x, y)) then '#' else ' ').mkString)
      .mkString("\n")
    s"\n$word\n"

  def readPaper(lines: Seq[String]): (Set[Coord], List[Fold]) =
    lines.foldLeft((Set.empty[Coord], List.empty[Fold]))
      ((acc, line) =>
        val (coords, folds) = acc
        line match
          case s"$x,$y" => (coords + Coord.ofStrings(x, y), folds)
          case s"fold along y=$y" => (coords, folds.appended(Up(y.toInt)))
          case s"fold along x=$x" => (coords, folds.appended(Left(x.toInt)))
          case _ => acc
        )

}
