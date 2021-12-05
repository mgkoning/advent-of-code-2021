import scala.io.Source

object Day01 extends PuzzleSolution {

  def title = "Sonar Sweep"

  def solve(input: Source): Unit =
    val depths = input.getLines.map(_.toInt).toList
    println("Part 1:")
    println(differences(depths).filter(0 < _).length)
    println("Part 2:")
    println(differences(windows(depths)).filter(0 < _).length)

  def differences(values: List[Int]) =
    values.zip(values.drop(1)).map((x, y) => y - x)

  def windows(values: List[Int]) =
    values.zip(values.drop(1)).zip(values.drop(2))
      .map((l, r) => l._1 + l._2 + r)
}
