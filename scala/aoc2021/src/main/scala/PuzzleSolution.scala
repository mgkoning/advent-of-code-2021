import scala.io.Source

trait PuzzleSolution {
  def title: String
  def solve(input: Source): Unit
}
