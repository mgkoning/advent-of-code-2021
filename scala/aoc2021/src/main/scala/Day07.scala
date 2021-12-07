import scala.io.Source

object Day07 extends PuzzleSolution {
  def title = "The Treachery of Whales"

  def solve(input: Source) =
    val crabPositions = readCrabPositions(input.getLines.toList.head).sorted
    println("Part 1:")
    println(minFuel(crabPositions, identity))
    println("Part 2:")
    val fuelMap = incrementalFuelMap(crabPositions.last)
    println(minFuel(crabPositions, fuelMap.getOrElse(_, 0)))

  def minFuel(crabPositions: IndexedSeq[Int], getFuel: Int => Int): Int =
    (crabPositions.head to crabPositions.last).map(fuelNeeded(crabPositions, _, getFuel)).min

  def fuelNeeded(crabPositions: Seq[Int], targetPosition: Int, getFuel: Int => Int) =
    crabPositions.map(getFuel.compose(Math.abs).compose(_ - targetPosition)).sum

  def incrementalFuelMap(maxPos: Int): Map[Int, Int] =
    (0 to maxPos).foldLeft(Map.empty)
      ((map, pos) => map.updated(pos, pos + map.getOrElse(pos - 1, 0)))

  def readCrabPositions(value: String): IndexedSeq[Int] =
    value.split(',').map(_.toInt).toIndexedSeq
}
