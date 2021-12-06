import scala.io.Source
import scala.annotation.tailrec

object Day06 extends PuzzleSolution {
  def title = "Lanternfish"

  def solve(input: Source) =
    val ages = readAges(input.toSeq)
    println("Part 1:")
    println(lanternfishAfterDays(80, ages))
    println("Part 2:")
    println(lanternfishAfterDays(256, ages))

  def lanternfishAfterDays(days: Int, startAges: Seq[Int]): Long =
    val cache = getBirthTimeCache(days)
    startAges.map(age => cache.get(age - 8).get).sum

  // to prevent calculating the same values over and over, we work 'backwards' from the maximum age:
  // determine how many fish would be spawned if a lanternfish is born at a certain time. We will
  // eventually also visit its parent, and we'll know how many spawns the parent will have, plus
  // how many spawn their spawn will have. We accumulate it to -10 because the fish we start with
  // were born in the past.
  def getBirthTimeCache(maxAge: Int): Map[Int, Long] =
    @tailrec def buildCache(age: Int, accumulated: Map[Int, Long]): Map[Int, Long] =
      age match
        case i if i < -10 => accumulated
        case i =>
          val firstSpawn = i + 9
          val spawns = Seq.iterate(firstSpawn, maxAge / 6 + 1)(_ + 7).takeWhile(_ <= maxAge)
          val allSpawns = spawns.map(age => accumulated.get(age).get).sum
          buildCache(age - 1, accumulated.updated(i, 1 + allSpawns))

    val initialMap = (0 to 7).map(maxAge - _).map(_ -> 1L).toMap
    buildCache(maxAge - 8, initialMap)

  val toIgnore = Set(',', '\n', '\r')
  def readAges(chars: Seq[Char]): List[Int] =
    chars.filter(!toIgnore.contains(_)).map(_ - '0').toList
}
