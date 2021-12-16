import scala.annotation.tailrec
object Day11  extends PuzzleSolution:
  def title = "Dumbo Octopus"

  type Octopodes = Map[Coord, Int]

  def solve(input: scala.io.Source) =
    val octopodes = readOctopodes(input.getLines.toSeq)
    println("Part 1:")
    println(Iterator.iterate(octopodes)(step).take(101).map(countFlashers).sum)
    println("Part 2:")
    println(Iterator.iterate(octopodes)(step).takeWhile(countFlashers(_) != octopodes.size).size)

  def countFlashers(octopodes: Octopodes): Int = octopodes.count(_._2 == 0)

  def step(before: Octopodes): Octopodes =
    @tailrec def chainReaction(state: Octopodes): Octopodes =
      val flashers = state.filter(9 < _._2).keys.toSeq
      if flashers.isEmpty
        then state
        else
          val afterInitialFlash = flashers.foldLeft(state)((state, c) => state.updated(c, 0))
          val neighbors = flashers.flatMap(Coord.adjacent8)
          val newState = neighbors
            .foldLeft(afterInitialFlash)((state, neighbor) =>
              val neighborValue = state.getOrElse(neighbor, 0)
              if neighborValue == 0
                then state
                else state.updated(neighbor, neighborValue + 1))
          chainReaction(newState)
    chainReaction(before.mapValues(_ + 1).toMap)

  def readOctopodes(lines: Seq[String]): Octopodes =
    Input.readGrid(lines, _.toSeq, _ - '0')

