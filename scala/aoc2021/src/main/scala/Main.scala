import java.time.LocalDate

@main def aoc2021(args: String*): Unit =
  val firstArg = args.headOption
  firstArg match
    case None => runDay(LocalDate.now.getDayOfMonth)
    case Some("all") => runners.keySet.toSeq.sorted.foreach(runDay)
    case Some(day) => runDay(day.toInt)

def runDay(day: Int) = runners.get(day) match
  case Some(runner) => 
    println(s"\n--- Day $day: ${runner.title} ---\n")
    runner.solve(Input.getFile(day))
  case None => println("Can't run " + day)

val runners: Map[Int, PuzzleSolution] =
  val runners = List(
    Day01, Day02, Day03, Day04, Day05, Day06, Day07, Day08, Day09, Day10, Day11, Day12, Day13,
    Day14, Day15, Day16, Day17, Day18, Day19, Day20, Day21, Day22, Day23, Day24, Day25
  )
  LazyList.iterate(1)(_ + 1).zip(runners).toMap

