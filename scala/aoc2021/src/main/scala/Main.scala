import java.time.LocalDate

@main def aoc2021(args: String*): Unit = 
  val day = args.headOption.map(_.toInt).getOrElse(LocalDate.now.getDayOfMonth)
  runners.get(day) match
    case Some(runner) => 
      println("Running day " + day)
      runner.solve(Input.asString(day))
    case None => println("Can't run " + day)


val runners: Map[Int, PuzzleSolution] =
  (1 to 31).zip(Seq(Day01, Day02)).toMap

