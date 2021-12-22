object Day22 extends PuzzleSolution:
  def title = "Reactor Reboot"

  sealed trait State
  case object On extends State
  case object Off extends State
  case class RebootStep(state: State, area: Cuboid)
  case class Cuboid(from: Coord3, to: Coord3)

  object Cuboid:
    def cubes(cuboid: Cuboid): Seq[Coord3] =
      for x <- cuboid.from.x to cuboid.to.x
          y <- cuboid.from.y to cuboid.to.y
          z <- cuboid.from.z to cuboid.to.z
      yield Coord3(x, y, z)
    def size(cuboid: Cuboid): Long =
      Math.max(0, (cuboid.to.x - cuboid.from.x + 1)).toLong *
        Math.max(0, (cuboid.to.y - cuboid.from.y + 1)) *
        Math.max(0, (cuboid.to.z - cuboid.from.z + 1))

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val rebootSteps = readRebootSteps(input.getLines)
    println(runInitialization(rebootSteps))
    println("Part 2:")
    println(runSteps(rebootSteps).map(Cuboid.size).sum)

  val initializationArea = Cuboid(Coord3(-50, -50, -50), Coord3(50, 50, 50))
  def runInitialization(steps: List[RebootStep]): Long =
    val initSteps = steps.filter(s => whollyWithin(initializationArea)(s.area))
    //runStepsNaive(initSteps).size
    runSteps(initSteps).map(Cuboid.size).sum

  def runStepsNaive(steps: List[RebootStep]): Set[Coord3] =
    steps.foldLeft(Set.empty[Coord3])((cubes, step) =>
      val stepCubes = Cuboid.cubes(step.area).toSet
      step.state match
        case On => cubes.union(stepCubes)
        case Off => cubes.diff(stepCubes))

  def runSteps(steps: List[RebootStep]): List[Cuboid] =
    // for every step: determine which parts of the currently 'On' cuboids do not overlap
    // with the step's area. Retain those parts for the next iteration, and add the current
    // area only if the state is `On`.
    steps.foldLeft(List.empty[Cuboid])((onCubes, step) =>
      val RebootStep(state, area) = step
      val piecesOutside = onCubes.flatMap(shatter(area))
      if state == On then area::piecesOutside else piecesOutside)

  // Breaks `current` into pieces that do not overlap with `previous`.
  def shatter(previous: Cuboid)(current: Cuboid): Seq[Cuboid] =
    if whollyWithin(previous)(current) then Seq.empty
    else
      overlaps(previous)(current) match
        case None => Seq(current)
        case Some(overlap) =>
          // Determine the pairs of coordinates by axis to create the pieces outside
          // of the overlap. Note that the overlap cuboid has inclusive boundaries.
          def pairs(selector: Coord3 => Int) = Seq(
              (selector(current.from), selector(overlap.from) - 1),
              (selector(overlap.from), selector(overlap.to)),
              (selector(overlap.to) + 1, selector(current.to)))
          val subcubes = for (x0, x1) <- pairs(_.x);
                             (y0, y1) <- pairs(_.y);
                             (z0, z1) <- pairs(_.z)
                         yield Cuboid(Coord3(x0, y0, z0), Coord3(x1, y1, z1))
          subcubes // remove cubes that are zero-sized as well as the overlap.
            .filter(0 < Cuboid.size(_))
            .filter(!whollyWithin(overlap)(_))

  // Determines the overlap between `one` and `other`.
  def overlaps(one: Cuboid)(other: Cuboid): Option[Cuboid] =
    val Cuboid(from0, to0) = one
    val Cuboid(from1, to1) = other
    val overlap = Cuboid(
      Coord3(math.max(from0.x, from1.x), math.max(from0.y, from1.y), math.max(from0.z, from1.z)),
      Coord3(math.min(to0.x, to1.x), math.min(to0.y, to1.y), math.min(to0.z, to1.z))
    )
    if 0 < Cuboid.size(overlap) then Some(overlap) else None

  def allAxesLessThanOrEqual(one: Coord3, other: Coord3) =
    one.x <= other.x && one.y <= other.y && one.z <= other.z
  def whollyWithin(area: Cuboid)(candidate: Cuboid): Boolean =
    allAxesLessThanOrEqual(area.from, candidate.from) &&
      allAxesLessThanOrEqual(candidate.to, area.to)

  def readRebootSteps(lines: Iterator[String]): List[RebootStep] =
    lines
      .flatMap(line => line match
        case s"$state x=$x0..$x1,y=$y0..$y1,z=$z0..$z1" =>
          val area = Cuboid(Coord3.ofStrings(x0, y0, z0), Coord3.ofStrings(x1, y1, z1))
          Some(RebootStep(if state == "on" then On else Off, area))
        case _ => None)
      .toList
