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
    steps.foldLeft(List.empty[Cuboid])((onCubes, step) =>
      val RebootStep(state, area) = step
      val piecesOutside = onCubes.flatMap(shatter(area))
      if state == On then area::piecesOutside else piecesOutside)

  def shatter(previous: Cuboid)(current: Cuboid): Seq[Cuboid] =
    if whollyWithin(previous)(current) then Seq.empty
    else
      overlaps(previous)(current) match
        case None => Seq(current)
        case Some(overlap) =>
          def pairs(selector: Coord3 => Int) =
            Seq((selector(current.from), selector(overlap.from) - 1),
              (selector(overlap.from), selector(overlap.to)),
              (selector(overlap.to) + 1, selector(current.to)))
          val subcubes = for (x0, x1) <- pairs(_.x); (y0, y1) <- pairs(_.y); (z0, z1) <- pairs(_.z)
                         yield Cuboid(Coord3(x0, y0, z0), Coord3(x1, y1, z1))
          subcubes
            .filter(0 < Cuboid.size(_))
            .filter(!whollyWithin(overlap)(_))

  def overlaps(one: Cuboid)(other: Cuboid): Option[Cuboid] =
    val overlap = Cuboid(
      Coord3(math.max(other.from.x, one.from.x), math.max(other.from.y, one.from.y), math.max(other.from.z, one.from.z)),
      Coord3(math.min(other.to.x, one.to.x), math.min(other.to.y, one.to.y), math.min(other.to.z, one.to.z))
    )
    if 0 < Cuboid.size(overlap) then Some(overlap) else None

  def allAxesLessThanOrEqual(one: Coord3, other: Coord3) =
    one.x <= other.x && one.y <= other.y && one.z <= other.z
  def whollyWithin(area: Cuboid)(candidate: Cuboid): Boolean =
    allAxesLessThanOrEqual(area.from, candidate.from) &&
      allAxesLessThanOrEqual(candidate.to, area.to)

  def readRebootSteps(lines: Iterator[String]): List[RebootStep] =
    lines
      .flatMap(l =>
        l match
          case s"$state x=$x0..$x1,y=$y0..$y1,z=$z0..$z1" =>
            val area = Cuboid(Coord3.ofStrings(x0, y0, z0), Coord3.ofStrings(x1, y1, z1))
            Some(RebootStep(if state == "on" then On else Off, area))
          case _ => None)
      .toList
