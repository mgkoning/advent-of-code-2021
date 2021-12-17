object Day17 extends PuzzleSolution:
  def title = "Trick Shot"

  case class Rectangle(topLeft: Coord, bottomRight: Coord)
  object Rectangle:
    def contains(rect: Rectangle, point: Coord) =
      point.x <= rect.bottomRight.x && rect.topLeft.x <= point.x &&
      rect.bottomRight.y <= point.y && point.y <= rect.topLeft.y

  case class Velocity(dx: Int, dy: Int)
  case class Trajectory(startVelocity: Velocity, path: List[Coord])

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val targetArea = readTargetArea(input.getLines.toSeq)
    val trajectories = findTrajectoriesToHit(targetArea)
    println(findTrickShot(trajectories))
    println("Part 2:")
    println(trajectories.length)

  def findTrickShot(trajectories: List[Trajectory]): Int =
    trajectories.map(_.path.map(_.y).max).sorted.reverse.head

  def findTrajectoriesToHit(targetArea: Rectangle): List[Trajectory] =
    val velocities =
      for dx <- 1 to targetArea.bottomRight.x
          dy <- targetArea.bottomRight.y to (-1 * targetArea.bottomRight.y)
      yield Velocity(dx, dy)
    velocities.map(simulate(targetArea)).flatMap(_.toSeq).toList

  def simulate(target: Rectangle)(v: Velocity): Option[Trajectory] =
    def drag(v: Velocity) = Velocity(v.dx.sign * (v.dx.abs - 1), v.dy - 1)
    val steps = LazyList.iterate((Coord(0,0), v))((c, v) => (Coord(c.x + v.dx, c.y + v.dy), drag(v)))
    val trajectory = steps.map(_._1)
      .takeWhile(c => c.x <= target.bottomRight.x && target.bottomRight.y <= c.y)
      .toList
    if trajectory.exists(Rectangle.contains(target, _))
      then Some(Trajectory(v, trajectory))
      else None

  def readTargetArea(lines: Seq[String]): Rectangle =
    lines.head match
      case s"target area: x=$x0..$x1, y=$y0..$y1" =>
        val List(xLeft, xRight) = List(x0.toInt, x1.toInt).sorted
        val List(yBottom, yTop) = List(y0.toInt, y1.toInt).sorted
        Rectangle(Coord(xLeft, yTop),Coord(xRight, yBottom))

