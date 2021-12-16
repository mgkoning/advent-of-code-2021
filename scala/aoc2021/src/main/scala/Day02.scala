import scala.io.Source

object Day02 extends PuzzleSolution:
  def title = "Dive!"

  def solve(input: Source): Unit =
    val directions = input.getLines.map(toDirection).toList
    val finalPos = directions.foldLeft(Position.zero)(Position.add)
    println("Part 1:")
    println(finalPos.depth * finalPos.hor)
    println("Part 2:")
    val finalPos2 = directions.foldLeft(PositionWithAim.zero)(PositionWithAim.add)
    println(finalPos2.pos.depth * finalPos2.pos.hor)

  val toDirection = (line: String) =>
    val parts = line.split(' ')
    (parts(0), parts(1).toInt) match
      case ("forward", f) => Forward(f)
      case ("down", d) => Down(d)
      case ("up", u) => Up(u)
      case _ => throw Exception("whoops")

  case class Position(hor: Int, depth: Int)
  object Position:
    val zero = Position(0, 0)
    def add(p: Position, d: Direction) = d match
      case Up(v) => p.copy(depth = p.depth - v)
      case Down(v) => p.copy(depth = p.depth + v)
      case Forward(f) => p.copy(hor = p.hor + f)

  case class PositionWithAim(pos: Position, aim: Int)
  object PositionWithAim:
    val zero = PositionWithAim(Position.zero, 0)
    def add(p: PositionWithAim, d: Direction) = d match
      case Up(v) => p.copy(aim = p.aim - v)
      case Down(v) => p.copy(aim = p.aim + v)
      case Forward(v) =>
        val pos = p.pos
        p.copy(pos = pos.copy(hor = pos.hor + v, depth = pos.depth + v * p.aim))

  sealed trait Direction
  case class Forward(value: Int) extends Direction
  case class Up(value: Int) extends Direction
  case class Down(value: Int) extends Direction

