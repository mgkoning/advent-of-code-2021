case class Coord(x: Int, y: Int) extends Ordered[Coord]:
  def compare(that: Coord) = Coord.coordOrdering.compare(this, that)
object Coord:
  implicit val coordOrdering: Ordering[Coord] =
    Ordering.by[Coord, Int](_.y).orElse(Ordering.by(_.x))

  def ofTuple(tuple: (Int, Int)): Coord = Coord(tuple._1, tuple._2)

  def ofStrings(x: String, y: String) = Coord(x.toInt, y.toInt)

  def adjacent4(c: Coord): Seq[Coord] =
    Seq(Coord(c.x - 1, c.y), Coord(c.x + 1, c.y), Coord(c.x, c.y - 1), Coord(c.x, c.y + 1))

  def adjacent8(c: Coord): Seq[Coord] =
    for dx <- (-1 to 1); dy <- (-1 to 1) if dx != 0 || dy != 0 yield Coord(c.x + dx, c.y + dy)

  def surroundingSquare(c: Coord): Seq[Coord] =
    for dy <- (-1 to 1); dx <- (-1 to 1) yield Coord(c.x + dx, c.y + dy)

case class Line(one: Coord, other: Coord):
  def isHorizontalOrVertical = one.x == other.x || one.y == other.y

  def isDiagonal = math.abs(other.x - one.x) == math.abs(other.y - one.y)

  def xs = one.x to other.x by (if one.x < other.x then 1 else -1)

  def ys = one.y to other.y by (if one.y < other.y then 1 else -1)

case class Coord3(x: Int, y: Int, z: Int) extends Ordered[Coord3]:
  def compare(that: Coord3) = Coord3.coord3Ordering.compare(this, that)
  def subtract(that: Coord3) =
    Coord3(x - that.x, y - that.y, z - that.z)
  def add(that: Coord3) =
    Coord3(x + that.x, y + that.y, z + that.z)
  def additiveInverse =
    Coord3(-x, -y, -z)

object Coord3:
  implicit val coord3Ordering: Ordering[Coord3] =
    Ordering.by[Coord3, Int](_.x)
      .orElse(Ordering.by(_.y))
      .orElse(Ordering.by(_.z))

  def ofStrings(x: String, y: String, z: String) = Coord3(x.toInt, y.toInt, z.toInt)

  def manhattanDistance(one: Coord3, other: Coord3): Int =
    math.abs(one.x - other.x) + math.abs(one.y - other.y) + math.abs(one.z - other.z)