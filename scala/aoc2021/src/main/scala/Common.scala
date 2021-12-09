case class Coord(x: Int, y: Int) extends Ordered[Coord]:
  def compare(that: Coord) = Coord.coordOrdering.compare(this, that)
object Coord:
  implicit val coordOrdering: Ordering[Coord] =
    Ordering.by[Coord, Int](_.x).orElse(Ordering.by[Coord, Int](_.y))

  def ofTuple(tuple: (Int, Int)): Coord = Coord(tuple._1, tuple._2)

  def ofStrings(x: String, y: String) = Coord(x.toInt, y.toInt)

  def adjacent(c: Coord): Seq[Coord] =
    Seq(Coord(c.x - 1, c.y), Coord(c.x + 1, c.y), Coord(c.x, c.y - 1), Coord(c.x, c.y + 1))

case class Line(one: Coord, other: Coord):
  def isHorizontalOrVertical = one.x == other.x || one.y == other.y

  def isDiagonal = math.abs(other.x - one.x) == math.abs(other.y - one.y)

  def xs = one.x to other.x by (if one.x < other.x then 1 else -1)

  def ys = one.y to other.y by (if one.y < other.y then 1 else -1)
