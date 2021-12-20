object Day20 extends PuzzleSolution:
  def title = "Trench Map"

  // We cannot assume all pixels outside will stay lit: in the actual puzzle input, pixels turn 'on'
  // when they and their neighbors are all off. So, for these infinite pixels outside of the known
  // pixels, we store just their expected state. We update this as we iterate the enhancements.
  case class Image(knownPixels: Map[Coord, Boolean], allLitOutside: Boolean)

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val (algorithm, pixels) = readInput(input.getLines.toSeq)
    val image = Image(pixels, false)
    val enhancements = LazyList.iterate(image)(enhance(algorithm))
    println(enhancements.drop(2).head._1.count(_._2))
    println("Part 2:")
    println(enhancements.drop(50).head._1.count(_._2))

  def enhance(algorithm: Set[Int])(image: Image): Image =
    def enhancePixel(c: Coord): (Coord, Boolean) =
      val pixels = Coord.surroundingSquare(c)
        .map(image.knownPixels.getOrElse(_, image.allLitOutside))
      val algoIndex = toDecimal(pixels)
      (c, algorithm.contains(algoIndex))
    val xs = image.knownPixels.keys.map(_.x)
    val ys = image.knownPixels.keys.map(_.y)
    val newPixels = for
      y <- ys.min - 1 to ys.max + 1
      x <- xs.min - 1 to xs.max + 1
    yield enhancePixel(Coord(x, y))
    Image(newPixels.toMap, algorithm.contains(if image.allLitOutside then 512 else 0))

  val bitValues = LazyList.iterate(1)(2 * _).take(9).toList
  def toDecimal(bits: Seq[Boolean]) =
    bits.reverse.zip(bitValues).map((b, v) => if b then v else 0).sum

  def readInput(lines: Seq[String]): (Set[Int], Map[Coord, Boolean]) =
    val (header, imageLines) = lines.splitAt(2)
    val algorithm = header.head.zipWithIndex.filter(_._1 == '#').map(_._2).toSet
    val image = Input.readGrid(imageLines, _.toSeq, identity).mapValues(_ == '#').toMap
    (algorithm, image)
