object Day14 extends PuzzleSolution {
  def title = "Extended Polymerization"

  type Rules = Map[(Char, Char), Char]
  type PolymerPair = (Char, Char)
  type PolymerPairCount = (PolymerPair, Long)

  def solve(input: scala.io.Source) =
    val (polymer, rules) = parseInput(input.getLines.toSeq)
    println("Part 1:")
    val afterStep10 = resultAfter(rules, 10)(polymer)
    println(afterStep10.values.max - afterStep10.values.min)
    println("Part 2:")
    val lastChar = polymer.last
    val initialPolymerPairCounts = polymer.zip(polymer.drop(1))
      .groupBy(identity).mapValues(_.size.toLong).toList
    val after40 = LazyList.iterate(initialPolymerPairCounts)(stepPairwise(rules)).drop(40).head
    val charCounts = after40.groupMap(_._1._1)(_._2)
      .map((c, v) => if c == lastChar then v.sum + 1 else v.sum)
    println(charCounts.max - charCounts.min)

  def resultAfter(rules: Rules, steps: Int)(polymer: List[Char]): Map[Char, Int] =
    val afterSteps = LazyList.iterate(polymer)(step(rules)).drop(steps).head
    afterSteps.groupBy(identity).mapValues(_.size).toMap

  def stepPairwise(rules: Rules)(pairs: List[PolymerPairCount]): List[PolymerPairCount] =
    def applyRule(pair: PolymerPair): List[PolymerPair] =
      val (left, right) = pair
      rules.get(pair).map(b => List((left, b), (b, right))).toList.flatMap(identity)

    val newPairs = pairs.flatMap((pair, count) => applyRule(pair).map(newPair => (newPair, count)))
    newPairs.foldLeft(Map.empty[PolymerPair, Long])(
      (m, next) => 
        val (pair, count) = next
        m.updatedWith(pair)(v => v.map(_ + count).orElse(Some(count)))).toList

  def step(rules: Rules)(polymer: List[Char]): List[Char] =
    def polymerize(remaining: List[Char], soFar: List[Char]): List[Char] =
      remaining match
        case Nil => soFar.reverse
        case x :: Nil => polymerize(Nil, x :: soFar)
        case a :: (rest @ b :: _) =>
          rules.get((a, b)) match
            case None => polymerize(rest, a :: soFar)
            case Some(c) => polymerize(rest, c :: a :: soFar)
    polymerize(polymer.toList, List.empty)

  def parseInput(lines: Seq[String]): (List[Char], Rules) =
    val polymer = lines.head
    val rules = lines.drop(2)
      .map(line => ((line.charAt(0), line.charAt(1)), line.charAt(6)))
      .toMap
    (polymer.toList, rules)
}
