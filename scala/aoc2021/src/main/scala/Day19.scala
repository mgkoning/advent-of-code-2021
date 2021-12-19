import scala.annotation.tailrec

object Day19 extends PuzzleSolution:
  def title = "Beacon Scanner"

  type ScannerReport = (String, List[Coord3])
  type Matrix = List[List[Int]]
  val identityMatrix3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))

  case class BeaconPair(beacons: List[Coord3], difference: Coord3)
  object BeaconPair:
    def make(beacons: List[Coord3]): BeaconPair =
      val sortedPair@List(one, other) = beacons.sorted
      BeaconPair(sortedPair, other.subtract(one))

  def matrixMult(left: Matrix, right: Matrix): Matrix =
    val transposed = right.transpose
    left.map(row => transposed.map(col => row.zip(col).map(_ * _).sum))

  val allOrientations = buildAllOrientations
  def buildAllOrientations: List[Matrix] =
    val cos90 = 0
    val sin90 = 1
    val rotateX90 = List(List(1, 0, 0), List(0, cos90, -sin90), List(0, sin90, cos90))
    val rotateY90 = List(List(cos90, 0, sin90), List(0, 1, 0), List(-sin90, 0, cos90))
    val rotateZ90 = List(List(cos90, -sin90, 0), List(sin90, cos90, 0), List(0, 0, 1))
    val xRotations = List.iterate(identityMatrix3, 4)(matrixMult(_, rotateX90))
    val yRotations = List.iterate(identityMatrix3, 4)(matrixMult(_, rotateY90))
    val zRotations = List.iterate(identityMatrix3, 4)(matrixMult(_, rotateZ90))
    xRotations.toSet.flatMap(x => yRotations.flatMap(y => zRotations.flatMap(z =>
      List(x, y, z).permutations.map(_.reduce(matrixMult))))).toList

  def transform(by: Matrix)(point: Coord3): Coord3 =
    val List(x, y, z) =
      matrixMult(by, List(point.x, point.y, point.z).map(List(_)))
        .flatMap(identity)
    Coord3(x, y, z)

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val scannerReports = readReports(input.getLines.toSeq)
    val (beacons, scanners) = findProbeLocations(scannerReports)
    println(beacons.size)
    println("Part 2:")
    println(
      scanners.combinations(2)
        .flatMap(comb => comb.zip(comb.drop(1)).map(Coord3.manhattanDistance))
        .toList.sorted.reverse.head
    )

  def findProbeLocations(reports: List[ScannerReport]): (List[Coord3], List[Coord3]) =
    val pairMatches = reports.combinations(2).flatMap(
      combination =>
        combination.zip(combination.drop(1)).flatMap(
          (report0, report1) =>
            val (mappings, _, rotation) = bestMatch(report0._2, report1._2)
            if mappings.size < 12 then None
              else
                val (otherBeacon, baseBeacon) = mappings.head
                val otherProbeLocation = baseBeacon.add(otherBeacon.additiveInverse)
                val result = (report0._1, report1._1, otherProbeLocation, rotation)
                Some(result)
          )
      ).flatMap(pair =>
        val (id1, id2, loc2, rotation) = pair
        val inverseRotation = rotation.transpose
        List(pair, (id2, id1, transform(inverseRotation)(loc2.additiveInverse), rotation.transpose))
      ).toList.groupBy(_._1)
    @tailrec def collectResults(known: Map[String, (Coord3, Matrix)], next: List[String]): Map[String, (Coord3, Matrix)] =
      next match
        case Nil => known
        case id1::rest =>
          val reachable = pairMatches.getOrElse(id1, List.empty).filter(p => !known.contains(p._2))
          val (location1, rotation1) = known.get(id1).get
          val newKnown = reachable.foldLeft(known)((k, r) =>
            val (_, id2, location2, rotation2) = r
            val actualRotation = matrixMult(rotation1, rotation2)
            val actualLocation = location1.add(transform(rotation1)(location2))
            k.updated(id2, (actualLocation, actualRotation))
          )
          collectResults(newKnown, (rest ++ reachable.map(_._2)).toSet.toList)
    val scanners = collectResults(Map(("0", (Coord3(0,0,0), identityMatrix3))), List("0"))
    val groupedReports = reports.toMap
    val beacons = scanners.flatMap((id, v) =>
      val (origin, rotation) = v
      groupedReports.getOrElse(id, List.empty).map(loc => origin.add(transform(rotation)(loc))))
      .toSet
    (beacons.toList, scanners.values.map(_._1).toList)

  def determineOverlap(known: List[BeaconPair], report: List[BeaconPair]): List[(Coord3, Coord3)] =
    report
      .flatMap(p => known.find(k => k._2 == p._2).map(k => p.beacons.zip(k.beacons)).toList)
      .flatMap(identity)
      .toSet
      .toList

  def bestMatch(base: List[Coord3], report: List[Coord3]): (List[(Coord3, Coord3)], List[Coord3], Matrix) =
    val baseRelativePositions = base.combinations(2).map(BeaconPair.make).toList
    val possibles = allOrientations.map(o => (o, report.map(transform(o))))
      .map((rotation, transformed) => 
        val pairs = transformed.combinations(2).map(BeaconPair.make).toList
        val overlap = determineOverlap(baseRelativePositions, pairs)
        (overlap, transformed, rotation)
      )
    possibles.sortBy(_._1.size).reverse.head

  def readReports(lines: Seq[String]): List[ScannerReport] =
    val ((id, beacons), reports) = lines.foldLeft((("", List.empty[Coord3]), List.empty[ScannerReport]))(
      (acc, line) =>
        val ((id, beacons), reports) = acc
        line match
          // start new report
          case s"--- scanner $newId ---" => ((newId, List.empty[Coord3]), reports)
          // add completed report to reports
          case "" => (("", List.empty), (id, beacons.reverse)::reports)
          // add beacon to current report
          case s"$x,$y,$z" => ((id, Coord3.ofStrings(x, y, z)::beacons), reports)
    )
    ((id, beacons.reverse)::reports).reverse
