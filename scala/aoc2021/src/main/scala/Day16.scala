import scala.annotation.tailrec
object Day16 extends PuzzleSolution {
  def title = "Packet Decoder"

  sealed trait Packet
  case class Literal(version: Long, payload: List[Char]) extends Packet
  case class Operator(version: Long, packetType: Long, packets: List[Packet]) extends Packet

  def solve(input: scala.io.Source) =
    println("Part 1:")
    val (packet, _) = parsePacket(readBits(input.toSeq))
    println(getVersions(packet).sum)
    println("Part 2:")
    println(getValue(packet))

  def getVersions(packet: Packet): List[Long] =
    packet match
      case Literal(version, _) => List(version)
      case Operator(version, _, subPackets) => version :: (subPackets.flatMap(getVersions))

  def getValue(packet: Packet): Long =
    packet match
      case Literal(_, payload) => toDecimal(payload)
      case Operator(_, packetType, packets) =>
        val values = packets.map(getValue)
        packetType match
          case 0 => values.sum
          case 1 => values.product
          case 2 => values.min
          case 3 => values.max
          case 5 =>
            val List(l, r) = values
            if l > r then 1L else 0L
          case 6 =>
            val List(l, r) = values
            if l < r then 1L else 0L
          case 7 =>
            val List(l, r) = values
            if l == r then 1L else 0L

  def parsePacket(bits: List[Char]): (Packet, List[Char]) =
    val (versionBits, rest) = consume(3, bits)
    val (pTypeBits, rest2) = consume(3, rest)
    val version = toDecimal(versionBits)
    val pType = toDecimal(pTypeBits)
    pType match
      case 4 => parseLiteral(version, rest2)
      case _ => parseOperator(version, pType, rest2)

  def parseLiteral(version: Long, bits: List[Char]): (Packet, List[Char]) =
    @tailrec def readGroups(read: List[Char], bits: List[Char]): (List[Char], List[Char]) =
      val (nextGroup, rest) = consume(5, bits)
      val result = read ++ nextGroup.tail
      nextGroup.head match
        case '0' => (result, rest)
        case '1' => readGroups(result, rest)
    val (payload, remaining) = readGroups(List.empty, bits)
    (Literal(version, payload), remaining)

  def parseOperator(version: Long, pType: Long, bits: List[Char]): (Packet, List[Char]) =
    val lengthType::rest = bits
    lengthType match
      case '0' =>
        val (totalLengthBits, rest2) = consume(15, rest)
        val totalLength = toDecimal(totalLengthBits)
        val (subPackets, rest3) = consume(totalLength.toInt, rest2)
        (Operator(version, pType, readAllPackets(subPackets)), rest3)
      case '1' =>
        val (packetCountBits, rest2) = consume(11, rest)
        val packetCount = toDecimal(packetCountBits)
        val (subPackets, rest3) = readPacketCount(rest2, packetCount)
        (Operator(version, pType, subPackets), rest3)

  @tailrec def readPacketCount(
    bits: List[Char], count: Long, soFar: List[Packet] = Nil): (List[Packet], List[Char]
  ) =
    if count == 0 then (soFar.reverse, bits)
    else
      val (packet, rest) = parsePacket(bits)
      readPacketCount(rest, count - 1, packet :: soFar)

  def readAllPackets(bits: List[Char], soFar: List[Packet] = Nil): List[Packet] =
    bits match
      case Nil => soFar.reverse
      case _ =>
        val (packet, remaining) = parsePacket(bits)
        readAllPackets(remaining, packet::soFar)

  def consume(num: Int, bits: List[Char]): (List[Char], List[Char]) =
    (bits.take(num), bits.drop(num))

  val bitValues = LazyList.iterate(1L)(2 * _).take(63).toList
  def toDecimal(bits: List[Char]) =
    bits.reverse.zip(bitValues).map((b, v) => if b == '0' then 0 else v).sum

  val hexMap = Map('0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011", '4' -> "0100"
    , '5' -> "0101", '6' -> "0110", '7' -> "0111", '8' -> "1000", '9' -> "1001", 'A' -> "1010"
    , 'B' -> "1011", 'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")
    .mapValues(_.toList)
    .toMap
  def readBits(input: Seq[Char]): List[Char] =
    input.flatMap(hexMap.getOrElse(_, List.empty)).toList
}
