import scala.annotation.tailrec

sealed trait Packet
case class Literal(version: Long, value: Long) extends Packet
case class Operator(version: Long, kind: OperatorKind, packets: List[Packet]) extends Packet

sealed trait OperatorKind
case object Sum extends OperatorKind
case object Product extends OperatorKind
case object Min extends OperatorKind
case object Max extends OperatorKind
case object GreaterThan extends OperatorKind
case object LessThan extends OperatorKind
case object EqualTo extends OperatorKind

object Packet:
  def parseFromHex(hexChars: Seq[Char]): Packet =
    val (packet, _) = parse(readBits(hexChars))
    packet

  private def parse(bits: List[Char]): (Packet, List[Char]) =
    val (versionBits, afterVersion) = consume(3, bits)
    val (packetTypeBits, remaining) = consume(3, afterVersion)
    val version = toDecimal(versionBits)
    val packetType = toDecimal(packetTypeBits)
    packetType match
      case 4 => parseLiteral(version, remaining)
      case _ => parseOperator(version, getOperatorKind(packetType), remaining)

  private def parseLiteral(version: Long, bits: List[Char]): (Packet, List[Char]) =
    @tailrec def readGroups(read: List[Char], bits: List[Char]): (List[Char], List[Char]) =
      val (nextGroup, rest) = consume(5, bits)
      val result = read ++ nextGroup.tail
      nextGroup.head match
        case '0' => (result, rest)
        case '1' => readGroups(result, rest)
    val (payload, remaining) = readGroups(List.empty, bits)
    (Literal(version, toDecimal(payload)), remaining)

  private def parseOperator(version: Long, kind: OperatorKind, bits: List[Char]): (Packet, List[Char]) =
    val lengthType::afterLengthType = bits
    lengthType match
      case '0' =>
        val (totalLengthBits, afterTotalLength) = consume(15, afterLengthType)
        val totalLength = toDecimal(totalLengthBits)
        val (subPackets, rest) = consume(totalLength.toInt, afterTotalLength)
        (Operator(version, kind, readAllPackets(subPackets)), rest)
      case '1' =>
        val (packetCountBits, afterPacketCount) = consume(11, afterLengthType)
        val packetCount = toDecimal(packetCountBits)
        val (subPackets, rest) = readPacketCount(afterPacketCount, packetCount)
        (Operator(version, kind, subPackets), rest)

  private def getOperatorKind(packetType: Long): OperatorKind =
    packetType match
      case 0 => Sum
      case 1 => Product
      case 2 => Min
      case 3 => Max
      case 5 => GreaterThan
      case 6 => LessThan
      case 7 => EqualTo

  @tailrec private def readPacketCount(
    bits: List[Char], count: Long, soFar: List[Packet] = Nil): (List[Packet], List[Char]
  ) =
    if count == 0 then (soFar.reverse, bits)
    else
      val (packet, rest) = parse(bits)
      readPacketCount(rest, count - 1, packet :: soFar)

  private def readAllPackets(bits: List[Char], soFar: List[Packet] = Nil): List[Packet] =
    bits match
      case Nil => soFar.reverse
      case _ =>
        val (packet, remaining) = parse(bits)
        readAllPackets(remaining, packet::soFar)

  private def consume(num: Int, bits: List[Char]): (List[Char], List[Char]) =
    (bits.take(num), bits.drop(num))

  private val bitValues = LazyList.iterate(1L)(2 * _).take(63).toList
  private def toDecimal(bits: List[Char]) =
    bits.reverse.zip(bitValues).map((b, v) => if b == '0' then 0 else v).sum

  private val hexMap = Map('0' -> "0000", '1' -> "0001", '2' -> "0010", '3' -> "0011", '4' -> "0100"
    , '5' -> "0101", '6' -> "0110", '7' -> "0111", '8' -> "1000", '9' -> "1001", 'A' -> "1010"
    , 'B' -> "1011", 'C' -> "1100", 'D' -> "1101", 'E' -> "1110", 'F' -> "1111")
    .mapValues(_.toList)
    .toMap
  private def readBits(input: Seq[Char]): List[Char] =
    input.flatMap(hexMap.getOrElse(_, List.empty)).toList
