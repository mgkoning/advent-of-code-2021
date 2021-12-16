import org.junit.Test
import org.junit.Assert.*
import Day16.*

class Day16Test:

  @Test def getValueTest =
    assertEquals(3, determineValue("C200B40A82"))
    assertEquals(54, determineValue("04005AC33890"))
    assertEquals(7, determineValue("880086C3E88112"))
    assertEquals(9, determineValue("CE00C43D881120"))
    assertEquals(1, determineValue("D8005AC2A8F0"))
    assertEquals(0, determineValue("F600BC2D8F"))
    assertEquals(0, determineValue("9C005AC2F8F0"))
    assertEquals(1, determineValue("9C0141080250320F1802104A08"))

  def determineValue(s: String) : Long =
    getValue(Packet.parseFromHex(s.toSeq))

