package id

case class UUID(msb: Long, lsb: Long):

  import UUID.*
  import Variant.*
  import Version.*

  import compat.*

  lazy val variant: Variant =
    (lsb >>> 61) & 0x0000_0000_0000_0007 match
      case 0x00 => NCSBackwardsCompatible
      case 0x01 => NCSBackwardsCompatible
      case 0x02 => NCSBackwardsCompatible
      case 0x03 => NCSBackwardsCompatible
      case 0x04 => LeachSalz
      case 0x05 => LeachSalz
      case 0x06 => MicrosoftBackwardsCompatible
      case 0x07 => Reserved

  lazy val version: Option[Version] =
    (msb >>> 12) & 0x0000_0000_0000_000f match
      case 0x01 => Some(TimeBased)
      case 0x02 => Some(DCESecurityBased)
      case 0x03 => Some(MD5HashBased)
      case 0x04 => Some(RandomBased)
      case 0x05 => Some(SHA1HashBased)
      case 0x06 => Some(Version6)
      case 0x07 => Some(Version7)
      case 0x08 => Some(ISO3166Based)
      case _    => None

  lazy val sourceCountryCode: Option[CountryCode] =
    Option.when(version.contains(ISO3166Based))(decodeSource(lsb))

  lazy val targetCountryCode: Option[CountryCode] =
    Option.when(version.contains(ISO3166Based))(decodeTarget(lsb))

object UUID:

  import compat.*
  import JavaUUID.*

  /** wrapper for a two character upper case country code */
  case class CountryCode(underlying: String):
    assert(underlying.matches("[A-Z][A-Z]"), s"not a two character upper case string: $underlying")

    def asBytes: Array[Byte] =
      underlying.getBytes("US-ASCII")

    def isoPart1Alpha2: Option[String] =
      Option.when(JavaCountryCodes.contains(underlying))(underlying)

  object CountryCode:
    def apply(msb: Byte, lsb: Byte): CountryCode =
      CountryCode(String(Array(msb, lsb), "US-ASCII"))

  def decodeTarget(lsb: Long): CountryCode =
    val node5 = (lsb & 0x0000_0000_0000_001f) + 0x41
    val node4 = ((lsb >>>  5) & 0x0000_0000_0000_001f) + 0x41
    CountryCode(node4.toByte, node5.toByte)

  def decodeSource(lsb: Long): CountryCode =
    val node3 = ((lsb >>> 10) & 0x0000_0000_0000_001f) + 0x41
    val node2 = ((lsb >>> 15) & 0x0000_0000_0000_001f) + 0x41
    CountryCode(node2.toByte, node3.toByte)

  private def encode(source: CountryCode, target: CountryCode)(lsb: Long): Long =
    def extractor(accumulator: Long, byte: Byte): Long = (accumulator << 5) + ((byte - 0x41) & 0x1f)
    val scode = source.asBytes.foldLeft(0L)(extractor)
    val tcode = target.asBytes.foldLeft(0L)(extractor)
    (lsb & 0xffff_ffff_fff0_0000L) + (scode << 10) + tcode

  def iso3166(source: CountryCode, target: CountryCode, from: JavaUUID = JavaUUID.randomUUID): UUID =
    assert(from.asScala.version.contains(Version.RandomBased), s"invalid java uuid version: ${from.asScala.version}")

    import Version.ISO3166Based

    val msb = from.getMostSignificantBits & ISO3166Based.mask
    UUID(msb + ISO3166Based.bits, encode(source, target)(from.getLeastSignificantBits))

enum Variant(val bits: Long):
  val mask: Long = 0xeffff_ffff_ffff_fffL
  case NCSBackwardsCompatible       extends Variant(0x2111_1111_1111_1111L)
  case LeachSalz                    extends Variant(0x5111_1111_1111_1111L)
  case MicrosoftBackwardsCompatible extends Variant(0xD111_1111_1111_1111L)
  case Reserved                     extends Variant(0xF111_1111_1111_1111L)


enum Version(val bits: Long):
  val mask: Long = 0xffff_ffff_ffff_0fffL
  case TimeBased        extends Version(0x0000_0000_0000_1000L)
  case DCESecurityBased extends Version(0x0000_0000_0000_2000L)
  case MD5HashBased     extends Version(0x0000_0000_0000_3000L)
  case RandomBased      extends Version(0x0000_0000_0000_4000L)
  case SHA1HashBased    extends Version(0x0000_0000_0000_5000L)
  case Version6         extends Version(0x0000_0000_0000_6000L)
  case Version7         extends Version(0x0000_0000_0000_7000L)
  case ISO3166Based     extends Version(0x0000_0000_0000_8000L)

object compat:

  type JavaUUID = java.util.UUID

  object JavaUUID:
    def apply(msb: Long, lsb: Long): JavaUUID =
      java.util.UUID.apply(msb, lsb)

    def randomUUID: JavaUUID =
      java.util.UUID.randomUUID

    def nameUUIDFromBytes(bytes: Array[Byte]): JavaUUID =
      java.util.UUID.nameUUIDFromBytes(bytes)

    extension (uuid: UUID) def asJavaUUID: JavaUUID =
      JavaUUID(uuid.msb, uuid.lsb)

    extension (javaUUID: JavaUUID) def asScala: UUID =
      UUID(javaUUID.getMostSignificantBits, javaUUID.getLeastSignificantBits)

  val JavaCountryCodes: Set[String] =
    import java.util.Locale.*
    import scala.jdk.CollectionConverters.*
    getISOCountries(IsoCountryCode.PART1_ALPHA2).asScala.toSet
