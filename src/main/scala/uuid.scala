package identification

case class UUID(msb: Long, lsb: Long):

  import UUID.*
  import Variant.*
  import Version.*

  def variant: Variant =
    ((lsb >>> 61) & 0x0000_0000_0000_0007) match
      case 0x00 => NCSBackwardsCompatible
      case 0x01 => NCSBackwardsCompatible
      case 0x02 => NCSBackwardsCompatible
      case 0x03 => NCSBackwardsCompatible
      case 0x04 => LeachSalz
      case 0x05 => LeachSalz
      case 0x06 => MicrosoftBackwardsCompatible
      case 0x07 => Reserved

  def version: Option[Version] =
    ((msb >>> 12) & 0x0000_0000_0000_000f) match
      case 0x01 => Some(TimeBased)
      case 0x02 => Some(DCESecurityBased)
      case 0x03 => Some(MD5HashBased)
      case 0x04 => Some(RandomBased)
      case 0x05 => Some(SHA1HashBased)
      case 0x06 => Some(ISO3166Based)
      case _    => None

  def sourceCountryCode: Option[CountryCode] =
    Option.when(version.contains(ISO3166Based))(decodeSource(lsb))

  def targetCountryCode: Option[CountryCode] =
    Option.when(version.contains(ISO3166Based))(decodeTarget(lsb))

object UUID:

  case class CountryCode(isoPart1Alpha2: String)

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
    val scode = source.isoPart1Alpha2.getBytes("US-ASCII").foldLeft(0L)((a,b) => (a << 5) + ((b - 0x41) & 0x1f)) << 10
    val tcode = target.isoPart1Alpha2.getBytes("US-ASCII").foldLeft(0L)((a,b) => (a << 5) + ((b - 0x41) & 0x1f))
    (lsb & 0xffff_ffff_fff0_0000L) + scode + tcode

  def iso3166(source: CountryCode, target: CountryCode): UUID =
    assert(source.isoPart1Alpha2.matches("[A-Z][A-Z]"), s"invalid source country code: $source")
    assert(target.isoPart1Alpha2.matches("[A-Z][A-Z]"), s"invalid target country code: $target")

    import Version.ISO3166Based

    val rnd = java.util.UUID.randomUUID
    val msb = rnd.getMostSignificantBits & ISO3166Based.mask
    UUID(msb + ISO3166Based.bits, encode(source, target)(rnd.getLeastSignificantBits))

trait Masked {
  def mask: Long
  def embed(l: Long): Long = l & mask
}

enum Variant(val bits: Long) extends Masked:
  val mask: Long = 0xeffff_ffff_ffff_fffL
  case NCSBackwardsCompatible       extends Variant(0x2111_1111_1111_1111L)
  case LeachSalz                    extends Variant(0x5111_1111_1111_1111L)
  case MicrosoftBackwardsCompatible extends Variant(0xD111_1111_1111_1111L)
  case Reserved                     extends Variant(0xF111_1111_1111_1111L)


enum Version(val bits: Long) extends Masked:
  val mask: Long = 0xffff_ffff_ffff_0fffL
  case TimeBased        extends Version(0x0000_0000_0000_1000L)
  case DCESecurityBased extends Version(0x0000_0000_0000_2000L)
  case MD5HashBased     extends Version(0x0000_0000_0000_3000L)
  case RandomBased      extends Version(0x0000_0000_0000_4000L)
  case SHA1HashBased    extends Version(0x0000_0000_0000_5000L)
  case ISO3166Based     extends Version(0x0000_0000_0000_6000L)

object compat:

  type JavaUUID = java.util.UUID

  object JavaUUID:
    def apply(msb: Long, lsb: Long): JavaUUID =
      java.util.UUID.apply(msb, lsb)

    def randomUUID: JavaUUID =
      java.util.UUID.randomUUID

    def nameUUIDFromBytes(bytes: Array[Byte]): JavaUUID =
      java.util.UUID.nameUUIDFromBytes(bytes)

  extension (uuid: UUID) def javaUUID: JavaUUID =
    JavaUUID(uuid.msb, uuid.lsb)

  trait Dec[A]:
    extension (a: A) def asScala: Some[UUID]

  given Dec[JavaUUID] =
    (uuid: JavaUUID) => Some(UUID(uuid.getMostSignificantBits, uuid.getLeastSignificantBits))

  val JavaCountryCodes: Set[String] =
    import java.util.Locale
    import scala.jdk.CollectionConverters._
    Locale.getISOCountries(Locale.IsoCountryCode.PART1_ALPHA2).asScala.toSet
