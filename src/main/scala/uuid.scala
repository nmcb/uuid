package identification

import uuid.Masked

object uuid:

  case class UUID(msb: Long)(lsb: Long):

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

    def sourceCountryCode: Option[String] =
      Option.when(version.contains(ISO3166Based))(UUID.source(lsb))

    def targetCountryCode: Option[String] =
      Option.when(version.contains(ISO3166Based))(UUID.target(lsb))

  object UUID:

    def target(lsb: Long): String =
      val node5 = (lsb & 0x0000_0000_0000_001f) + 0x41
      val node4 = ((lsb >>>  5) & 0x0000_0000_0000_001f) + 0x41
      String(Array(node4.toByte, node5.toByte), "US-ASCII")

    def source(lsb: Long): String =
      val node3 = ((lsb >>> 10) & 0x0000_0000_0000_001f) + 0x41
      val node2 = ((lsb >>> 15) & 0x0000_0000_0000_001f) + 0x41
      String(Array(node2.toByte, node3.toByte), "US-ASCII")

    private def encode(source: String, target: String)(lsb: Long): Long =
      val scode = source.getBytes("US-ASCII").foldLeft(0L)((a,b) => (a << 5) + ((b - 0x41) & 0x1f)) << 10
      val tcode = target.getBytes("US-ASCII").foldLeft(0L)((a,b) => (a << 5) + ((b - 0x41) & 0x1f))
      (lsb & 0xffff_ffff_fff0_0000L) + scode + tcode

    def iso3166(source: String, target: String): UUID =
      assert(source.matches("[A-Z][A-Z]"), s"invalid source country code: $source")
      assert(target.matches("[A-Z][A-Z]"), s"invalid target country code: $target")

      val rnd = java.util.UUID.randomUUID
      val msb = rnd.getMostSignificantBits  & 0xFFFFFFFFFFFF0FFFL
      UUID(msb + Version.ISO3166Based.bits)(encode(source, target)(rnd.getLeastSignificantBits))

  trait Masked {
    def mask: Long
    def mask(l: Long): Long = l & mask
  }

  enum Variant(val bits: Long) extends Masked:
    val mask: Long = 0xEFFFFFFFFFFFFFFFL
    case NCSBackwardsCompatible       extends Variant(0x2000000000000000L)
    case LeachSalz                    extends Variant(0x5000000000000000L)
    case MicrosoftBackwardsCompatible extends Variant(0xD000000000000000L)
    case Reserved                     extends Variant(0xF000000000000000L)


  enum Version(val bits: Long) extends Masked:
    val mask: Long = 0xFFFFFFFFFFFF0FFFL
    case TimeBased        extends Version(0x0000000000001000L)
    case DCESecurityBased extends Version(0x0000000000002000L)
    case MD5HashBased     extends Version(0x0000000000003000L)
    case RandomBased      extends Version(0x0000000000004000L)
    case SHA1HashBased    extends Version(0x0000000000005000L)
    case ISO3166Based     extends Version(0x0000000000006000L)

  object compat:

    type JavaUUID = java.util.UUID

    object JavaUUID:
      def apply(msb: Long, lsb: Long): JavaUUID =
        java.util.UUID.apply(msb, lsb)

    trait Dec[A]:
      extension (a: A) def decode: Option[UUID]

    given Dec[JavaUUID] =
      (uuid: JavaUUID) => Some(UUID(uuid.getMostSignificantBits)(uuid.getLeastSignificantBits))
