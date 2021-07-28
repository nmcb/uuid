object uuid:

  import java.util.Locale

  case class UUID(msb: Long, lsb: Long):

    import Variant.*
    import Version.*

    def source: String =
      ???

    def target: String =
      ???

    val variant: Variant =
      ((lsb >>> 61) & 0x07) match
        case 0x00 => NCSBackwardsCompatible
        case 0x01 => NCSBackwardsCompatible
        case 0x02 => NCSBackwardsCompatible
        case 0x03 => NCSBackwardsCompatible
        case 0x04 => LeachSalz
        case 0x05 => LeachSalz
        case 0x06 => MicrosoftBackwardsCompatible
        case 0x07 => Reserved

    val version: Option[Version] =
      ((msb >>> 12) & 0x0F) match
        case 0x01 => Some(TimeBased)
        case 0x02 => Some(DCESecurityBased)
        case 0x03 => Some(MD5HashBased)
        case 0x04 => Some(RandomBased)
        case 0x05 => Some(SHA1HashBased)
        case 0x06 => Some(ISO3166Based)
        case _    => None

  enum Variant:
    case NCSBackwardsCompatible
    case LeachSalz
    case MicrosoftBackwardsCompatible
    case Reserved

  enum Version:
    case TimeBased
    case DCESecurityBased
    case MD5HashBased
    case RandomBased
    case SHA1HashBased
    case ISO3166Based

  object compat:

    type JavaUUID = java.util.UUID

    trait Dec[A]:
      extension (a: A) def decode: Option[UUID]

    given Dec[JavaUUID] =
      (uuid: JavaUUID) => Some(UUID(uuid.getMostSignificantBits, uuid.getLeastSignificantBits))
