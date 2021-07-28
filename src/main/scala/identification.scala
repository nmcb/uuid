object uuid:

  case class UUID(msb: Long, lsb: Long):

    val variant: Option[Variant] =
      ((lsb >>> 61) & 0x07) match
        case 0x00 => Some(Variant.NCSCompatible)
        case 0x01 => Some(Variant.NCSCompatible)
        case 0x02 => Some(Variant.NCSCompatible)
        case 0x03 => Some(Variant.NCSCompatible)
        case 0x04 => Some(Variant.LeachSalz)
        case 0x05 => Some(Variant.LeachSalz)
        case 0x06 => Some(Variant.MicrosoftCompatible)
        case 0x07 => Some(Variant.Reserved)
        case _    => None

    val version: Option[Version] =
      ((msb >>> 12) & 0x0F) match
        case 0x01 => Some(Version.TimeBased)
        case 0x02 => Some(Version.DCESecurityBased)
        case 0x03 => Some(Version.MD5HashBased)
        case 0x04 => Some(Version.RandomBased)
        case 0x05 => Some(Version.SHA1HashBased)
        case _    => None

  enum Variant:
    case NCSCompatible
    case LeachSalz
    case MicrosoftCompatible
    case Reserved

  enum Version:
    case TimeBased
    case DCESecurityBased
    case MD5HashBased
    case RandomBased
    case SHA1HashBased


  object compat:

    type JavaUUID = java.util.UUID

    trait Dec[A]:
      extension (a: A) def decode: Option[UUID]

    given Dec[JavaUUID] =
      (uuid: JavaUUID) => Some(UUID(uuid.getMostSignificantBits, uuid.getLeastSignificantBits))
