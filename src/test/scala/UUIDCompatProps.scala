import org.scalacheck.*

object UUIDCompatProps extends Properties("uuid.compat"):

  import uuid.*
  import compat.{*, given}

  import Prop._
  import Variant.*
  import Version.*
  
  given genJavaUUID: Gen[JavaUUID] =
    for { msb <- Gen.long ; lsb <- Gen.long } yield java.util.UUID(msb, lsb)

  property("compat-java-uuid-variant") = forAll(genJavaUUID) { (inbound: JavaUUID) =>
    inbound.variant match
      case 0 => inbound.decode.get.variant == NCSBackwardsCompatible
      case 2 => inbound.decode.get.variant == LeachSalz
      case 6 => inbound.decode.get.variant == MicrosoftBackwardsCompatible
      case 7 => inbound.decode.get.variant == Reserved
      case _ => false
  }

  property("compat-java-uuid-version") = forAll(genJavaUUID) { (inbound: JavaUUID) =>
    inbound.version match
      case 1 => inbound.decode.get.version == Some(TimeBased)
      case 2 => inbound.decode.get.version == Some(DCESecurityBased)
      case 3 => inbound.decode.get.version == Some(MD5HashBased)
      case 4 => inbound.decode.get.version == Some(RandomBased)
      case 5 => inbound.decode.get.version == Some(SHA1HashBased)
      case 6 => inbound.decode.get.version == Some(ISO3166Based)
      case _ => inbound.decode.get.version == None
  }
