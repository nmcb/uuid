import org.scalacheck.*

object UUIDCompatProps extends Properties("uuid.compat"):

  import Prop._

  import uuid.*
  import compat.{*, given}
  
  given genJavaUUID: Gen[JavaUUID] =
    for { msb <- Gen.long ; lsb <- Gen.long } yield java.util.UUID(msb, lsb)

  property("compat-java-uuid-variant") = forAll(genJavaUUID) { (inbound: JavaUUID) =>
    inbound.variant match
      case 0 => inbound.decode.get.variant == Some(Variant.NCSCompatible)
      case 2 => inbound.decode.get.variant == Some(Variant.LeachSalz)
      case 6 => inbound.decode.get.variant == Some(Variant.MicrosoftCompatible)
      case 7 => inbound.decode.get.variant == Some(Variant.Reserved)
      case _ => inbound.decode.get.variant == None
  }

  property("compat-java-uuid-version") = forAll(genJavaUUID) { (inbound: JavaUUID) =>
    inbound.version match
      case 1 => inbound.decode.get.version == Some(Version.TimeBased)
      case 2 => inbound.decode.get.version == Some(Version.DCESecurityBased)
      case 3 => inbound.decode.get.version == Some(Version.MD5HashBased)
      case 4 => inbound.decode.get.version == Some(Version.RandomBased)
      case 5 => inbound.decode.get.version == Some(Version.SHA1HashBased)
      case _ => inbound.decode.get.version == None
  }
