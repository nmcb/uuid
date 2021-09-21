package identification

import org.scalacheck.*

object UUIDCompatProps extends Properties("uuid.compat"):

  import Prop._

  import uuid.*
  import Variant.*
  import Version.*
  
  import compat.given
  import compat.JavaUUID

  given genJavaUUID: Gen[JavaUUID] =
    for { msb <- Gen.long ; lsb <- Gen.long } yield JavaUUID(msb, lsb)

  property("compat-java-uuid-variant") = forAll(genJavaUUID) { (javaUUID: JavaUUID) =>
    javaUUID.variant match
      case 0 => javaUUID.decode.get.variant == NCSBackwardsCompatible
      case 2 => javaUUID.decode.get.variant == LeachSalz
      case 6 => javaUUID.decode.get.variant == MicrosoftBackwardsCompatible
      case 7 => javaUUID.decode.get.variant == Reserved
      case _ => false
  }

  property("compat-java-uuid-version") = forAll(genJavaUUID) { (javaUUID: JavaUUID) =>
    javaUUID.version match
      case 1 => javaUUID.decode.get.version == Some(TimeBased)
      case 2 => javaUUID.decode.get.version == Some(DCESecurityBased)
      case 3 => javaUUID.decode.get.version == Some(MD5HashBased)
      case 4 => javaUUID.decode.get.version == Some(RandomBased)
      case 5 => javaUUID.decode.get.version == Some(SHA1HashBased)
      case 6 => javaUUID.decode.get.version == Some(ISO3166Based)
      case _ => javaUUID.decode.get.version == None
  }
