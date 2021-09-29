package identification

import org.scalacheck.*

object UUIDCompatProps extends Properties("uuid.compat"):

  import identification.*
  import Variant.*
  import Version.*
  
  import compat.JavaUUID
  import compat.given

  val isJavaUUIDVariantCompatible: JavaUUID => Boolean =
    (javaUUID: JavaUUID) =>
      javaUUID.variant match
        case 0 => javaUUID.asScala.get.variant == NCSBackwardsCompatible
        case 2 => javaUUID.asScala.get.variant == LeachSalz
        case 6 => javaUUID.asScala.get.variant == MicrosoftBackwardsCompatible
        case 7 => javaUUID.asScala.get.variant == Reserved
        case _ => false

  val isJavaUUIDVersionCompatible: JavaUUID => Boolean =
    (javaUUID: JavaUUID) =>
      javaUUID.version match
        case 1 => javaUUID.asScala.get.version == Some(TimeBased)
        case 2 => javaUUID.asScala.get.version == Some(DCESecurityBased)
        case 3 => javaUUID.asScala.get.version == Some(MD5HashBased)
        case 4 => javaUUID.asScala.get.version == Some(RandomBased)
        case 5 => javaUUID.asScala.get.version == Some(SHA1HashBased)
        case 6 => javaUUID.asScala.get.version == Some(ISO3166Based)
        case _ => javaUUID.asScala.get.version == None

  import Prop.*

  import generators.*

  property("java-variant-v4")    = forAll(javaVersion4UUID)(isJavaUUIDVariantCompatible)
  property("java-version-v4")    = forAll(javaVersion4UUID)(isJavaUUIDVersionCompatible)
  property("java-variant-apply") = forAll(javaUUID)(isJavaUUIDVariantCompatible)
  property("java-version-apply") = forAll(javaUUID)(isJavaUUIDVersionCompatible)

  object generators:

    val javaUUID: Gen[JavaUUID] =
      for { msb <- Gen.long ; lsb <- Gen.long } yield JavaUUID(msb, lsb)

    val javaVersion4UUID: Gen[JavaUUID] =    
      Gen.label("java.util.UUID.randomUUID").map(_ => java.util.UUID.randomUUID)
