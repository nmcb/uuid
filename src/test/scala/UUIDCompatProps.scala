package identification
package test

import org.scalacheck.*
import identification.compat.JavaUUID

object JavaUUIDCompatabilityProperties extends Properties("uuid.compat"):

  import generators.*
  import Prop.*

  property("applyIsJavaUUIDVariantCompatible") = forAll(javaApplyUUIDs)(isJavaUUIDVariantCompatible)
  property("applyIsJavaUUIDVersionCompatible") = forAll(javaApplyUUIDs)(isJavaUUIDVersionCompatible)
  property("v4IsJavaUUIDVariantCompatible") = forAll(javaVersion4UUIDs)(isJavaUUIDVariantCompatible)
  property("v4IsJavaUUIDVersionCompatible") = forAll(javaVersion4UUIDs)(isJavaUUIDVersionCompatible)
  property("v5IsJavaUUIDVariantCompatible") = forAll(javaVersion5UUIDs)(isJavaUUIDVariantCompatible)
  property("v5IsJavaUUIDVersionCompatible") = forAll(javaVersion5UUIDs)(isJavaUUIDVersionCompatible)


  import identification.*
  import Variant.*
  import Version.*
  
  import compat.*
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

  object generators:

    import Arbitrary.*

    val javaApplyUUIDs: Gen[JavaUUID] =
      for {
        msb <- arbitrary[Long]
        lsb <- arbitrary[Long]
      } yield JavaUUID(msb, lsb)

    val javaVersion4UUIDs: Gen[JavaUUID] =    
      Gen.map(_ => JavaUUID.randomUUID)

    val javaVersion5UUIDs: Gen[JavaUUID] =    
      Gen.containerOf[Array,Byte](arbitrary[Byte]).map(JavaUUID.nameUUIDFromBytes)

