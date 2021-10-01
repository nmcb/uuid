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

  def compatibleWith(assertion: UUID => Boolean)(javaUUID: JavaUUID): Boolean =
    javaUUID.asScala.map(assertion).getOrElse(false)

  val isJavaUUIDVariantCompatible: JavaUUID => Boolean =
    (javaUUID: JavaUUID) =>
      javaUUID.variant match
        case 0 => compatibleWith(_.variant == NCSBackwardsCompatible)(javaUUID)
        case 2 => compatibleWith(_.variant == LeachSalz)(javaUUID)
        case 6 => compatibleWith(_.variant == MicrosoftBackwardsCompatible)(javaUUID)
        case 7 => compatibleWith(_.variant == Reserved)(javaUUID)
        case _ => false

  val isJavaUUIDVersionCompatible: JavaUUID => Boolean =
    (javaUUID: JavaUUID) =>
      javaUUID.version match
        case 1 => compatibleWith(_.version == Some(TimeBased))(javaUUID)
        case 2 => compatibleWith(_.version == Some(DCESecurityBased))(javaUUID)
        case 3 => compatibleWith(_.version == Some(MD5HashBased))(javaUUID)
        case 4 => compatibleWith(_.version == Some(RandomBased))(javaUUID)
        case 5 => compatibleWith(_.version == Some(SHA1HashBased))(javaUUID)
        case 6 => compatibleWith(_.version == Some(ISO3166Based))(javaUUID)
        case _ => compatibleWith(_.version == None)(javaUUID)

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
