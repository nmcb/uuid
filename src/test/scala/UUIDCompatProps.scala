package identification
package test

import org.scalacheck.*
import identification.compat.JavaUUID

object JavaUUIDCompatabilityProperties extends Properties("uuid.compat"):

  import generators.*
  import Prop.*

  property("applyIsJavaUUIDCompatible") = forAll(javaApplyUUIDs)(isJavaUUIDCompatible)
  property("v4IsJavaUUIDCompatible") = forAll(javaVersion4UUIDs)(isJavaUUIDCompatible)
  property("v5IsJavaUUIDCompatible") = forAll(javaVersion5UUIDs)(isJavaUUIDCompatible)

  import identification.*
  import Variant.*
  import Version.*
  
  import compat.*
  import compat.given

  def isJavaUUIDCompatible(javaUUID: JavaUUID): Boolean =
    isJavaUUIDVersionCompatible(javaUUID) &&
    isJavaUUIDVariantCompatible(javaUUID)

  def isJavaUUIDVariantCompatible(javaUUID: JavaUUID): Boolean =
    javaUUID.variant match
      case 0 => javaUUID.compatibleWith(_.variant == NCSBackwardsCompatible)
      case 2 => javaUUID.compatibleWith(_.variant == LeachSalz)
      case 6 => javaUUID.compatibleWith(_.variant == MicrosoftBackwardsCompatible)
      case 7 => javaUUID.compatibleWith(_.variant == Reserved)
      case _ => false

  def isJavaUUIDVersionCompatible(javaUUID: JavaUUID): Boolean =
    javaUUID.version match
      case 1 => javaUUID.compatibleWith(_.version == Some(TimeBased))
      case 2 => javaUUID.compatibleWith(_.version == Some(DCESecurityBased))
      case 3 => javaUUID.compatibleWith(_.version == Some(MD5HashBased))
      case 4 => javaUUID.compatibleWith(_.version == Some(RandomBased))
      case 5 => javaUUID.compatibleWith(_.version == Some(SHA1HashBased))
      case 6 => javaUUID.compatibleWith(_.version == Some(ISO3166Based))
      case _ => javaUUID.compatibleWith(_.version == None)

  extension (javaUUID: JavaUUID) def compatibleWith(assertion: UUID => Boolean): Boolean =
    javaUUID.asScala.map(assertion).getOrElse(false)


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
