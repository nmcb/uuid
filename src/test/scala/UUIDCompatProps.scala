package id
package test

import org.scalacheck.*
import id.compat.JavaUUID

object JavaUUIDCompatabilityProperties extends Properties("uuid.compat"):

  import generators.*
  import Prop.*

  property("applyIsJavaUUIDCompatible") =
    forAll(javaApplyUUIDs)(isJavaUUIDCompatible)

  property("v4IsJavaUUIDCompatible - RandomBased") =
    forAll(javaVersion4UUIDs)(isJavaUUIDVersion4Compatible)

  property("v3IsJavaUUIDCompatible - MD5HashBased") =
    forAll(javaVersion3UUIDs)(isJavaUUIDVersion3Compatible)

  import id.*
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

  def isJavaUUIDVersion4Compatible(javaUUID: JavaUUID): Boolean =
    javaUUID.compatibleWith(_.version == Some(RandomBased)) &&
    isJavaUUIDVariantCompatible(javaUUID)

  def isJavaUUIDVersion3Compatible(javaUUID: JavaUUID, name: Array[Byte]): Boolean =
    java.util.UUID.nameUUIDFromBytes(name) == javaUUID &&
    javaUUID.compatibleWith(_.version == Some(MD5HashBased)) &&
    isJavaUUIDVariantCompatible(javaUUID)


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

    val javaVersion3UUIDs: Gen[(JavaUUID,Array[Byte])] =    
      Gen
        .containerOf[Array,Byte](arbitrary[Byte])
        .map(bytes => (JavaUUID.nameUUIDFromBytes(bytes), bytes))
