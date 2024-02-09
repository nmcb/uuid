package id
package test

import org.scalacheck.*
import id.compat.JavaUUID

object JavaUUIDCompatabilityProps extends Properties("uuid.compat"):

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
  import compat.JavaUUID.*

  def isJavaUUIDCompatible(javaUUID: JavaUUID): Boolean =
    isJavaUUIDVersionCompatible(javaUUID) &&
    isJavaUUIDVariantCompatible(javaUUID)

  def isJavaUUIDVariantCompatible(javaUUID: JavaUUID): Boolean =
    javaUUID.variant match
      case 0 => javaUUID.asScala.variant == NCSBackwardsCompatible
      case 2 => javaUUID.asScala.variant == LeachSalz
      case 6 => javaUUID.asScala.variant == MicrosoftBackwardsCompatible
      case 7 => javaUUID.asScala.variant == Reserved
      case _ => false

  def isJavaUUIDVersionCompatible(javaUUID: JavaUUID): Boolean =
    javaUUID.version match
      case 1 => javaUUID.asScala.version == Some(TimeBased)
      case 2 => javaUUID.asScala.version == Some(DCESecurityBased)
      case 3 => javaUUID.asScala.version == Some(MD5HashBased)
      case 4 => javaUUID.asScala.version == Some(RandomBased)
      case 5 => javaUUID.asScala.version == Some(SHA1HashBased)
      case 6 => javaUUID.asScala.version == Some(Version6)
      case 7 => javaUUID.asScala.version == Some(Version7)
      case 8 => javaUUID.asScala.version == Some(ISO3166Based)
      case _ => javaUUID.asScala.version == None

  def isJavaUUIDVersion4Compatible(javaUUID: JavaUUID): Boolean =
    javaUUID.asScala.version == Some(RandomBased) &&
    isJavaUUIDVariantCompatible(javaUUID)

  def isJavaUUIDVersion3Compatible(javaUUID: JavaUUID, name: Array[Byte]): Boolean =
    java.util.UUID.nameUUIDFromBytes(name) == javaUUID &&
    javaUUID.asScala.version == Some(MD5HashBased) &&
    isJavaUUIDVariantCompatible(javaUUID)

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
