package id

import org.scalacheck.*

object JavaUUIDCompatibilityProps extends Properties("uuid.compat"):

  import Prop.*
  import generators.*

  property("applyIsJavaUUIDCompatible") =
    forAll(javaApplyUUIDs)(isJavaUUIDCompatible)

  property("v4IsJavaUUIDCompatible - RandomBased") =
    forAll(javaVersion4UUIDs)(isJavaUUIDVersion4Compatible)

  property("v3IsJavaUUIDCompatible - MD5HashBased") =
    forAll(javaVersion3UUIDs)(isJavaUUIDVersion3Compatible)

  import compat.{*, given}
  import JavaUUID.*
  import Variant.*
  import Version.*

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
      case 1 => javaUUID.asScala.version.contains(TimeBased)
      case 2 => javaUUID.asScala.version.contains(DCESecurityBased)
      case 3 => javaUUID.asScala.version.contains(MD5HashBased)
      case 4 => javaUUID.asScala.version.contains(RandomBased)
      case 5 => javaUUID.asScala.version.contains(SHA1HashBased)
      case 6 => javaUUID.asScala.version.contains(Version6)
      case 7 => javaUUID.asScala.version.contains(Version7)
      case 8 => javaUUID.asScala.version.contains(ISO3166Based)
      case _ => javaUUID.asScala.version.isEmpty

  def isJavaUUIDVersion4Compatible(javaUUID: JavaUUID): Boolean =
    val isRandomBased = javaUUID.asScala.version.contains(RandomBased)
    isRandomBased && isJavaUUIDVariantCompatible(javaUUID)

  def isJavaUUIDVersion3Compatible(javaUUID: JavaUUID, name: Array[Byte]): Boolean =
    val isNameBased = java.util.UUID.nameUUIDFromBytes(name) == javaUUID
    val isMD5Hashed = javaUUID.asScala.version.contains(MD5HashBased)
    isNameBased && isMD5Hashed && isJavaUUIDVariantCompatible(javaUUID)

  object generators:

    import Arbitrary.*

    val javaApplyUUIDs: Gen[JavaUUID] =
      for
        msb <- arbitrary[Long]
        lsb <- arbitrary[Long]
      yield
        JavaUUID(msb, lsb)

    val javaVersion4UUIDs: Gen[JavaUUID] =    
      Gen.map(_ => JavaUUID.randomUUID)

    val javaVersion3UUIDs: Gen[(JavaUUID, Array[Byte])] =    
      Gen
        .containerOf[Array, Byte](arbitrary[Byte])
        .map(bytes => (JavaUUID.nameUUIDFromBytes(bytes), bytes))
