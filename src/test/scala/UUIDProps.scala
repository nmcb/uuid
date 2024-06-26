package id

import org.scalacheck.*
import id.UUID.CountryCode

object UUIDProps extends Properties("uuid.UUID"):

  import id.*

  import util.*
  import util.given
  import Prop.*
  import Variant.*
  import Version.*

  property("variant") = forAll { (msb: Long, lsb: Long) =>
    UUID(msb, lsb).variant match
      case Reserved                     => lsb.toBinaryString.startsWith("111")
      case MicrosoftBackwardsCompatible => lsb.toBinaryString.startsWith("110")
      case LeachSalz                    => lsb.toBinaryString.startsWith("10")
      case NCSBackwardsCompatible       => true
  }

  property("version") = forAll { (msb: Long, lsb: Long) =>
    UUID(msb, lsb).version match
      case Some(TimeBased)        =>  (msb & 0xF000L) == 0x1000L
      case Some(DCESecurityBased) =>  (msb & 0xF000L) == 0x2000L
      case Some(MD5HashBased)     =>  (msb & 0xF000L) == 0x3000L
      case Some(RandomBased)      =>  (msb & 0xF000L) == 0x4000L
      case Some(SHA1HashBased)    =>  (msb & 0xF000L) == 0x5000L
      case Some(Version6)         =>  (msb & 0xF000L) == 0x6000L
      case Some(Version7)         =>  (msb & 0xF000L) == 0x7000L
      case Some(ISO3166Based)     =>  (msb & 0xF000L) == 0x8000L
      case None                   => ((msb & 0xF000L) == 0x0000L) || ((msb & 0xF000L) >= 0x7000L)
  }

  property("iso3166") = forAll(isoSourcesAndTargets) { (source: CountryCode, target: CountryCode) =>
      val uuid = UUID.iso3166(source, target)
      val correctType = (uuid.variant == LeachSalz) && (uuid.version.get == ISO3166Based)
      val correctData = (uuid.sourceCountryCode.get == source) && (uuid.targetCountryCode.get == target)
      correctType && correctData
  }

  val isoSourcesAndTargets: Gen[(CountryCode, CountryCode)] =
    import compat.JavaCountryCodes
    for {
      source <- Gen.oneOf(JavaCountryCodes).map(CountryCode.apply)
      target <- Gen.oneOf(JavaCountryCodes).map(CountryCode.apply)
    } yield (source, target)
