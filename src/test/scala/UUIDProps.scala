import org.scalacheck.*

object UUIDProps extends Properties("uuid.UUID"):

  import uuid.*
  import util.*
  import util.given

  import Prop.*
  import Gen.*
  import Variant.*
  import Version.*

  property("variant") = forAll { (msb: Long, lsb: Long) =>
    UUID(msb,lsb).variant match
      case Reserved                     => lsb.toBinaryString.startsWith("111")
      case MicrosoftBackwardsCompatible => lsb.toBinaryString.startsWith("110")
      case LeachSalz                    => lsb.toBinaryString.startsWith("10")
      case NCSBackwardsCompatible       => true
  }

  property("version") = forAll { (msb: Long, lsb: Long) =>
    UUID(msb,lsb).version match
      case Some(TimeBased)        =>  (msb & 0x0000F000) == 0x1000
      case Some(DCESecurityBased) =>  (msb & 0x0000F000) == 0x2000
      case Some(MD5HashBased)     =>  (msb & 0x0000F000) == 0x3000
      case Some(RandomBased)      =>  (msb & 0x0000F000) == 0x4000
      case Some(SHA1HashBased)    =>  (msb & 0x0000F000) == 0x5000
      case Some(ISO3166Based)     =>  (msb & 0x0000F000) == 0x6000
      case None                   => ((msb & 0x0000F000) == 0x0000) || ((msb & 0x0000F000) >= 0x6000)
  }

  property("iso3166") = forAll(sourcesAndTargets) { (source: String, target: String) =>
      val uuid = UUID.iso3166(source, target)
      (uuid.variant == LeachSalz) && (uuid.version.get == ISO3166Based) && (source == uuid.source) && (target == uuid.target)
  }

  given sourcesAndTargets: Gen[(String, String)] =
    for {
      source <- stringOfN(2, alphaUpperChar)
      target <- stringOfN(2, alphaUpperChar)
    } yield (source, target)
