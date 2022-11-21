package fr.inrae.metabolomics.p2m2.diagnostic
import fr.inrae.metabolomics.p2m2.builder.ScanLoader
import utest.{TestSuite, Tests, test}

import java.io.File
import scala.util.Try

object DaughterIonsDiagTest extends TestSuite {
  val tests : Tests = Tests {
    val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))
    val v2 = {
      ScanLoader.selectEligibleIons(
        v._1,
        v._2,
        Some(11.5), // RT start
        Some(12.0), // RT end
        noiseIntensity = 0.1,
        nbCarbonMin = 4.0,
        nbCarbonMax = 20.0,
        nbSulfurMin = 2.0,
        nbSulfurMax = 5.0,
        minMzCoreStructure = 0.01,
        precisionDeltaM0M2 = 0.001,
        deltaMOM2 = 1.996
      )
    }

    test("test") {


      /*
          Frequence des Ions dans les scans de la liste des peak selectionné avec le delta
       */

      v2.map(
       p => DaughterIonsDiag.getPeaksWithIntensitiesNoNull(v._1, v._2,p)
      ).foldLeft(Map[Int,Int]())(
        (acc : Map[Int,Int],v : Seq[Int]) => {
          v.map( p2 => acc.get(p2) match {
              case Some(s) => p2 -> (s + 1)
              case None => p2 -> 1
          }).toMap
        }
      ).toSeq.sortBy(_._2)
    }
    test("IonsFrequencyOnSelectedScanPeakDetected") {
      assert(Try(DaughterIonsDiag.IonsFrequencyOnSelectedScanPeakDetected(v._1, v._2,v2)).isSuccess)
    }

  }
}