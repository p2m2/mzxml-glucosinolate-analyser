package fr.inrae.metabolomics.p2m2.builder

import utest.{TestSuite, Tests, test}

import java.io.File
import scala.Double.NaN

object ScanLoaderTest extends TestSuite {
  val tests = Tests {

    def read = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

    test("read") {
      val v = read
      assert(v._1 != null && v._2 != null)
    }

    test("getScanIdxAndSpectrum3IsotopesSulfurContaining - nbCarbonMax=0 should find 0 ions >>>") {
      val v = read
      val v2 =
        ScanLoader.selectEligibleIons(
          v._1,
          v._2,
          None,  // RT start
          None,  // RT end
          noiseIntensity=0.1,
          5.0,
          nbCarbonMax=0.0,
          2.0,
          5.0,
          deltaMOM2 = 1.996
        )
      assert(v2.isEmpty)
    }

    test("getScanIdxAndSpectrum3IsotopesSulfurContaining - nbSulfurMax=0 should find 0 ions >>>") {
      val v = read
      val v2 =
        ScanLoader.selectEligibleIons(
          v._1,
          v._2,
          None, // RT start
          None, // RT end
          noiseIntensity = 0.1,
          5.0,
          nbCarbonMax = 25.0,
          2.0,
          0.0,
          deltaMOM2 = 1.996
        )
      assert(v2.isEmpty)
    }

    test("getScanIdxAndSpectrum3IsotopesSulfurContaining - nbSulfurMin>nbSulfurMax should find 0 ions >>>") {
      val v = read
      val v2 =
        ScanLoader.selectEligibleIons(
          v._1,
          v._2,
          None, // RT start
          None, // RT end
          noiseIntensity = 0.1,
          5.0,
          nbCarbonMax = 25.0,
          2.0,
          1.0,
          deltaMOM2 = 1.996
        )
      assert(v2.isEmpty)
    }

    test("getScanIdxAndSpectrum3IsotopesSulfurContaining - nbSulfurMin<nbSulfurMax & nbCarbonMin<nbCarbonMax " +
      "should find a ions list not empty >>>") {
      val v = read
      val v2 =
        ScanLoader.selectEligibleIons(
          v._1,
          v._2,
          None, // RT start
          None, // RT end
          noiseIntensity = 0.1,
          5.0,
          nbCarbonMax = 25.0,
          2.0,
          5.0,
          deltaMOM2 = 1.996
        )
      assert(v2.nonEmpty)
    }

    test("calculBackgroundNoisePeak") {
      val v = read
      val v2 = ScanLoader.calcBackgroundNoisePeak(
        v._1,
        v._2,
        0.2
      )
      assert(v2 == v2)
    }

    test("keepSimilarMzWithMaxAbundance") {
      ScanLoader.keepSimilarMzWithMaxAbundance(
        Seq(PeakIdentification(3569, Seq(1501), Seq(Peak(0, 0.0, 0, 0)), 0.0)),1
      )
    }


  }
}
