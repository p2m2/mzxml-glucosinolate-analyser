package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.builder.ScanLoaderTest.getClass
import umich.ms.datatypes.scan.IScan
import utest.{TestSuite, Tests, test}

import java.io.File

object ScanLoaderDetectNeutralLossesTest extends TestSuite {
  val tests = Tests {
    val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

    test("detectNeutralLoss") {
      //3569
      val scan: IScan  = v._1.parseScan(3569, true)
      val spectrum = scan.fetchSpectrum()
     // (spectrum.getMZs.zipWithIndex.foreach(println))
      //1501 M
      //1509 M+1
      //1516 M+2
      println(spectrum.getMZs()(1501),spectrum.getIntensities()(1501))
      println(spectrum.getMZs()(1509),spectrum.getIntensities()(1509))
      println(spectrum.getMZs()(1516),spectrum.getIntensities()(1516))
      println(scan.getBasePeakIntensity)
      val r = (ScanLoader.getScanIdxAndSpectrum3IsotopesSulfurContaining(v._1,v._2))
      println(r)
    }

  }
}