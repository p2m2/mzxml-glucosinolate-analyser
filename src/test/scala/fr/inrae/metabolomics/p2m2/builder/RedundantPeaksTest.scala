package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.builder.ScanLoaderDetectNeutralLossesTest.getClass
import umich.ms.datatypes.scan.IScan
import utest.{TestSuite, Tests, test}

import java.io.File
import scala.jdk.CollectionConverters.CollectionHasAsScala

object RedundantPeaksTest extends TestSuite {


  val tests = Tests {
    val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

    def exec(mz : Double) = {
      v._2
        .getMapByRawNum
        .keySet() // The second parameter asks the parser to parse the spectrum along
        .asScala
        // .filter( _ == 3569)
        .filter(scanNumRaw => v._1.parseScan(scanNumRaw, false).getMsLevel == 1)
        //.slice(0, 10)
        .map(
          scanNumRaw => {
            // Do something with the scan.
            // Note that some features, like scan.getChildScans() will not work in
            // this case, as there is not enough information to build those
            // relationships.
            val scan: IScan = v._1.parseScan(scanNumRaw, true)
            val spectrum = scan.fetchSpectrum()
            val idx = spectrum.findClosestMzIdx(mz)
            spectrum.getIntensities()(idx)
          }
        )
    }

    def printC(mz : Double) = println(s"$mz=>" , exec(mz).count(_ != 0))
    test("detectNeutralLoss") {
      //MetaboliteIdentification(v._1,v._2,Seq(PeakIdentification(3569,Seq(1501)))).filterOverRepresentedPeak(100)
      printC(308.94)
     // printC(305.858)
     // printC(479.056)
    }



  }
}
