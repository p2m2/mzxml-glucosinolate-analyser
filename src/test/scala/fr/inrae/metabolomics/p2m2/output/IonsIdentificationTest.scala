package fr.inrae.metabolomics.p2m2.output

import fr.inrae.metabolomics.p2m2.builder.{PeakIdentification, ScanLoader}
import upickle.default._
import utest.{TestSuite, Tests, test}

import java.io.File
import scala.util.Try

object IonsIdentificationTest extends TestSuite {

  val tests: Tests = Tests {
    test("empty serialization") {
      assert(Try(read[IonsIdentification](write(IonsIdentification("",PeakIdentification(),Map(),Map())))).isSuccess)
    }

    test("serialization") {
      val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

      for (elem <- ScanLoader.
          getScanIdxAndSpectrumM0M2WithDelta(
            v._1,
            v._2,
            Some(10.0),
            Some(12.8),
            0.1,
            nbCarbonMin = 4,
            nbCarbonMax = 20,
            nbSulfurMin = 2,
            nbSulfurMax = 5,
            deltaMOM2 = 1.996,
            )) {
          assert(Try(read[PeakIdentification](write(elem))).isSuccess)
        }
    }

  }

}
