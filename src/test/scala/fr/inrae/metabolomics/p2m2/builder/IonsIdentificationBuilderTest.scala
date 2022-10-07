package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.builder.ScanLoaderTest.getClass
import utest.{TestSuite, Tests, test}

import java.io.File

object IonsIdentificationBuilderTest extends TestSuite {
  val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

  val tests: Tests = Tests {
    test("instance") {
      IonsIdentificationBuilder(v._1,v._2,None,None,Seq(),Seq(),Seq())
    }

    test("getInfo") {
      IonsIdentificationBuilder(v._1,v._2,None,None,Seq(),Seq(),Seq()).getInfo(PeakIdentification(0,Seq(1501),Seq(),0.0),1,0.0)
    }

    test("getInfo 2") {
      IonsIdentificationBuilder(v._1, v._2, None, None, Seq(), Seq(), Seq()).getInfo(
        PeakIdentification(3569, Seq(1501),
        Seq(Peak(0,0.0,0,0)), 0.0), 1,0.0)
    }

    test("getInfos") {
      IonsIdentificationBuilder(v._1, v._2, None, None, Seq(), Seq(), Seq()).findDiagnosticIonsAndNeutralLosses(1,0.0)
    }

    test("getInfos 2") {
      IonsIdentificationBuilder(v._1, v._2, None, None,
        Seq(PeakIdentification(3569, Seq(1501), Seq(Peak(0, 0.0, 0, 0)), 0.0)),
        Seq(), Seq())
        .findDiagnosticIonsAndNeutralLosses(1,0.0)
    }

  }
}
