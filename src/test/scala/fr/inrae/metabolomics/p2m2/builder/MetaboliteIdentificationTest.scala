package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.builder.ScanLoaderTest.getClass
import utest.{TestSuite, Tests, test}

import java.io.File

object MetaboliteIdentificationTest extends TestSuite {
  val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

  val tests: Tests = Tests {
    test("instance") {
      MetaboliteIdentification(v._1,v._2,None,None,Seq(),Seq(),Seq())
    }

    test("getInfo") {
      MetaboliteIdentification(v._1,v._2,None,None,Seq(),Seq(),Seq()).getInfo(PeakIdentification(0,Seq(1501),Seq(),0.0),1)
    }



    test("getInfo 2") {
      MetaboliteIdentification(v._1, v._2, None, None, Seq(), Seq(), Seq()).getInfo(
        PeakIdentification(3569, Seq(1501),
        Seq(Peak(0,0.0,0,0,0)), 0.0), 1)
    }

    test("getInfos") {
      MetaboliteIdentification(v._1, v._2, None, None, Seq(), Seq(), Seq()).getInfos(1)
    }

    test("getInfos 2") {
      MetaboliteIdentification(v._1, v._2, None, None,
        Seq(PeakIdentification(3569, Seq(1501), Seq(Peak(0, 0.0, 0, 0, 0)), 0.0)),
        Seq(), Seq()).getInfos(1)
    }

  }
}
