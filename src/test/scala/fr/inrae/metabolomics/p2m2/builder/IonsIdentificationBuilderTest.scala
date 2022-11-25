package fr.inrae.metabolomics.p2m2.builder

import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}
import utest.{TestSuite, Tests, test}

import java.io.File

object IonsIdentificationBuilderTest extends TestSuite {
  val v: (MZXMLFile, MZXMLIndex) = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

  val tests: Tests = Tests {
    test("instance") {
      IonsIdentificationBuilder(v._1,v._2,Seq(),Seq(),Seq())
    }

    test("getInfo") {
      IonsIdentificationBuilder(v._1,v._2,Seq(),Seq(),Seq()).getInfo(PeakIdentification(0,Seq(1501),Seq()),0.1)
    }

    test("getInfo") {
      IonsIdentificationBuilder(v._1, v._2, Seq(), Seq(), Seq())
        .getInfo(PeakIdentification(0, Seq(1501), Seq()), 0.1)
    }

    test("getInfo 2") {
      IonsIdentificationBuilder(v._1, v._2, Seq(), Seq(), Seq()).getInfo(
        PeakIdentification(3569, Seq(1501),
        Seq(Peak(0,0.0,0,0))),0.1)
    }

    test("getInfos") {
      IonsIdentificationBuilder(v._1, v._2,Seq(), Seq(), Seq()).findDiagnosticIonsAndNeutralLosses(0.1)
    }

    test("getInfos 2") {
      IonsIdentificationBuilder(v._1, v._2,
        Seq(PeakIdentification(3569, Seq(1501), Seq(Peak(0, 0.0, 0, 0)))),
        Seq(), Seq())
        .findDiagnosticIonsAndNeutralLosses(0.1)
    }

  }
}
