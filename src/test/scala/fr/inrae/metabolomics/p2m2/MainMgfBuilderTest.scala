package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests}

object MainMgfBuilderTest extends TestSuite {

  val tests: Tests = Tests {
    /**
     * Generation of dump
     */
    fr.inrae.metabolomics.p2m2.MainDetection.main(Array(
      "-s", "5.0",
      "-e", "7.5",
      "-o", "./test",
      getClass.getResource("/20181018-037.mzXML").getPath))
    fr.inrae.metabolomics.p2m2.MainMgfBuilder.main(Array(
      "-n","500.0",
      "-m","0.1",
      "-d","10",
      "-p","2",
      "-q", "0",
      "-r", "1",
      "./test",
    ))
  }

}
