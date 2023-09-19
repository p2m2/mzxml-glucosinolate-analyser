package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests}

object MainClusteringTest extends TestSuite {

  val tests: Tests = Tests {
    /**
     * Generation of dump
     */
    fr.inrae.metabolomics.p2m2.MainDetection.main(Array(
      "-s", "5.0",
      "-e", "7.5",
      "-o", "./testClustering",
      getClass.getResource("/20181018-037.mzXML").getPath))
    fr.inrae.metabolomics.p2m2.MainClustering.main(Array(
      "./testClustering",
    ))
  }

}
