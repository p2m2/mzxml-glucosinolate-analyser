package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests}

object MainClusteringTest extends TestSuite {

  val tests: Tests = Tests {
    fr.inrae.metabolomics.p2m2.MainClustering.main(Array(
      getClass.getResource("/20181018-037_388_390_Glucosinolate").getPath,
    ))
  }

}
