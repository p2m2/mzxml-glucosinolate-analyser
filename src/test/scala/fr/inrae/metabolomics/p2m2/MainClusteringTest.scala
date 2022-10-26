package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests}

object MainClusteringTest extends TestSuite {

  val tests: Tests = Tests {
    /*
        java -cp ./assembly/pack.jar fr.inrae.metabolomics.p2m2.MainDetection -s 6.70 -e 6.72 ./src/test/resources/20181018-037.mzXML
        mv 20181018-037_Glucosinolate ./src/test/resources/20181018-037_388_390_Glucosinolate
     */
    fr.inrae.metabolomics.p2m2.MainClustering.main(Array(
      getClass.getResource("/20181018-037_388_390_Glucosinolate").getPath,
    ))
  }

}
