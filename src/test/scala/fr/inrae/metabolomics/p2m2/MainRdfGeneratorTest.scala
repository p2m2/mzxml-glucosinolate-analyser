package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests, test}

import java.io.File

object MainRdfGeneratorTest extends TestSuite {

  val tests: Tests = Tests {

    test("Main with args") {
      /**
       * Generation of dump
       */
      fr.inrae.metabolomics.p2m2.MainDetection.main(Array(
        "-s", "5.0",
        "-e", "7.5",
        "-o", "./test",
        getClass.getResource("/20181018-037.mzXML").getPath))

      fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        "./test",
      ))

      new File("./test").delete()
    }
  }

}
