package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests, test}

object MainRdfBrachemDbTest extends TestSuite {

  val tests: Tests = Tests {

    test("Main with args") {
      /**
       * Generation of dump
       */

      fr.inrae.metabolomics.p2m2.MainRdfBrachemDb.main(Array(
        "-o", "./test.ttl"
      ))
    }
  }

}
