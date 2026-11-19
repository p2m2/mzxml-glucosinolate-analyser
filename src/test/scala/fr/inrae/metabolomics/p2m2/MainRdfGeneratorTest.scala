package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests, test}

import scala.util.Try

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
        "-m","4",
        "-o","./test.ttl",
        "./test",
      ))

      fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        "-o", "./test.jsonld",
        "./test",
      ))

      fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        "-o", "./test.trig",
        "./test",
      ))

      fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        "-o", "./test.nt",
        "./test",
      ))

      fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        "-o", "./test.rdf",
        "./test",
      ))

      fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        "-o", "./test.n3",
        "./test",
      ))

      assert(Try(fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        "-o", "./test.toto",
        "./test",
      ))).isFailure)
    }
  }

}
