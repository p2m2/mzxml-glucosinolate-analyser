package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests, test}

import java.io.File
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
/*
      new File("./test").delete()
      new File("./test.ttl").delete()
      new File("./test.jsonld").delete()
      new File("./test.trig").delete()
      new File("./test.nt").delete()
      new File("./test.rdf").delete()
      new File("./test.n3").delete()
 */
    }
  }

}
