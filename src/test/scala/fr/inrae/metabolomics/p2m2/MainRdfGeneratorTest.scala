package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests, test}

import java.io.File
import scala.util.{Failure, Success, Try}

object MainRdfGeneratorTest extends TestSuite {

  val tests: Tests = Tests {

    test("Main with args") {
      fr.inrae.metabolomics.p2m2.MainRdfGenerator.main(Array(
        getClass.getResource("/20181018-037_388_390_Glucosinolate").getPath,
      ))
    }
  }

}
