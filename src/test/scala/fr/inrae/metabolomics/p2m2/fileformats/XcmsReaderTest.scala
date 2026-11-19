package fr.inrae.metabolomics.p2m2.fileformats

import utest.{TestSuite, Tests, test}

import java.io.File

object XcmsReaderTest extends TestSuite {

  val tests: Tests = Tests {

    test("detect xcms files correctly") {
      val reader = new XcmsReader()
      assert(reader.supports(new File("test.xcms")))
      assert(reader.supports(new File("test.csv")))
      assert(!reader.supports(new File("test.txt")))
      assert(!reader.supports(new File("test.mzml")))
      assert(!reader.supports(new File("test.mzxml")))
    }

    // Tests supplémentaires pour la lecture de fichiers
    // (nécessitera des fichiers de test réels ou des mocks)
  }
}
