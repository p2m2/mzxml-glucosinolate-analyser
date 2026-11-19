package fr.inrae.metabolomics.p2m2.fileformats

import utest.{TestSuite, Tests, test}

import java.io.File

object MzXmlReaderTest extends TestSuite {

  val tests: Tests = Tests {

    test("detect mzXML files correctly") {
      val reader = new MzXmlReader()
      assert(reader.supports(new File("test.mzxml")))
      assert(!reader.supports(new File("test.mzml")))
      assert(!reader.supports(new File("test.txt")))
    }

    // Tests supplémentaires pour la lecture de fichiers
    // (nécessitera des fichiers de test réels ou des mocks)
  }
}
