package fr.inrae.metabolomics.p2m2.fileformats

import utest.{TestSuite, Tests, test}

import java.io.File

object SpectrumFileReaderFactoryTest extends TestSuite {

  val tests: Tests = Tests {

    test("return appropriate reader for mzXML files") {
      val factory = SpectrumFileReaderFactory
      val reader = factory.getReaderForFile(new File("test.mzxml"))
      assert(reader.isDefined)
      assert(reader.get.isInstanceOf[MzXmlReader])
    }

    test("return appropriate reader for mzML files") {
      val factory = SpectrumFileReaderFactory
      val reader = factory.getReaderForFile(new File("test.mzml"))
      assert(reader.isDefined)
      assert(reader.get.isInstanceOf[MzMLReader])
    }

    test("return appropriate reader for xcms files") {
      val factory = SpectrumFileReaderFactory
      val reader = factory.getReaderForFile(new File("test.xcms"))
      assert(reader.isDefined)
      assert(reader.get.isInstanceOf[XcmsReader])
    }

    test("return None for unsupported file types") {
      val factory = SpectrumFileReaderFactory
      val reader = factory.getReaderForFile(new File("test.txt"))
      assert(reader.isEmpty)
    }
  }
}
