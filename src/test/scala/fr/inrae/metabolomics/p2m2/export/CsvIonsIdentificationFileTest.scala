package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.builder.{PeakIdentification, ScanLoader}
import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import utest.{TestSuite, Tests, test}

import java.io.File
import scala.io.Source

object CsvIonsIdentificationFileTest extends TestSuite {
  val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

  val tests: Tests = Tests {
    test("instance") {
      val confJson = ConfigReader.read(
         Source.fromInputStream(
          getClass.getResource("/default.json")
            .openStream()).getLines().mkString)

      val f = File.createTempFile("test",".csv")

      CsvIonsIdentificationFile.build(
        Seq(
          IonsIdentification(
            PeakIdentification(),
            neutralLosses = Map(),
            daughterIons = Map()
          )
        ),
        familyMetabolite =  "Glucosinolate",
        configJson = confJson,
        out = f
      )
    }
  }

  }
