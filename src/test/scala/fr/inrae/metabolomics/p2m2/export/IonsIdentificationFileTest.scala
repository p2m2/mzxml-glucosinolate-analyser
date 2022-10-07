package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.builder.ScanLoader
import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import utest.{TestSuite, Tests, test}

import java.io.File
import scala.io.Source

object IonsIdentificationFileTest extends TestSuite {
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
            mz = Seq(1.0, 1.0),
            intensity = Seq(1.0, 1.0),
            abundance = Seq(1.0, 1.0),
            rt = 0.1,
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
