package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.builder.{IonsIdentificationBuilder, PeakIdentification, ScanLoader}
import fr.inrae.metabolomics.p2m2.config.ConfigReader
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}
import utest.{TestSuite, Tests, test}

import java.io.File
import scala.io.Source

object CsvIonsIdentificationFileTest extends TestSuite {
  val v: (MZXMLFile, MZXMLIndex) = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))
  val v2: Seq[PeakIdentification] =
    ScanLoader.selectEligibleIons(
      v._1,
      v._2,
      Some(6.6), // RT start
      Some(7.0), // RT end
      noiseIntensity = 0.1,
      nbCarbonMin = 4.0,
      nbCarbonMax = 20.0,
      nbSulfurMin = 2.0,
      nbSulfurMax = 5.0,
      minMzCoreStructure = 0.01,
      precisionDeltaM0M2 = 0.001,
      deltaMOM2 = 1.996
    )

    val m: IonsIdentificationBuilder =
      ScanLoader.filterOverRepresentedPeak(
        v._1,
        v._2,
        v2,
        noiseIntensity = 100.0,
        800,
        Seq(),
        Seq(),

      )

    val tests: Tests = Tests {
      test("instance") {
        val confJson = ConfigReader.read(
          Source.fromInputStream(
            getClass.getResource("/default.json")
              .openStream()).getLines().mkString)

        val f = File.createTempFile("test", ".csv")

        CsvIonsIdentificationFile.build(
          m.findDiagnosticIonsAndNeutralLosses(0.1, 200),
          familyMetabolite = "Glucosinolate",
          configJson = confJson,
          out = f
        )
      }
    }
}
