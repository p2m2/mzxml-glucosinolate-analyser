package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.`export`.CsvIonsIdentificationFileTest.getClass
import fr.inrae.metabolomics.p2m2.builder.ScanLoader
import fr.inrae.metabolomics.p2m2.config.ConfigReader
import utest.{TestSuite, Tests, test}
import scala.util.{Try,Success,Failure}
import java.io.File

object IonsIdentificationFileTest extends TestSuite {
  val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))

  val tests: Tests = Tests {
    test("instance") {
      val f = File.createTempFile("pref","suf")
      IonsIdentificationFile.save(Seq(),"",ConfigReader(Map(),Map(),Map(),Map()),f)
      Try(IonsIdentificationFile.load(f)) match {
        case Success(value) => assert( value == (Seq(),"",ConfigReader(Map(),Map(),Map(),Map())))
        case Failure(_) => assert(false)
      }
    }
  }
}
