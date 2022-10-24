package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import utest.{TestSuite, Tests, test}

import java.io.File
import scala.collection.parallel.CollectionConverters._
import scala.io.Source

object CandidateResumeTest extends TestSuite {
  
  val l = Seq((123.12,Seq()))
  val tests: Tests = Tests {
    test("instance") {
      val confJson = ConfigReader.read(
        Source.fromInputStream(
          getClass.getResource("/default.json")
            .openStream()).getLines().mkString)

      val f = File.createTempFile("test", ".csv")
      println(f.getPath)
      CandidateResume.build(
        l.map { case (r,l2) => (r,l2.map((_,"")).par) },
        familyMetabolite = "Glucosinolate",
        configJson = confJson,
        out = f
      )

      println(Source.fromFile(f).getLines().mkString("\n"))
    }
  }
}
