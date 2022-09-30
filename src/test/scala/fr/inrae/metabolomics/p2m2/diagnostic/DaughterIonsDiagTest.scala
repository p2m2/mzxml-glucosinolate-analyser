package fr.inrae.metabolomics.p2m2.diagnostic
import fr.inrae.metabolomics.p2m2.builder.ScanLoader
import utest.{TestSuite, Tests, test}

import java.io.File

object DaughterIonsDiagTest extends TestSuite {
  val tests : Tests = Tests {

    test("test") {
      val v = ScanLoader.read(new File(getClass.getResource("/20181018-037.mzXML").getPath))
      val v2 = {
        (ScanLoader.getScanIdxAndSpectrumM0M2WithDelta(
          v._1,
          v._2,
          None, // RT start
          None, // RT end
          thresholdAbundanceM0Filter = 0.1, intensityFilter = 0,
          precision = 0.01,
          deltaMOM2 = 1.996
        ))
      }
      println("=============================================n peaks=",v2.size)
      /*
          Frequence des Ions dans les scans de la liste des peak selectionné avec le delta
       */

      v2.map(
       p => DaughterIonsDiag.getPeaksWithIntensitiesNoNull(v._1, v._2,p)
      ).foldLeft(Map[Int,Int]())(
        (acc : Map[Int,Int],v : Seq[Int]) => {
          v.map( p2 => acc.get(p2) match {
              case Some(s) => (p2 -> (s + 1))
              case None => (p2 -> 1)
          }).toMap
        }
      ).toSeq.sortBy(_._2)
    }

  }
}