package fr.inrae.metabolomics.p2m2

import utest.{TestSuite, Tests, test}

import java.io.{File, FileOutputStream}
import scala.util.{Failure, Success, Try}

object MainDetectionTest extends TestSuite {

  val tests: Tests = Tests {

    test("Main with args") {
      Try(fr.inrae.metabolomics.p2m2.MainDetection.main(Array(
        "-s","1.0",
        "-e","1.5",
        "-i","1000",
        "-p","500",
        "-w","0.7",
        "-m","100",
        "-t","0.01",
        "-o","test.csv",
        getClass.getResource("/20181018-037.mzXML").getPath))) match {
        case Success(_) =>
          new File("test.csv").delete(); assert(true)
          assert(
            Try(fr.inrae.metabolomics.p2m2.MainDetection.main(
              Array(getClass.getResource("./20181018-037_Glucosinolate").getPath))).isFailure)
        case Failure(e) => println(e); assert(false)
      }
    }

    test("Main without args") {
      assert(Try(fr.inrae.metabolomics.p2m2.MainDetection.main(Array(
        "-s", "10.0",
        "-e", "13.0",
        getClass.getResource("/20181018-037.mzXML").getPath))).isSuccess)
    }

    test("Main failure") {
      assert(Try(fr.inrae.metabolomics.p2m2.MainDetection.main(Array(""))).isFailure)
    }

    test("Main 2") {
      Try(fr.inrae.metabolomics.p2m2.MainDetection.main(Array(
        "-j",getClass.getResource("/default.json").getPath,
        "-s", "1.0",
        "-e", "1.5",
        "-o", "test.csv",
        getClass.getResource("/20181018-037.mzXML").getPath))) match {
        case Success(_) => new File("test.csv").delete(); assert(true)
        case Failure(e) => println(e); assert(false)
      }
    }

  }

}
