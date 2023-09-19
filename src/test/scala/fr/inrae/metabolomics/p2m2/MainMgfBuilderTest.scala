package fr.inrae.metabolomics.p2m2
import scala.util.Try
import utest.{TestSuite, Tests, test}

object MainMgfBuilderTest extends TestSuite {

  val tests: Tests = Tests {
    test("main") {
    /**
     * Generation of dump
     */
    fr.inrae.metabolomics.p2m2.MainDetection.main(Array(
      "-s","1.0",
        "-e","5.5",
        "-i","1000",
        "-p","500",
        "-w","0.7",
        "-m","100",
        "-t","0.01",
      "-o", "./test",
      getClass.getResource("/20181018-037.mzXML").getPath))
    fr.inrae.metabolomics.p2m2.MainMgfBuilder.main(Array(
      "-n","5000.0",
      "-m","0.1",
      "-d","10000",
      "-p","2",
      "-q", "0",
      "-r", "1",
      "./test",
    ))
    }

    test("round") {
      assert(fr.inrae.metabolomics.p2m2.MainMgfBuilder.round(4.44,1)==4.4)
    }

    test("rtInMs") {
      assert(fr.inrae.metabolomics.p2m2.MainMgfBuilder.rtInMs(4.0)==4.0*1000*60)
    }

    test("error config") {
      assert(Try(fr.inrae.metabolomics.p2m2.MainMgfBuilder.main(Array(
      "-nnnn","500.0"))).isFailure)
    }
    
  }

}
