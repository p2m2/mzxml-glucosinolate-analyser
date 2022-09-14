package fr.inrae.metabolomics.p2m2.builder

import utest.{TestSuite, Tests, test}

import java.io.File

object ScanLoaderTest extends TestSuite {
  val tests = Tests {
    val v = ScanLoader.read(new File(getClass.getResource("/test.mzXML").getPath))

    test("getIdentifiedMetaboliteMonoCharged") {
      val v2 = ScanLoader.getScanIdxAndSpectrum3IsotopesWithEqualDelta(v._1,v._2)
      ScanLoader.getIdentifiedMetaboliteMonoCharged(v2)
    }

    test("detectNeutralLoss") {
      val p = List(
        PeakIdentification(1107, 692, List(233.93598079258905, 234.073373358917, 235.92627811976865)),
        PeakIdentification(2583, 135, List(140.10892640034714, 141.70386329740901, 142.09905700269698)),
        PeakIdentification(2121, 476, List(188.93310762787098, 189.62481225036157, 190.92317341305693)),
        PeakIdentification(7407, 353, List(176.86571256811493, 177.0004882914496, 178.85592864484966)),
        PeakIdentification(129, 2453, List(623.8232560311458, 624.8145835367992, 625.8124528515347)),
        PeakIdentification(12595, 424, List(187.94125902905355, 188.81782890010373, 189.9320997844912)),
        PeakIdentification(13249, 532, List(227.89525734682886, 228.90598608758933, 229.88596513993042)),
        PeakIdentification(86, 1822, List(396.8975626191338, 397.8911533176062, 398.8880249995413)),
        PeakIdentification(3525, 443, List(211.92736190753823, 212.91058199483302, 213.91706839249088)),
        PeakIdentification(2505, 338, List(193.44624662680576, 195.0480828142617, 195.43531090871707)))

      p.foreach(x => println(ScanLoader.detectNeutralLoss(v._1, x, 80.0)))
    }

    test("getScanIdxAndSpectrum3IsotopesSulfurContaining") {
      val v2 = ScanLoader.getScanIdxAndSpectrum3IsotopesSulfurContaining(v._1,v._2)
      MetaboliteIdentification(v._1,v._2,v2).getInfos()
      println(v2.slice(0,10))
    }


  }
}
