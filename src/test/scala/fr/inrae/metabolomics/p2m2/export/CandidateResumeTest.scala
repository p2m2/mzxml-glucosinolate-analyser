package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.builder.{Peak, PeakIdentification}
import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import utest.{TestSuite, Tests, test}

import java.io.File
import scala.collection.immutable.HashMap
import scala.collection.parallel.CollectionConverters._
import scala.io.Source

object CandidateResumeTest extends TestSuite {
  
  val l = List((386.05,List(IonsIdentification(PeakIdentification(2141,List(857, 914, 928, 943),
    List(Peak(0,1930177.25,0.9643518601788234,386.0549621582031), 
      Peak(1,204095.078125,0.10196963426192389,387.06005859375), 
      Peak(2,144970.34375,0.07242983548069275,388.0517272949219), Peak(3,15609.8525390625,0.007798967858087671,389.0559387207031)),14.4804),
    HashMap("sulfureTrioxide" -> Some(306.1775817871094), "anhydroglucose" -> Some(223.94546508789062), 
      "glucosinolate_223" -> Some(163.12960815429688), "gluconolactone" -> Some(208.00234985351562), "thioglucose" -> Some(190.1595458984375), 
      "thioglucose_s03" -> Some(144.107421875), "RCNO4S2-" -> Some(223.94546508789062)),
    HashMap("HO4S2-_128" -> None, "C2H3O5S-_138" -> Some(139.05892944335938), "C6H11O9S_259" -> Some(259.0452880859375), "C2H4O5NS-_153" -> None, 
      "C6H11O5S-_195" -> Some(195.15640258789062), "HSO4-_97" -> None, "C6H10O8S-_241" -> Some(242.034423828125),
      "C2H2O4S-_135" -> None, "C2H3OS-" -> None, "C6H11O7S-_227" -> Some(227.11065673828125), "C6H11O8S2_275" -> None, 
      "C6H9NO8S_241" -> None, "SO4-_95" -> None, "C6H11O2-_153" -> Some(163.12960815429688))))), 
    (386.06,List(IonsIdentification(PeakIdentification(2133,List(1227, 1260, 1275, 1295),
      List(Peak(0,517472.5,0.9747179112992527,386.0578308105469), Peak(1,52580.5703125,0.09904144407659053,387.06005859375), 
        Peak(2,39614.6796875,0.07461872436079005,388.0517272949219), Peak(3,4766.6884765625,0.008978596226252055,389.0559387207031)),14.429),
      HashMap("sulfureTrioxide" -> Some(306.14111328125), "anhydroglucose" -> None, "glucosinolate_223" -> Some(163.0186767578125),
        "gluconolactone" -> Some(208.1163787841797), "thioglucose" -> Some(189.99575805664062), "thioglucose_s03" -> Some(144.74136352539062), 
        "RCNO4S2-" -> None),HashMap("HO4S2-_128" -> None, "C2H3O5S-_138" -> Some(138.922607421875), "C6H11O9S_259" -> Some(259.0414123535156), 
        "C2H4O5NS-_153" -> None, "C6H11O5S-_195" -> Some(195.07904052734375), "HSO4-_97" -> None, "C6H10O8S-_241" -> None, "C2H2O4S-_135" -> None,
        "C2H3OS-" -> None, "C6H11O7S-_227" -> Some(227.2979736328125), "C6H11O8S2_275" -> Some(275.0428466796875), "C6H9NO8S_241" -> Some(240.85574340820312),
        "SO4-_95" -> None, "C6H11O2-_153 " -> Some(163.0186767578125))))), (438.05,List(IonsIdentification(PeakIdentification(2053,List(1241, 1248, 1270, 1281),
      List(Peak(0,17944.623046875,0.4962111944750583,438.0538635253906), Peak(1,2235.7412109375,0.061823523064175406,439.0567626953125), 
        Peak(2,1006.3428955078125,0.02782775703490646,440.045654296875), Peak(3,0.0,0.0,440.9154052734375)),13.876433333333333),
      HashMap("sulfureTrioxide" -> Some(358.0008239746094), "anhydroglucose" -> Some(275.7400207519531), "glucosinolate_223" -> Some(215.0611572265625),
        "gluconolactone" -> Some(260.8398742675781), "thioglucose" -> Some(242.87152099609375), "thioglucose_s03" -> Some(196.19830322265625),
        "RCNO4S2-" -> Some(275.7400207519531)),HashMap("HO4S2-_128" -> None, "C2H3O5S-_138" -> None, "C6H11O9S_259" -> Some(258.9526062011719),
        "C2H4O5NS-_153" -> Some(153.9756622314453), "C6H11O5S-_195" -> Some(195.00088500976562), "HSO4-_97" -> None, "C6H10O8S-_241" -> Some(242.07217407226562),
        "C2H2O4S-_135" -> Some(136.0654296875), "C2H3OS-" -> None, "C6H11O7S-_227" -> Some(227.0313720703125), "C6H11O8S2_275" -> Some(275.0786437988281),
        "C6H9NO8S_241" -> Some(241.05459594726562), "SO4-_95" -> None, "C6H11O2-_153 " -> Some(162.88873291015625))), IonsIdentification(PeakIdentification(2057,
      List(1347, 1354, 1368, 1378),List(Peak(0,20862.939453125,0.6093894869292313,438.0504455566406), 
        Peak(1,2582.57666015625,0.07543496300816283,439.0567932128906), Peak(2,1711.4429931640625,0.049989857366750774,440.0491638183594), 
        Peak(3,0.0,0.0,440.9119567871094)),13.904033333333333),HashMap("sulfureTrioxide" -> Some(358.0040283203125), "anhydroglucose" -> Some(275.21978759765625),
      "glucosinolate_223" -> Some(215.0611572265625), "gluconolactone" -> Some(260.0679626464844), "thioglucose" -> Some(242.9249267578125),
      "thioglucose_s03" -> Some(196.21749877929688), "RCNO4S2-" -> Some(275.21978759765625)),HashMap("HO4S2-_128" -> None, "C2H3O5S-_138" -> Some(138.9679718017578),
      "C6H11O9S_259" -> Some(259.0535888671875), "C2H4O5NS-_153" -> Some(153.9756622314453), "C6H11O5S-_195" -> None, "HSO4-_97" -> None, 
      "C6H10O8S-_241" -> Some(242.1724853515625), "C2H2O4S-_135" -> Some(136.0654296875), "C2H3OS-" -> None, "C6H11O7S-_227" -> Some(227.0313720703125),
      "C6H11O8S2_275" -> Some(275.21978759765625), "C6H9NO8S_241" -> Some(241.05459594726562), "SO4-_95" -> None, "C6H11O2-_153 " -> None)),
      IonsIdentification(PeakIdentification(2061,List(1363, 1371, 1395, 1405),List(Peak(0,17944.580078125,0.4780661797976846,438.0505065917969), 
        Peak(1,2089.769775390625,0.05567409484246067,439.0533752441406), Peak(2,769.8866577148438,0.020510748745779545,440.0457458496094), 
        Peak(3,0.0,0.0,440.9120178222656)),13.931766666666666),HashMap("sulfureTrioxide" -> Some(358.0997619628906), "anhydroglucose" -> Some(275.21978759765625), "glucosinolate_223" -> None, "gluconolactone" -> Some(260.0931701660156),
        "thioglucose" -> Some(242.90115356445312), "thioglucose_s03" -> Some(196.21749877929688), "RCNO4S2-" -> Some(275.21978759765625)),HashMap("HO4S2-_128" -> None, "C2H3O5S-_138" -> Some(138.9679718017578), "C6H11O9S_259" -> Some(259.0535888671875), "C2H4O5NS-_153" -> None, "C6H11O5S-_195" -> Some(195.0664825439453), "HSO4-_97" -> None, "C6H10O8S-_241" -> Some(242.1724853515625),
        "C2H2O4S-_135" -> Some(135.9614715576172), "C2H3OS-" -> None,
        "C6H11O7S-_227" -> Some(227.0274658203125), "C6H11O8S2_275" -> Some(275.21978759765625), "C6H9NO8S_241" -> None,
        "SO4-_95" -> None, "C6H11O2-_153 " -> None)))), (479.04,List(IonsIdentification(PeakIdentification(1961,List(1484, 1492, 1500, 1508),List(Peak(0,87445.59375,0.7964271679196497,479.0405578613281), Peak(1,13274.5263671875,0.12090024193007273,480.04443359375), Peak(2,5956.67138671875,0.054251503355507,481.0355224609375), Peak(3,1379.9561767578125,0.012568210044413497,482.0416564941406)),13.244916666666667),HashMap("sulfureTrioxide" -> Some(399.7521057128906), "anhydroglucose" -> Some(316.79534912109375), "glucosinolate_223" -> Some(256.0777282714844), "gluconolactone" -> Some(301.02337646484375),
      "thioglucose" -> Some(282.9837646484375), "thioglucose_s03" -> Some(237.2308807373047), "RCNO4S2-" -> Some(316.79534912109375)),HashMap("HO4S2-_128" -> None, "C2H3O5S-_138" -> Some(138.7873077392578), "C6H11O9S_259" -> Some(259.0037536621094), "C2H4O5NS-_153" -> None,
      "C6H11O5S-_195" -> Some(195.0782012939453), "HSO4-_97" -> None, "C6H10O8S-_241" -> None, "C2H2O4S-_135" -> None, "C2H3OS-" -> None, "C6H11O7S-_227" -> None, "C6H11O8S2_275" -> Some(275.0330505371094), "C6H9NO8S_241" -> Some(241.14169311523438), "SO4-_95" -> None, "C6H11O2-_153 " -> None)), IonsIdentification(PeakIdentification(1969,List(1458, 1473, 1480, 1488),List(Peak(0,189756.09375,0.9962123163456589,479.04022216796875), Peak(1,29791.912109375,0.1564064119592826,480.0440979003906), Peak(2,13140.384765625,0.06898652310098204,481.0351867675781), Peak(3,2970.91064453125,0.015597168535434455,482.0373229980469)),13.2976),
      HashMap("sulfureTrioxide" -> Some(399.53790283203125), "anhydroglucose" -> Some(317.28424072265625),
      "glucosinolate_223" -> Some(256.03729248046875), "gluconolactone" -> Some(301.15447998046875), "thioglucose" -> Some(283.74066162109375),
        "thioglucose_s03" -> Some(237.08123779296875), "RCNO4S2-" -> Some(316.27099609375)),HashMap("HO4S2-_128" -> None, "C2H3O5S-_138" -> None, "C6H11O9S_259" -> Some(259.1313781738281), "C2H4O5NS-_153" -> None, "C6H11O5S-_195" -> Some(194.83314514160156), "HSO4-_97" -> None,
      "C6H10O8S-_241" -> None, "C2H2O4S-_135" -> None, "C2H3OS-" -> None, "C6H11O7S-_227" -> Some(227.1016845703125), "C6H11O8S2_275" -> Some(275.0069274902344), "C6H9NO8S_241" -> Some(241.1009979248047), "SO4-_95" -> None, "C6H11O2-_153 " -> None)))))
  
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
