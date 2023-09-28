package fr.inrae.metabolomics.p2m2.config

import utest.{TestSuite, Tests,test}

object ConfigReaderTest extends TestSuite {
  val tests: Tests = Tests {
    val config =
      """
        |{
        |  "Glucosinolate" : {
        |    "deltaMp0Mp2" : 1.996,
        |    "numberCarbonMin" : 2,
        |    "numberCarbonMax" : 25,
        |    "numberSulfurMin" : 2,
        |    "numberSulfurMax" : 5,
        |    "minMzCoreStructure" : 0,
        |    "minM0Abundance" : 0.1,
        |    "neutralLoss" :
        |      {
        |        "gluconolactone" : 178.0
        |      },
        |    "daughterIons" : {
        |      "C6H11O9S_259" : 259.0
        |    },
        |    "databaseReference" : {
        |    "methyl" : {
        |        "formula" : "C8H14O9S2N1",
        |        "name" : "glucoapparin"
        |      },
        |      "n-pentyl" : {
        |        "formula" : "C12H22O9S2N1"
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
    test("init") {
      val conf = ConfigReader.read(config)
      assert(conf.metabolites == Seq("Glucosinolate"))
      assert(conf.deltaMp0Mp2("Glucosinolate") == 1.996)
      assert(conf.numberSulfurMin("Glucosinolate") == 2.0)
      assert(conf.neutralLoss("Glucosinolate") == Map("gluconolactone" -> 178.0))
      assert(conf.daughterIons("Glucosinolate") == Map("C6H11O9S_259" -> 259.0))


    }
  }
}
