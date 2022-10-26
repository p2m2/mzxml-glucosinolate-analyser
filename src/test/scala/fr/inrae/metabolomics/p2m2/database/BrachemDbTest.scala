package fr.inrae.metabolomics.p2m2.database

import utest.{TestSuite, Tests, test}

object BrachemDbTest extends TestSuite {
  val tests : Tests = Tests {

    test("brachemdb") {
        BraChemDb.getEntries(481.07).foreach(
          e => println(e)
        )
    }

  }
}
