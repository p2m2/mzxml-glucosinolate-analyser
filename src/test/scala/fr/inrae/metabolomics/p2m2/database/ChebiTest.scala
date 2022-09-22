package fr.inrae.metabolomics.p2m2.database

import utest.{TestSuite, Tests, test}

object ChebiTest extends TestSuite {
  val tests = Tests {

    test("chebi") {
        Chebi.getEntries(481.07).foreach(
          e => println(e("MONOISOTOPIC MASS"),e("NAME"),e("ID"))
        )
    }

  }
}
