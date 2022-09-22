package fr.inrae.metabolomics.p2m2.database

import utest.{TestSuite, Tests, test}

object BrassicaAccessionTest  extends TestSuite {
  val tests: Tests = Tests {

    test("brassica accession") {
      BrassicaAccession.getEntries(358.027).foreach(
        e => println(e("MONOISOTOPIC MASS"), e("NAME"), e("ID"))
      )
    }

  }
}
