package fr.inrae.metabolomics.p2m2.database

import utest.{TestSuite, Tests, test}

object ChemicalUtilsTest extends TestSuite {
  val tests: Tests = Tests {

    test("composition CH2") {
      assert(ChemicalUtils.composition("CH2").atoms == Map(
        "C" -> 1, "H" -> 2
      ))
    }

    test("composition CHV2") {
      assert(ChemicalUtils.composition("CHV2").atoms == Map(
        "C" -> 1, "H"-> 1, "V" -> 2
      ))
    }

    test("mz C") {
      assert(ChemicalUtils.composition("C").mz == ChemicalUtils.massAtom("C"))
    }

    test("mz CH") {
      assert(ChemicalUtils.composition("CH").mz == ChemicalUtils.massAtom("C")+ChemicalUtils.massAtom("H"))
    }

  }
}
