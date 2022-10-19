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
      assert(ChemicalUtils.composition("C").monoIsotopicMass == ChemicalUtils.massAtom("C"))
    }

    test("mz C2") {
      assert(ChemicalUtils.composition("C2").monoIsotopicMass == ChemicalUtils.massAtom("C")*2)
    }

    test("mz CH") {
      assert(ChemicalUtils.composition("CH").monoIsotopicMass == ChemicalUtils.massAtom("C")+ChemicalUtils.massAtom("H"))
    }

    test("probabilityIsotope 1 - C") {
      assert(ChemicalUtils.composition("C").probabilityIsotope(1,1) == ChemicalUtils.abundanceIsotope("C")(1))
    }

    test("probabilityIsotope 1 - CH2") {
      assert(ChemicalUtils.composition("CH2").probabilityIsotope(1, 1) ==
        (ChemicalUtils.abundanceIsotope("C")(1)+ChemicalUtils.abundanceIsotope("H")(1)*2))
    }

    test("probabilityIsotope 2 - C - not exist ") {
      assert(ChemicalUtils.composition("C").probabilityIsotope(2, 1) == 0.0)
    }

    test("probabilityIsotope 1 - C - existe 2 times is null ") {
      assert(ChemicalUtils.composition("C").probabilityIsotope(1, 2) == 0.0)
    }

    test("probabilityIsotope 2 - CO") {
      assert(ChemicalUtils.composition("CO").probabilityIsotope(2, 1) == ChemicalUtils.abundanceIsotope("O")(2))
    }

    test("isotopicCluster C") {
      assert(ChemicalUtils.composition("C").isotopicCluster == Seq(ChemicalUtils.abundanceIsotope("C")(1), 0.0, 0.0, 0.0, 0.0, 0.0))
    }

    test("isotopicCluster CH") {
      assert(ChemicalUtils.composition("CH").isotopicCluster ==
        Seq(ChemicalUtils.abundanceIsotope("C")(1)+ChemicalUtils.abundanceIsotope("H")(1), 0.0, 0.0, 0.0, 0.0, 0.0))
    }

    /**
     * check with https://www.chemcalc.org/
     */
    test("isotopicCluster CO") {
      ChemicalUtils.composition("C12H21O10S3N").isotopicCluster
    }

    test("correlation") {
      assert(ChemicalUtils.correlation("C11H21O10S3N", Seq(1.0, 0.015, 0.01657)) > 0.99)
    }

    test("correlation") {
      assert(ChemicalUtils.correlation("C11H21O10S3N",Seq(1.0,0.015,16.57))< 0.5)
    }
  }
}
