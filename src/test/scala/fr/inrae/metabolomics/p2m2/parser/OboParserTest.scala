package fr.inrae.metabolomics.p2m2.parser

import utest.{TestSuite, Tests, test}

object OboParserTest extends TestSuite {


  val tests: Tests = Tests {
    test("parse") {
      val o = OboParser(getClass.getResource("/chebi.obo").getPath)
      println("1:",o.nextHeader(),o.next())
      println("2:",o.nextHeader(),o.next())
      println("3:",o.nextHeader(),o.next())
    }

    test("termFilter") {
      val o = OboParser(getClass.getResource("/chebi_1.obo").getPath)
      val mo = o.termFilter()
      assert(mo.nonEmpty)
      val m = mo.get
      assert( m("id") == "CHEBI:32213")
      assert( m("name") == "((201)Tl)thallium monochloride")
      assert( m("smiles") == "Cl[201Tl]")
      assert( m("charge") == "0")
      assert( m("inchikey") == "GBECUEIQVRDUKB-RYDPDVNUSA-M")
      assert( m("monoisotopicmass") == "235.93966")
      assert( m("mass") == "236.420")
      assert( m("subset") == "3_STAR")
      assert( m("inchi") == "InChI=1S/ClH.Tl/h1H;/q;+1/p-1/i;1-3")
      assert( m("formula") == "Cl[201Tl]")
    }
    test("termFilter regex charge") {
      val o = OboParser(getClass.getResource("/chebi_1.obo").getPath)
      val mo = o.termFilter(chargeRegx = Some("1"))
      assert(mo.isEmpty)
    }
    test("termFilter regex formula") {
      val o = OboParser(getClass.getResource("/chebi_1.obo").getPath)
      val mo = o.termFilter(chargeRegx = Some("U"))
      assert(mo.isEmpty)
    }

    test("next") {
      val o = OboParser(getClass.getResource("/chebi_1.obo").getPath)
      while (o.hasNext) {
        o.termFilter()
      }
    }

    test("bidon") {
      val o = OboParser("/home/ofilangi/Téléchargements/chebi.obo")
      while(o.hasNext) {
       // o.termFilter()
      }
    }
  }
}
