package fr.inrae.metabolomics.p2m2.database

import org.openscience.cdk.interfaces.IBond
import org.openscience.cdk.{Atom, AtomContainer}

case object ChemicalUtils {
  val massProton: Double = 1.007276
  val massNeutron: Double = 1.008665
  val massElectron: Double = 0.0000549

  val massCarbon12: Double = 12.0107
  val massCarbon13: Double = 13.0033
  val abundanceCarbon13 : Double = 1.1  // 1.08

  val massHydrogen: Double = 1.007825
  val massDeuterium: Double = 2.0140
  val abundanceDeuterium : Double = 0.015 //0.012

  val massOxygen16: Double = 15.99499
  val massOxygen17: Double = 16.9991
  val abundanceOxygen17 : Double = 0.04
  val massOxygen18: Double = 17.9992
  val abundanceOxygen18 : Double = 0.20

  val massSulfur32: Double = 31.9721
  val massSulfur33: Double = 32.9715
  val abundanceSulfur33 : Double = 0.789
  val massSulfur34: Double = 33.9679
  val abundanceSulfur34 : Double = 4.44

  val massNitrogen14: Double = 14.00307
  val massNitrogen15: Double = 15.0001
  val abundanceNitrogen15 : Double = 0.37

  def mz(formula : String ) = {

    println(formula)

    val mol = new AtomContainer

    /* Glucose  C6H12O6 */
    mol.addAtom(new Atom("C")) // 0

    mol.addAtom(new Atom("C")) // 0
    mol.addAtom(new Atom("C")) // 1
    mol.addAtom(new Atom("C")) // 2
    mol.addAtom(new Atom("C")) // 3
    mol.addAtom(new Atom("C")) // 4
    mol.addAtom(new Atom("C")) // 5
    mol.addAtom(new Atom("C")) // 6
    mol.addAtom(new Atom("C")) // 7
    mol.addAtom(new Atom("C")) // 8
    mol.addAtom(new Atom("C")) // 9
    mol.addAtom(new Atom("C")) // 10


    mol.addBond(0, 1, IBond.Order.SINGLE)
    mol.addBond(1, 2, IBond.Order.SINGLE)
    mol.addBond(2, 3, IBond.Order.SINGLE)
    mol.addBond(3, 4, IBond.Order.SINGLE)
    mol.addBond(4, 5, IBond.Order.SINGLE)
    mol.addBond(5, 0, IBond.Order.SINGLE)
    mol.addBond(5, 6, IBond.Order.SINGLE)
    mol.addBond(6, 7, IBond.Order.SINGLE) // 8RingSet

    mol.addBond(7, 8, IBond.Order.SINGLE)
    mol.addBond(8, 9, IBond.Order.SINGLE)
    mol.addBond(9, 0, IBond.Order.SINGLE) // 11

    mol.addBond(5, 10, IBond.Order.SINGLE) // 12
    mol.

  }

}
