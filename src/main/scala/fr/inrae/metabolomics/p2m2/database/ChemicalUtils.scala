package fr.inrae.metabolomics.p2m2.database

case object ChemicalUtils {
  val massProton: Double = 1.007276
  val massNeutron: Double = 1.008665
  val massElectron: Double = 0.0000549

  val abundanceCarbon13 : Double = 1.1  // 1.08
  val abundanceDeuterium : Double = 0.015 //0.012
  val abundanceOxygen17 : Double = 0.04
  val abundanceOxygen18 : Double = 0.20
  val abundanceSulfur33 : Double = 0.789
  val abundanceSulfur34 : Double = 4.44
  val abundanceNitrogen15 : Double = 0.37

  val massAtom : Map[String, Double] = Map(
    "C" ->  12.0107,
    "12C" ->  12.0107,
    "13C" -> 13.0033,
    "H" -> 1.007825,
    "2H" -> 2.0140,
    "O" -> 15.99499,
    "16O" -> 15.99499,
    "17O" -> 16.9991,
    "18O" -> 17.9992,
    "S" -> 31.9721,
    "32S" -> 31.9721,
    "33S" -> 32.9715,
    "34S" -> 33.9679,
    "N" -> 14.00307,
    "14N" -> 14.00307,
    "15N" -> 15.0001
    )

  /**
   * Molecule composition
   * @param atoms Atomic and number of element
   */
  case class MoleculeComposition(atoms : Map[String,Int] ) {
    def mz: Double = atoms.foldLeft(0.0) {
      case (sum,at -> nb) => sum + nb * massAtom.getOrElse(at,0.0)
    }
  }

  /**
   * Decompose a molecule into atomic element
   * @param formula
   * @return
   */
  def composition(formula : String): MoleculeComposition = {
    MoleculeComposition("(\\w)(\\d*)".r.findAllMatchIn(formula).map  {
      sol =>
      sol.group(2) match {
        case "" => sol.group(1) -> 1
        case _ =>  sol.group(1) -> sol.group(2).toInt
      }
    }.toSeq.toMap)
  }

}
