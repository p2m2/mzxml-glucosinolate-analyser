package fr.inrae.metabolomics.p2m2.builder.GLSRelatedDiagnostic

case object GLSRelatedDiagnostic {
  object NLs extends Enumeration {
    type NLs = Value
    val
    gluconolactone,
    sulfureTrioxide,
    anhydroglucose,
    thioglucose_s03,
    thioglucose,
    glucosinolate_223 = Value
  }

  case class NeutralLosses(name : GLSRelatedDiagnostic.NLs.Value,distance: Double)

  def nls() : Seq[NeutralLosses] = {
    Seq(
      NeutralLosses(NLs.gluconolactone,178.0),
      NeutralLosses(NLs.sulfureTrioxide,80.0),
      NeutralLosses(NLs.anhydroglucose,162.0),
      NeutralLosses(NLs.thioglucose_s03,242.0),
      NeutralLosses(NLs.thioglucose,196.0),
      NeutralLosses(NLs.glucosinolate_223,223.0),
    )
  }

  object DIs extends Enumeration {
    type Fragments = Value
    val
    `C6H11O9S_259`,
    `C6H11O8S2_275`,
    `C6H11NOXSY_291`,
    `C6H9NO8S_241`,
    `C6H11O7S_227`
    = Value
  }

  case class DaughterIons(name : GLSRelatedDiagnostic.DIs.Value, distance: Double)

  def dis() : Seq[DaughterIons] = Seq(
    DaughterIons(DIs.`C6H11O9S_259`,259),
    DaughterIons(DIs.`C6H11O8S2_275`,275),
    DaughterIons(DIs.`C6H11NOXSY_291`,291),
    DaughterIons(DIs.`C6H9NO8S_241`,241),
    DaughterIons(DIs.`C6H11O7S_227`,227),
  )
}



