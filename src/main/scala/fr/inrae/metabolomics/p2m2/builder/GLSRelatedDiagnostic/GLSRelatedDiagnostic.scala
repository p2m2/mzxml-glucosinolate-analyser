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

  object Fragments extends Enumeration {
    type Fragments = Value
    val
    f1 = Value
  }

  case class DaughterIons(name : GLSRelatedDiagnostic.Fragments.Value,distance: Double)

  def fragments() : Seq[DaughterIons] = Seq()
}



