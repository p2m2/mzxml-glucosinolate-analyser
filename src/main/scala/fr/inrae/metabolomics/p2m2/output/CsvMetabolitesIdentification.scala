package fr.inrae.metabolomics.p2m2.output

import fr.inrae.metabolomics.p2m2.builder.GLSRelatedDiagnostic.GLSRelatedDiagnostic

case class CsvMetabolitesIdentification(
                                        mz : Seq[Double],
                                        intensity : Seq[Double],
                                        abundance : Seq[Double],
                                        rt :Double,
                                        neutralLosses : Map[GLSRelatedDiagnostic.NLs.Value,Option[Double]],
                                        daughterIons : Map[GLSRelatedDiagnostic.DIs.Value,Option[Double]]
                                       )
