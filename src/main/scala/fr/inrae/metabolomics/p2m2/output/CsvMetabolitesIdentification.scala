package fr.inrae.metabolomics.p2m2.output

case class CsvMetabolitesIdentification(
                                        mz : Seq[Double],
                                        intensity : Seq[Double],
                                        abundance : Seq[Double],
                                        rt :Double,
                                        neutralLosses : Map[String,Option[Double]],
                                        daughterIons : Map[String,Option[Double]]
                                       )
