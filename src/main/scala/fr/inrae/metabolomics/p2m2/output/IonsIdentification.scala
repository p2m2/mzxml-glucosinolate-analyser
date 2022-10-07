package fr.inrae.metabolomics.p2m2.output
import upickle.default._

object IonsIdentification {
  implicit val rw: ReadWriter[IonsIdentification] = macroRW
}
case class IonsIdentification(
                                        mz : Seq[Double],
                                        intensity : Seq[Double],
                                        abundance : Seq[Double],
                                        rt :Double,
                                        neutralLosses : Map[String,Option[Double]],
                                        daughterIons : Map[String,Option[Double]]
                                       ) {
  def scoreIdentification: Int = neutralLosses.values.flatten.size + daughterIons.values.flatten.size
}
