package fr.inrae.metabolomics.p2m2.output
import fr.inrae.metabolomics.p2m2.builder.PeakIdentification
import upickle.default._

object IonsIdentification {
  implicit val rw: ReadWriter[IonsIdentification] = macroRW
}
case class IonsIdentification(pathFile : String,
                              ion : PeakIdentification,
                              neutralLosses : Map[String,Option[(String,Double,Double)]], // mz,abundance
                              daughterIons : Map[String,Option[(String,Double,Double)]] // mz,abundance
                             ) {
  def scoreIdentification: Int = neutralLosses.values.flatten.size + daughterIons.values.flatten.size
}
