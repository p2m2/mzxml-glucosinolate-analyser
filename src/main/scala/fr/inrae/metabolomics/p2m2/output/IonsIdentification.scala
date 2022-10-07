package fr.inrae.metabolomics.p2m2.output
import fr.inrae.metabolomics.p2m2.builder.PeakIdentification
import upickle.default._

object IonsIdentification {
  implicit val rw: ReadWriter[IonsIdentification] = macroRW
}
case class IonsIdentification(ion : PeakIdentification,
                              neutralLosses : Map[String,Option[Double]],
                              daughterIons : Map[String,Option[Double]]) {
  def scoreIdentification: Int = neutralLosses.values.flatten.size + daughterIons.values.flatten.size
}
