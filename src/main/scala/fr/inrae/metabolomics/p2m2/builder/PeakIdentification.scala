package fr.inrae.metabolomics.p2m2.builder
import upickle.default._
object PeakIdentification {
  implicit val rw: ReadWriter[PeakIdentification] = macroRW
}

/**
 *
 * @param numScan
 * @param indexIsotopeInSpectrum
 */
case class PeakIdentification(
                               numScan : Int = -1, /* scan on mzxml*/
                               idxPeaks : Seq[Int]= Seq(), /* peaks index inside the spectrum */
                               peaks : Seq[Peak] = Seq(),
                               rt : Double = 0.0 /* Retention time */
                             )
