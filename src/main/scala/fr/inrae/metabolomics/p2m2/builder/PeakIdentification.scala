package fr.inrae.metabolomics.p2m2.builder

/**
 *
 * @param numScan
 * @param indexIsotopeInSpectrum
 */
case class PeakIdentification(
                               numScan : Int, /* scan on mzxml*/
                               idxPeaks : Seq[Int], /* peaks index inside the spectrum */
                               peaks : Seq[Peak],
                               rt : Double /* Retention time */
                             )
