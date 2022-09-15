package fr.inrae.metabolomics.p2m2.builder

/**
 *
 * @param numScan
 * @param indexIsotopeInSpectrum
 */
case class PeakIdentification(
                               numScan : Int,
                               indexIsotopeInSpectrum : Seq[Int]
                             )
