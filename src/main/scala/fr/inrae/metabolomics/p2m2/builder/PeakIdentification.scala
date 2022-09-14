package fr.inrae.metabolomics.p2m2.builder

/**
 *
 * @param numScan
 * @param indexFirstIsotopeInSpectrum
 * @param nbIsotope
 */
case class PeakIdentification(
                               numScan : Int,
                               indexFirstIsotopeInSpectrum : Int,
                               mz: Seq[Double] // Others isotopes are on the right of the numSpectrum
                             )
