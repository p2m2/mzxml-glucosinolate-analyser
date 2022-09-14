package fr.inrae.metabolomics.p2m2.builder

import umich.ms.fileio.filetypes.mzxml.{MZXMLIndex, _}
import umich.ms.datatypes.scan.IScan

import java.io.File
import scala.jdk.CollectionConverters._

case object ScanLoader {

  def read( f : File ) : (MZXMLFile,MZXMLIndex) = {
    val source : MZXMLFile = new MZXMLFile(f.getPath)
    println(source.parseRunInfo())
    // Notice that we use fetchIndex() instead of getIndex().
    // fetchIndex() will either get a cached copy or parse it from
    // disk, if no cache is available. The index will be cached after parsing.
    val index : MZXMLIndex = source.fetchIndex()
    (source,index)
    // The index gives you the scan numbers, on the lowest level you can parse// The index gives you the scan numbers, on the lowest level you can parse
  }

  /*
   * Intensité = abondance relative
   *  Proportionnelle à la
   *  concentration mais aussi à la
   *  capacité d’ionisation de la
   *  molécule
   */

  /**
   * Anani (2) filter => Nb. of charges m/(Z?)
   * get set of spectrum (3 peaks) with a delta according the condition M1-M2 == M2-M3
   */
  def getScanIdxAndSpectrum3IsotopesWithEqualDelta(
                                      source : MZXMLFile,
                                      index : MZXMLIndex,
                                      precision : Double = 0.001

                           ) : Seq[PeakIdentification] = {
    // the file using those numbers. We need the raw scan numbers (the numbers
    // as they're used in the file). The internal scan numbering scheme always
    // renumbers all scans starting from 1 and increasing by 1 consecutively.
    index
      .getMapByRawNum
      .keySet() // The second parameter asks the parser to parse the spectrum along
      .asScala
      .filter( scanNumRaw => source.parseScan(scanNumRaw, true).getMsLevel == 1)
      //.slice(0, 10)
      .flatMap(
        scanNumRaw => {
          // Do something with the scan.
          // Note that some features, like scan.getChildScans() will not work in
          // this case, as there is not enough information to build those
          // relationships.
          val scan: IScan = source.parseScan(scanNumRaw, true)
          val spectrum = scan.fetchSpectrum()
          val mzValues = spectrum.getMZs
          // remove the first one to compute Delta M
          (mzValues.drop(2) zip mzValues.drop(1) zip mzValues)
            .map {  case ((a,b), c) => (a,b,c) }
            .zipWithIndex
            .map {  case ((a,b,c), d) => (a,b,c,d) }
            .filter { case ( v1,v2,v3,_) => ((v1 - v2) - (v2-v3)).abs < precision }
            //.foreach( a => println(a,a._1-a._2,a._2-a._3))
            .map { case (v1,v2,v3,spectrumIdx) =>  PeakIdentification(scanNumRaw.toInt, spectrumIdx, Seq(v1,v2,v3)) }
        }
      ).toSeq
  }

  def getIdentifiedMetaboliteMonoCharged(
                                          peaks : Seq[PeakIdentification],
                                          precision : Double = 0.001
                                        ): Seq[PeakIdentification] = {
    peaks.filter( p => ((p.mz(0)-p.mz(1)).abs - 1.0).abs < precision)
  }


  def getScanIdxAndSpectrum3IsotopesSulfurContaining(
                                      source: MZXMLFile,
                                      index: MZXMLIndex,
                                      precision: Double = 0.001

                                    ): Seq[PeakIdentification] = {

    // the file using those numbers. We need the raw scan numbers (the numbers
    // as they're used in the file). The internal scan numbering scheme always
    // renumbers all scans starting from 1 and increasing by 1 consecutively.
    index
      .getMapByRawNum
      .keySet() // The second parameter asks the parser to parse the spectrum along
      .asScala
      .filter(scanNumRaw => source.parseScan(scanNumRaw, true).getMsLevel == 1)
      //.slice(0, 10)
      .flatMap(
        scanNumRaw => {
          // Do something with the scan.
          // Note that some features, like scan.getChildScans() will not work in
          // this case, as there is not enough information to build those
          // relationships.
          val scan: IScan = source.parseScan(scanNumRaw, true)
          val spectrum = scan.fetchSpectrum()
          val mzValues = spectrum.getMZs
          // remove the first one to compute Delta M
          (mzValues.drop(2) zip mzValues)
            .zipWithIndex
            .map { case ((a, b), c) => (a, b, c) }
            .filter { case (v1, v2, _) => ((v1 - v2) - 1.99).abs < precision }
            .map { case (_, _, spectrumIdx) =>
              PeakIdentification(scanNumRaw.toInt, spectrumIdx,
                Seq(mzValues(spectrumIdx), mzValues(spectrumIdx+1),mzValues(spectrumIdx+2)))
            }
        }
      ).toSeq
  }

  /**
   *
   * @param distance distance in m/z to check a peak
   * @return
   */
  def detectNeutralLoss(
                         source: MZXMLFile,
                         p : PeakIdentification,
                         distance : Double,
                         precision: Double = 0.1
                       ) : Option[Double] = {
    val scan : IScan = source.parseScan(p.numScan, true)
    val spectrum = scan.fetchSpectrum()
    val mzSearch = p.mz(0)-distance
    //    println(  p.mz(0)-distance, spectrum.getMZs()(v1),spectrum.getMZs()(v2),spectrum.getMZs()(v3))
    val v = (spectrum.findClosestMzIdx(mzSearch))

    if ( (mzSearch - spectrum.getMZs()(v) ).abs < precision )
      Some(spectrum.getIntensities()(v))
    else None
  }
}
