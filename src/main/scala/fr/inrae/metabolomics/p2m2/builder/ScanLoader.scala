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
    source.setExcludeEmptyScans(true)

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

  def getScanIdxAndSpectrum3IsotopesSulfurContaining(
                                      source: MZXMLFile,
                                      index: MZXMLIndex,
                                      precision: Double = 0.01

                                    ): Seq[PeakIdentification] = {

    // the file using those numbers. We need the raw scan numbers (the numbers
    // as they're used in the file). The internal scan numbering scheme always
    // renumbers all scans starting from 1 and increasing by 1 consecutively.
    index
      .getMapByRawNum
      .keySet() // The second parameter asks the parser to parse the spectrum along
      .asScala
      .filter( _ == 3569)
      .filter(scanNumRaw => source.parseScan(scanNumRaw, false).getMsLevel == 1)
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
          mzValues
            .zipWithIndex
            .filter { case (_, idx) => spectrum.getIntensities()(idx) >0.0   }
            .map { case (mz, idx1) => {
              val mz_ms_p2 = mz + 1.99
              val idx2 = spectrum.findClosestMzIdx(mz_ms_p2)
              val mz_p2 = spectrum.getMZs()(idx2)
              (mz,idx1,mz_p2,idx2)
            }}
            .filter { case (_,_,_,idx2) => spectrum.getIntensities()(idx2) >0.0  }
            .filter { case (mz, idx1,mz_p2,idx2) => {
              /*
              if ( mz>477.0 && mz< 477.1) println(mz,mz_p2, "-->",
                ((mz - mz_p2).abs - 1.99).abs,((mz - mz_p2).abs - 1.99).abs < precision,spectrum.getIntensities()(idx1),spectrum.getIntensities()(idx2))*/
              ((mz - mz_p2).abs - 1.99).abs < precision
            }}
            .map { case (_, idx1,_,idx2) =>
              PeakIdentification(scanNumRaw.toInt, Seq(idx1,idx2))
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
                         index : MZXMLIndex,
                         p : PeakIdentification,
                         distance : Double,
                         precision: Double = 0.1
                       ) : Option[Double] = {
    val scan : IScan = source.parseScan(p.numScan, true)
    val mz = scan.getSpectrum.getMZs()(p.indexIsotopeInSpectrum(0))
    val mzSearch = mz-distance

    index.getMapByRawNum.keySet() // The second parameter asks the parser to parse the spectrum along
      .asScala
      .filter(scanNumRaw => {
        val scanMs2 = source.parseScan(scanNumRaw, false)
        (scanMs2.getMsLevel == 2) && ((scanMs2.getRt - scan.getRt).abs < 0.1)
      }).flatMap {
      case scanMs2 =>
        val scan = source.parseScan(scanMs2, true)
        scan.getSpectrum match {
          case spectrum if spectrum != null => val v = (spectrum.findClosestMzIdx(mzSearch))
            if ((mzSearch - spectrum.getMZs()(v)).abs < precision)
              Some(spectrum.getIntensities()(v))
            else None
          case _ => None
        }
    }.toSeq.sorted.lastOption //prendre la plus grande valeur
    //    println(  p.mz(0)-distance, spectrum.getMZs()(v1),spectrum.getMZs()(v2),spectrum.getMZs()(v3))
  }
}
