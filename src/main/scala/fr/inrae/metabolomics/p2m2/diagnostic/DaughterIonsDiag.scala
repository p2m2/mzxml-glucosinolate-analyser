package fr.inrae.metabolomics.p2m2.diagnostic

import fr.inrae.metabolomics.p2m2.builder.PeakIdentification
import umich.ms.datatypes.scan.IScan
import umich.ms.fileio.filetypes.mzxml.MZXMLFile

case object DaughterIonsDiag {

  def round(v : Double) = (v*1).round / 1.toDouble

  def getPeaksWithIntensitiesNoNull(
                                     source: MZXMLFile,
                                     p : PeakIdentification
                                   ) : Seq[Int] = {
    val scan : IScan = source.parseScan(p.numScan, true)

    (scan.getSpectrum().getMZs() zip scan.getSpectrum().getIntensities()).filter{
      case (_,y) => y>0
    } map { case (x,_) => round(x).toInt }
  }

  /**
   * Frequence des Ions dans les scans de la liste des peaks selectionnés avec un delta
   * @param peaks
   * @return
   */
  def IonsFrequencyOnSelectedScanPeakDetected(source: MZXMLFile,peaks : Seq[PeakIdentification]) : Seq[(Int,Int)] = {
    peaks.map(
      p => DaughterIonsDiag.getPeaksWithIntensitiesNoNull(source, p)
    ).foldLeft(Map[Int, Int]())(
      (acc: Map[Int, Int], v: Seq[Int]) => {
        v.map(p2 => acc.get(p2) match {
          case Some(s) => (p2 -> (s + 1))
          case None => (p2 -> 1)
        }).toMap
      }
    ).toSeq.sortBy(_._2)
  }
}
