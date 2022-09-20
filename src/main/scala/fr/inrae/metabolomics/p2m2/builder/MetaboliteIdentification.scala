package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.builder.GLSRelatedDiagnostic.GLSRelatedDiagnostic.NLs
import fr.inrae.metabolomics.p2m2.output.CsvMetabolitesIdentification
import umich.ms.datatypes.scan.IScan
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.util.{Success, Try}

case class MetaboliteIdentification(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     start: Option[Double],
                                     end: Option[Double],
                                     peaks : Seq[PeakIdentification]
                                   ) {
/*
  def filterOverRepresentedPeak2(threshold : Int): MetaboliteIdentification = {

    val newL : Seq[PeakIdentification] = peaks.flatMap (
      p => {
        val s = source.parseScan(p.numScan, true)
        val mz = p.peaks.head.mz

        index
          .getMapByRawNum
          .keySet() // The second parameter asks the parser to parse the spectrum along
          .asScala
          .filter(scanNumRaw => source.parseScan(scanNumRaw, false).getMsLevel == 1)
          .map(
            scanNumRaw => {
              // Do something with the scan.
              // Note that some features, like scan.getChildScans() will not work in
              // this case, as there is not enough information to build those
              // relationships.
              val scan: IScan = source.parseScan(scanNumRaw, true)
              val spectrum = scan.fetchSpectrum()
              val idx = spectrum.findClosestMzIdx(mz)
              spectrum.getIntensities()(idx)
            }
          ).size match {
          case s if s>threshold => None
          case _ => Some(p)
        }
      }
    )
    MetaboliteIdentification(source,index,newL)
  }
*/

  def getInfo( p :PeakIdentification) : CsvMetabolitesIdentification = {
    val mz = p.peaks.map(p2 => (p2.mz*1000 ).round / 1000.toDouble )
    val intensities = p.peaks.map(_.intensity)
    val abundance = p.peaks.map(_.abundance)

    CsvMetabolitesIdentification(
      mz,
      intensities,
      abundance,
      p.rt,
      neutralLosses = ScanLoader.detectNeutralLoss(source,index,start,end,p,GLSRelatedDiagnostic.GLSRelatedDiagnostic.nls()),
      daughterIons = ScanLoader.detectDaughterIons(source,index,start,end,p,GLSRelatedDiagnostic.GLSRelatedDiagnostic.dis())
    )
  }
  def getInfos: Seq[CsvMetabolitesIdentification] = {
    println("\n== detectNeutralLoss/detectDaughterIons == ")
    peaks
    //  .slice(0,2) //debug
     .zipWithIndex
      . map {
     case (x,idx) =>
       print(s"\r===>$idx/${peaks.size}")
       getInfo(x)
    }
      /* remove entry if none neutral and none daughters ions detected */
      .filter( csvM => csvM.neutralLosses.values.flatten.nonEmpty && csvM.daughterIons.values.flatten.nonEmpty)
      .sortBy( x => (x.rt,x.mz.head) )
  }
}
