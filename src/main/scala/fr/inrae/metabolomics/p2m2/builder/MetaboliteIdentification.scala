package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.output.CsvMetabolitesIdentification
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

case class MetaboliteIdentification(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     start: Option[Double],
                                     end: Option[Double],
                                     peaks : Seq[PeakIdentification]
                                   ) {
  def getInfo( p :PeakIdentification,precisionMzh : Int) : CsvMetabolitesIdentification = {
    val mz = p.peaks.map(p2 => (p2.mz*precisionMzh ).round / precisionMzh.toDouble )
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

  def getInfos(precisionMzh : Int): Seq[CsvMetabolitesIdentification] = {
    println("\n== detectNeutralLoss/detectDaughterIons == ")

    peaks.zipWithIndex
      . map {
     case (x,idx) =>
       print(s"\r===>$idx/${peaks.size}")
       getInfo(x,precisionMzh)
    }
      /* remove entry if none neutral and none daughters ions detected or big abundance (>60%)*/
      .filter( csvM => (csvM.neutralLosses.values.flatten.nonEmpty && csvM.daughterIons.values.flatten.nonEmpty) )
      .sortBy( x => (x.rt,x.mz.head) )
  }
}
