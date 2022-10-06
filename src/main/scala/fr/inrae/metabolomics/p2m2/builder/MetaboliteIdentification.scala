package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.output.MetabolitesIdentification
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

case class MetaboliteIdentification(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     start: Option[Double],
                                     end: Option[Double],
                                     peaks : Seq[PeakIdentification],
                                     nls : Seq[(String,Double)],
                                     dis : Seq[(String,Double)]
                                   ) {
  def getInfo( p :PeakIdentification,precisionMzh : Int, mzCoreStructure : Double) : Option[MetabolitesIdentification] = p.peaks.nonEmpty match {
    case true =>
      val mz = p.peaks.map(p2 => (p2.mz*precisionMzh ).round / precisionMzh.toDouble )
      val intensities = p.peaks.map(_.intensity)
      val abundance = p.peaks.map(_.abundance)

      if ( p.peaks.head.mz >= mzCoreStructure )
        Some(MetabolitesIdentification(
          mz,
          intensities,
          abundance,
          p.rt,
          neutralLosses = ScanLoader.detectNeutralLoss(source,index,start,end,p,nls),
          daughterIons = ScanLoader.detectDaughterIons(source,index,start,end,p,dis)
        ))
      else
        Some(MetabolitesIdentification(
          mz,
          intensities,
          abundance,
          p.rt,
          neutralLosses = Map(),
          daughterIons = Map()
        ))
    case false => None
  }

  /**
   *
   * @param precisionMzh precision of mzh
   * @param mzCoreStructure minimum size of a metabolite according param family
   * @return
   */
  def findDiagnosticIonsAndNeutralLosses(precisionMzh : Int, mzCoreStructure : Double): Seq[MetabolitesIdentification] = {
    println("\n== detectNeutralLoss/detectDaughterIons == ")

    peaks.zipWithIndex
      . flatMap {
     case (x,idx) =>
       print(s"\r===>$idx/${peaks.size}")
       getInfo(x,precisionMzh,mzCoreStructure)
    }
      .sortBy( x => (x.rt,x.mz.head) )
  }
}
