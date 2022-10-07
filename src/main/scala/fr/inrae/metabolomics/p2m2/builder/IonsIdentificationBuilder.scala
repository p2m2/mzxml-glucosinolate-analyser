package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

case class IonsIdentificationBuilder(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     start: Option[Double],
                                     end: Option[Double],
                                     peaks : Seq[PeakIdentification],
                                     nls : Seq[(String,Double)],
                                     dis : Seq[(String,Double)]
                                   ) {
  def getInfo( p :PeakIdentification,precisionMzh : Int, mzCoreStructure : Double) : Option[IonsIdentification] = p.peaks.nonEmpty match {
    case true =>
      val mz = p.peaks.map(p2 => (p2.mz*precisionMzh ).round / precisionMzh.toDouble )
      val intensities = p.peaks.map(_.intensity)
      val abundance = p.peaks.map(_.abundance)

      if ( p.peaks.head.mz >= mzCoreStructure )
        Some(IonsIdentification(
          p,
          neutralLosses = ScanLoader.detectNeutralLoss(source,index,p,nls),
          daughterIons = ScanLoader.detectDaughterIons(source,index,p,dis)
        ))
      else
        Some(IonsIdentification(
          p,
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
  def findDiagnosticIonsAndNeutralLosses(precisionMzh : Int, mzCoreStructure : Double): Seq[IonsIdentification] = {
    println("\n== detectNeutralLoss/detectDaughterIons == ")

    peaks.zipWithIndex
      . flatMap {
     case (x,idx) =>
       print(s"\r===>$idx/${peaks.size}")
       getInfo(x,precisionMzh,mzCoreStructure)
    }
      .sortBy( x => (x.ion.rt,x.ion.peaks.head.mz) )
  }
}
