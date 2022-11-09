package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

import java.io.File

case class IonsIdentificationBuilder(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     peaks : Seq[PeakIdentification],
                                     nls : Seq[(String,Double)],
                                     dis : Seq[(String,Double)],
                                     noiseIntensity : Double = 0.0
                                   ) {
  def getPath(source : MZXMLFile) : String =
    new File(source.getPath).getCanonicalPath //.replace(new File(".").getCanonicalPath,".")
  def getInfo( p :PeakIdentification,tolMzh : Double, mzCoreStructure : Double) : Option[IonsIdentification] = p.peaks.nonEmpty match {
    case true =>
      if ( p.peaks.head.mz >= mzCoreStructure )
        Some(IonsIdentification(
          getPath(source),
          p,
          neutralLosses = ScanLoader.detectNeutralLoss(source,index,p,nls,tolMzh=tolMzh,noiseIntensity = noiseIntensity),
          daughterIons = ScanLoader.detectDaughterIons(source,index,p,dis,tolMzh=tolMzh,noiseIntensity = noiseIntensity)
        ))
      else
        Some(IonsIdentification(
          getPath(source),
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
  def findDiagnosticIonsAndNeutralLosses(tolMzh : Double, mzCoreStructure : Double): Seq[IonsIdentification] = {
    println("\n== detectNeutralLoss/detectDaughterIons == ")

    peaks.zipWithIndex
      . flatMap {
     case (x,idx) =>
       print(s"\r===>$idx/${peaks.size}")
       getInfo(x,tolMzh,mzCoreStructure)
    }
      .sortBy( x => (x.ion.rt,x.ion.peaks.head.mz) )
  }
}
