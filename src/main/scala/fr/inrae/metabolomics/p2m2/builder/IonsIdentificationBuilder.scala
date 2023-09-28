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
  def getInfo( p :PeakIdentification,tolMzh : Double) : Option[IonsIdentification] = if (p.peaks.nonEmpty) {
    Some(IonsIdentification(
      getPath(source),
      p,
      neutralLosses = ScanLoader.detectNeutralLoss(source, index, p, nls, tolMzh = tolMzh, noiseIntensity = noiseIntensity),
      daughterIons = ScanLoader.detectDaughterIons(source, index, p, dis, tolMzh = tolMzh, noiseIntensity = noiseIntensity)
    ))
  } else {
    None
  }

  /**
   *
   * @param precisionMzh precision of mzh
   * @param mzCoreStructure minimum size of a metabolite according param family
   * @return
   */
  def findDiagnosticIonsAndNeutralLosses(tolMzh : Double): Seq[IonsIdentification] = {
    println("\n== detectNeutralLoss/detectDaughterIons == ")

    peaks.zipWithIndex
      . flatMap {
     case (x,idx) =>
       print(s"\r===>$idx/${peaks.size}")
       getInfo(x,tolMzh)
    }
      .sortBy( x => (x.ion.rt,x.ion.peaks.head.mz) )
  }
}
