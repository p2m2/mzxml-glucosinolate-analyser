package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.fileformats.SpectrumData
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

import java.io.File

case class IonsIdentificationBuilder(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     peaks : Seq[PeakIdentification],
                                     nls : Seq[(String,Double)],
                                     dis : Seq[(String,Double)],
                                     noiseIntensity : Double = 0.0,
                                     spectrumData : Option[SpectrumData] = None
                                   ) {

  def getPath(source : MZXMLFile) : String =
    new File(source.getPath).getCanonicalPath

  private def pathFile : String = spectrumData match {
    case Some(sd) =>
      if (sd.metadata.path.nonEmpty) sd.metadata.path
      else new File(sd.metadata.fileName).getCanonicalPath
    case None => new File(source.getPath).getCanonicalPath
  }

  private def detectNeutralLoss(p : PeakIdentification, tolMzh : Double) : Map[String,Option[(String,Double,Double)]] =
    spectrumData match {
      case Some(sd) => ScanLoader.detectNeutralLoss(sd,p,nls,tolMzh=tolMzh,precisionRtTime=0.02,noiseIntensity = noiseIntensity)
      case None => ScanLoader.detectNeutralLoss(source,index,p,nls,tolMzh=tolMzh,noiseIntensity = noiseIntensity)
    }

  private def detectDaughterIons(p : PeakIdentification, tolMzh : Double) : Map[String,Option[(String,Double,Double)]] =
    spectrumData match {
      case Some(sd) => ScanLoader.detectDaughterIons(sd,p,dis,tolMzh=tolMzh,precisionRtTime=0.02,noiseIntensity = noiseIntensity)
      case None => ScanLoader.detectDaughterIons(source,index,p,dis,tolMzh=tolMzh,noiseIntensity = noiseIntensity)
    }

  def getInfo( p :PeakIdentification,tolMzh : Double, mzCoreStructure : Double) : Option[IonsIdentification] = p.peaks.nonEmpty match {
    case true =>
      if ( p.peaks.head.mz >= mzCoreStructure )
        Some(IonsIdentification(
          pathFile,
          p,
          neutralLosses = detectNeutralLoss(p,tolMzh),
          daughterIons = detectDaughterIons(p,tolMzh)
        ))
      else
        Some(IonsIdentification(
          pathFile,
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
      .flatMap {
        case (x,idx) =>
          print(s"\r===>$idx/${peaks.size}")
          getInfo(x,tolMzh,mzCoreStructure)
      }
      .sortBy( x => (x.ion.rt,x.ion.peaks.head.mz) )
  }
}

object IonsIdentificationBuilder {

  /**
   * Build an [[IonsIdentificationBuilder]] from a unified [[SpectrumData]].
   */
  def fromSpectrumData(
                        spectrumData : SpectrumData,
                        peaks : Seq[PeakIdentification],
                        nls : Seq[(String,Double)],
                        dis : Seq[(String,Double)],
                        noiseIntensity : Double = 0.0
                      ) : IonsIdentificationBuilder =
    IonsIdentificationBuilder(null,null,peaks,nls,dis,noiseIntensity,Some(spectrumData))
}
