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
                                     peaks : Seq[PeakIdentification]
                                   ) {

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


  def filterOverRepresentedPeak(threshold: Int): MetaboliteIdentification = {

    println(s"=== filterOverRepresentedPeak == threshold=$threshold size=${peaks.length}")

    val mzs = peaks.map(_.peaks.head.mz)

      val countAllPeak : Seq[Int]= index
          .getMapByRawNum
          .keySet() // The second parameter asks the parser to parse the spectrum along
          .asScala
          // .filter( _ == 3569)
          .flatMap(scanRaw => Try(source.parseScan(scanRaw, true)) match {
            case Success(a) => Some(a)
            case _ => None
          }).
          filter( _.getMsLevel == 1 )
          //.slice(0, 10)
          .map(
            scan => {
              // Do something with the scan.
              // Note that some features, like scan.getChildScans() will not work in
              // this case, as there is not enough information to build those
              // relationships.
              val spectrum = scan.fetchSpectrum()
              mzs.indices
                .map( id => {
                  val mz = mzs(id)
                  val idx = spectrum.findClosestMzIdx(mz)
                  //println(mz,idx)
                  if (spectrum.getIntensities()(idx)>0)
                    1
                  else
                    0
                })
            })
        /* count all peak over the chromatogram */
        .foldLeft(mzs.indices.map(_=>0))(
          ( s , elt) => { s.zipWithIndex.map {case (e,i) => e+elt(i)} }
      )

    val newL = peaks.zipWithIndex filter {
      case (_,i) =>  countAllPeak(i)<threshold
    } map {
      case (p,_) => p
    }

    println(s" new size:${newL.length}")
    MetaboliteIdentification(source, index, newL)
  }

  def getInfo( p :PeakIdentification) : CsvMetabolitesIdentification = {
    val mz = p.peaks.map(p2 => (p2.mz*1000 ).round / 1000.toDouble )
    val intensities = p.peaks.map(_.intensity)
    val abundance = p.peaks.map(_.abundance)

    CsvMetabolitesIdentification(
      mz,
      intensities,
      abundance,
      p.rt,
      neutralLosses = ScanLoader.detectNeutralLoss(source,index,p,GLSRelatedDiagnostic.GLSRelatedDiagnostic.nls()),
      daughterIons = ScanLoader.detectDaughterIons(source,index,p,GLSRelatedDiagnostic.GLSRelatedDiagnostic.dis())
    )
  }

  def getInfos() : Seq[CsvMetabolitesIdentification] = {
    val nScan = index.getMapByNum.keySet().size()
    peaks
    //  .slice(0,2) //debug
     .zipWithIndex. map {
     case (x,idx) => { println(s"${idx}/${peaks.length}") ; getInfo(x) }
    }
      /* remove entry if none neutral or daughters ions detected */
      .filter( csvM => (csvM.neutralLosses.values.flatten.size+ csvM.daughterIons.values.flatten.size)>0)
      .sortBy( x => (x.rt,x.mz.head) )
  }
}
