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
        val mz = s.fetchSpectrum().getMZs()(p.indexIsotopeInSpectrum.head)

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

    val mzs = peaks.map(
      p => {
        val s = source.parseScan(p.numScan, true)
        s.fetchSpectrum().getMZs()(p.indexIsotopeInSpectrum.head)
      })

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
    val scan : IScan = source.parseScan(p.numScan, true)

    val gluconolactone_178 = ScanLoader.detectNeutralLoss(source,index,p,178)
    val sulfureTrioxide_80 = ScanLoader.detectNeutralLoss(source,index,p,80)
    val anhydroglucose_162 = ScanLoader.detectNeutralLoss(source,index,p,162)
    val thioglucose_s03_242 = ScanLoader.detectNeutralLoss(source,index,p,242)
    val thioglucose_196 = ScanLoader.detectNeutralLoss(source,index,p,196)
    val last_223 = ScanLoader.detectNeutralLoss(source,index,p,223)


    val spectrum = scan.fetchSpectrum()
    val mz = p.indexIsotopeInSpectrum.map(idx => (spectrum.getMZs()(idx)*1000 ).round / 1000.toDouble )
    val intensities = p.indexIsotopeInSpectrum.map(idx => spectrum.getIntensities()(idx))
    val abundance = intensities.map( i => (i / scan.getBasePeakIntensity) )
    CsvMetabolitesIdentification(
      mz,
      intensities,
      abundance,
      scan.getRt,
      neutralLosses = Map(
        NLs.gluconolactone -> gluconolactone_178,
        NLs.sulfureTrioxide -> sulfureTrioxide_80,
        NLs.anhydroglucose -> anhydroglucose_162,
        NLs.thioglucose_s03 -> thioglucose_s03_242,
        NLs.thioglucose -> thioglucose_196,
        NLs.glucosinolate_223 -> last_223
      ),
      daughterIons = Map()
    )
  }

  def getInfos() : Seq[CsvMetabolitesIdentification] = {
    val nScan = index.getMapByNum.keySet().size()
    peaks
    //  .slice(0,2) //debug
     .zipWithIndex. map {
     case (x,idx) => { println(s"${idx}/${peaks.length}") ; getInfo(x) }
    }
      .sortBy( x => (x.rt,x.mz.head) )
  }
}
