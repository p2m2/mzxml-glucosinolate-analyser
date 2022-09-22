package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.builder.GLSRelatedDiagnostic.GLSRelatedDiagnostic.{DaughterIons, NeutralLosses}
import umich.ms.datatypes.scan.IScan
import umich.ms.datatypes.spectrum.ISpectrum
import umich.ms.fileio.filetypes.mzxml._

import java.io.File
import scala.jdk.CollectionConverters._
import scala.math.sqrt
import scala.util.{Success, Try}

case object ScanLoader {

  /**
   * Reade a MZXML File and give MZXML Structure file
   * @param f
   * @return
   */
  def read( f : File ) : (MZXMLFile,MZXMLIndex) = {
    val source : MZXMLFile = new MZXMLFile(f.getPath)
    println(source.parseRunInfo())
    source.setExcludeEmptyScans(true)

    val index : MZXMLIndex = source.fetchIndex()
    println("==========================================================")
    println("MS1 size        : "+scansMs(source, index, None, None, 1).size)
    println("MS2 size        : "+scansMs(source, index, None, None, 2).size)
    println(s"Instruments    : \n${source.parseRunInfo().getInstruments.values().asScala.map(
      k =>
        "Model:"+k.getModel+" Analyzer:"+k.getAnalyzer +"\n"+
        "Detector:"+k.getDetector +" Ionisation:"+k.getIonisation +"\n"+
        "Manufacturer:"+k.getManufacturer + " S/N:" + k.getSerialNumber
    ).mkString("\n\n") } ")


    (source,index)
    // The index gives you the scan numbers, on the lowest level you can parse// The index gives you the scan numbers, on the lowest level you can parse
  }

  /**
   *
   * @param scan           : Scan of MzXML
   * @param spectrum       : Spectrum (list ok peak detection)
   * @param idxIsotope0    : index of M+0 in the spectrum
   * @param idxIsotope1    : index of M+1 in the spectrum - could be None if the method don't use M+1
   * @param idxIsotope2    : index of M+2 in the spectrum - could be None if the method don't use M+2
   * @return PeakIdentification
   */
  def fillPeakIdentification(
                              scan : IScan,
                              spectrum : ISpectrum,
                              idxIsotope0: Int,
                              idxIsotope1: Option[Int],
                              idxIsotope2: Option[Int],
                            ) : PeakIdentification = {
    val lIdxPeaks = Seq(Some(idxIsotope0),idxIsotope1,idxIsotope2)
    PeakIdentification(
      scan.getNum,
      lIdxPeaks.flatten,
      lIdxPeaks.zipWithIndex.flatMap{
        case (idxOption,isotopeNum) => idxOption match {
          case Some(idx) =>
            Some(Peak(
              isotopeNum,
              spectrum.getIntensities()(idx),
              spectrum.getIntensities()(idx) / scan.getBasePeakIntensity,
              spectrum.getMZs()(idx)))
          case None => None
      }},
      scan.getRt
    )

  }

  /**
   * Get Scan according MS Type.
   * Spectrum are not loaded.
   * @param source   : source of MZXML
   * @param index    : index of MZXML
   * @param ms       :1 or 2 MS Type
   * @return available scans
   */
  def scansMs(
                source: MZXMLFile,
                index: MZXMLIndex,
                start: Option[Double],
                end: Option[Double],
                ms : Integer
             ) : Seq[IScan] = {
    index
      .getMapByRawNum
      .keySet() // The second parameter asks the parser to parse the spectrum along
      .asScala
      // .filter( _ == 3569)
      .flatMap(scanNumRaw => Try(source.parseScan(scanNumRaw, false)) match {
        case Success(scan) => Some(scan)
        case _ => None
      })
      .filter(_.getMsLevel == ms).toSeq
      .filter(scan => start match {
        case Some(v) => v < scan.getRt
        case None => true
      })
      .filter(scan => end match {
        case Some(v) => v > scan.getRt
        case None => true
      })
  }

  def calculBackgroundNoisePeak(
                                 source: MZXMLFile,
                                 index: MZXMLIndex,
                                 start: Option[Double],
                                 end: Option[Double],
                                 startDurationTime : Double = 2.0
                               ): Int = {
    val allScans =
      scansMs(source,index,start,end,1)
        .filter( _.getRt<startDurationTime)
        .map {
          scanMs1 =>
            val scan = source.parseScan(scanMs1.getNum, true)
            val spectrum = scan.fetchSpectrum()

            spectrum.getSumInt/spectrum.getIntensities.length
        }
    val mean = allScans.sum/allScans.size
    val std = sqrt(allScans.map( v => (v - mean)*(v - mean) ).sum / allScans.size)
    println(" ======= BackgroundNoisePeak ==========")
    println(s"=====   mean = $mean std = $std =========")
    mean.toInt
  }

  def getScanIdxAndSpectrum3IsotopesSulfurContaining(
                                      source: MZXMLFile,
                                      index: MZXMLIndex,
                                      start : Option[Double] = None,
                                      end : Option[Double] = None,
                                      thresholdAbundanceM0Filter : Double,
                                      intensityFilter : Int,
                                      precision: Double = 0.01
                                    ): Seq[PeakIdentification] = {
    println("\n== Search for isotopes sulfur == ")
    // the file using those numbers. We need the raw scan numbers (the numbers
    // as they're used in the file). The internal scan numbering scheme always
    // renumbers all scans starting from 1 and increasing by 1 consecutively.
    val allScans = scansMs(source,index,start,end,1)
      .filter( scan => start match {
        case Some(v) => v < scan.getRt
        case None => true
      })
      .filter(scan => end match {
        case Some(v) => v > scan.getRt
        case None => true
      })

    allScans.zipWithIndex.flatMap {
        case (basicScan,i) => {
          print(s"\r===>$i/${allScans.size}")
          val scan = source.parseScan(basicScan.getNum, true)
          val spectrum = scan.fetchSpectrum()
          val mzValues = spectrum.getMZs

          // remove the first one to compute Delta M
          mzValues
            .zipWithIndex
            .filter { case (_, idx) => (spectrum.getIntensities()(idx)/scan.getBasePeakIntensity)>thresholdAbundanceM0Filter   }
            .map { case (mz, idx1) =>
              val mz_ms_p2 = mz + 1.99
              val idx2 = spectrum.findClosestMzIdx(mz_ms_p2)
              val mz_p2 = spectrum.getMZs()(idx2)
              (mz,idx1,mz_p2,idx2)
            }
            .filter { case (_,_,_,idx2) => spectrum.getIntensities()(idx2) > intensityFilter  }
            //.filter { case (_,_,_,idx2) => (spectrum.getIntensities()(idx2)/scan.getBasePeakIntensity)*(25.0) > 1.5  }
            /* abundance filter */
        /*    .filter { case (_,idx1,_,idx2) =>
              (spectrum.getIntensities()(idx1) + spectrum.getIntensities()(idx2))/scan.getBasePeakIntensity > 0.1  }*/
            .filter { case (mz, idx1,mz_p2,idx2) => {
              ((mz - mz_p2).abs - 1.99).abs < precision
            }}
            .map { case (_, idx1,_,idx2) => fillPeakIdentification(scan,spectrum,idx1,None,Some(idx2))
              //PeakIdentification(scan.getNum, Seq(idx1,idx2))
            }
        }}.toSeq
  }

  /**
   * Merge all M/z and keep the Ions with the maximum abundance
   * @param peaks
   * @return
   */
  def keepSimilarMzWithMaxAbundance(peaks: Seq[PeakIdentification],precisionMzh : Int): Seq[PeakIdentification] = {
    peaks.map {
      p =>
        val mz = (p.peaks.head.mz * precisionMzh).round / precisionMzh.toDouble
        (mz, p)
    }.foldLeft(Map[Double, Seq[PeakIdentification]]()) {
      case (acc, (mz, p)) if acc.contains(mz) => acc + (mz -> (acc(mz) ++ Seq(p)))
      case (acc, (mz, p)) => acc + (mz -> Seq(p))

    }.map {
      case (_, listPeaks) => listPeaks.maxBy(_.peaks.head.abundance)
    }.toSeq
  }

  def filterOverRepresentedPeak(
                                 source: MZXMLFile,
                                 index: MZXMLIndex,
                                 start: Option[Double],
                                 end: Option[Double],
                                 peaks: Seq[PeakIdentification],
                                 intensityFilter : Double,
                                 threshold: Int): MetaboliteIdentification = {

    println(s"\n=== filterOverRepresentedPeak == threshold=$threshold size=${peaks.length}")

   // val mzs = peaks.map(_.peaks.head.mz)
    val allScans = scansMs(source, index,start,end, 1)

    val countAllPeak: Seq[Int] =
      allScans
      .zipWithIndex
      .map {
        case (scanR,i) => {
          val scan = source.parseScan(scanR.getNum, true)
          print(s"\r===>$i/${allScans.size}")
          // Do something with the scan.
          // Note that some features, like scan.getChildScans() will not work in
          // this case, as there is not enough information to build those
          // relationships.
          val spectrum = scan.fetchSpectrum()

          peaks.map(_.peaks.head.mz)
            .map(mz => {
              val idx = spectrum.findClosestMzIdx(mz)
              if (spectrum.getIntensities()(idx) > intensityFilter)
                1
              else
                0
            })
        }}
      /* count all peak over the chromatogram */
      .foldLeft(peaks.indices.map(_ => 0))(
        (s, elt) => {
          s.zipWithIndex.map { case (e, i) => e + elt(i) }
        }
      )

   // println(countAllPeak)
    /* calcul distribution of Peak number  */
    println("\n == distribution of selected peak ( intensity / number )")
    val u = countAllPeak.foldLeft(Map[Int,Int]()) {
      case (acc, c) if acc.contains(c) => acc + (c -> (acc(c)+1))
      case (acc, c) => acc + (c -> 1)
    }.map( x => (x._1,x._2) ).toSeq.sortWith(
      (x,y) =>
        if (x._2 == y._2) {
          x._1 >= y._1
        } else (x._2 >= y._2)
    )

    println(u)

    val newL = peaks.zipWithIndex filter {
      case (_, i) => countAllPeak(i) < threshold
    } map {
      case (p, _) => p
    }

    println(s" new size:${newL.length}")
    MetaboliteIdentification(source, index,start,end,newL)
  }

  def searchIons(source: MZXMLFile,
                 l: Seq[IScan],
                 mzSearch:Double,
                 precisionPeakDetection: Double
                ): Option[Double] = {
    l.flatMap {
      scanMs2 =>
        val scan2 = source.parseScan(scanMs2.getNum, true)
        scan2.getSpectrum match {
          case spectrum if (spectrum != null) => val v = (spectrum.findClosestMzIdx(mzSearch))
            if ((mzSearch - spectrum.getMZs()(v)).abs < precisionPeakDetection)
              Some(spectrum.getIntensities()(v))
            else None
          case _ => None
        }
    }.sorted.lastOption // take the biggest value
  }

  /**
   *
   * @param distance distance in m/z to check a peak
   * @return
   */
  def detectNeutralLoss(
                         source: MZXMLFile,
                         index : MZXMLIndex,
                         start: Option[Double],
                         end: Option[Double],
                         p : PeakIdentification,
                         nls : Seq[NeutralLosses],
                         precisionPeakDetection: Double = 0.2,
                         precisionRtTime : Double = 0.03
                       ) : Map[GLSRelatedDiagnostic.GLSRelatedDiagnostic.NLs.Value,Option[Double]] = {

    val scanMs2: Seq[IScan] = scansMs(source, index,start,end, 2)
      .filter(scanMs2 => {
        (scanMs2.getRt - p.rt).abs < precisionRtTime
      })

    val mz = p.peaks.head.mz

    nls.map (
      nl => {
        nl.name->searchIons(source,scanMs2,mz - nl.distance,precisionPeakDetection)
      }
    ).toMap
  }

  /**
   *
   * @param distance distance in m/z to check a peak
   * @return
   */
  def detectDaughterIons(
                         source: MZXMLFile,
                         index: MZXMLIndex,
                         start: Option[Double],
                         end: Option[Double],
                         p: PeakIdentification,
                         dis: Seq[DaughterIons],
                         precisionPeakDetection: Double = 0.1,
                         precisionRtTime: Double = 0.03
                       ): Map[GLSRelatedDiagnostic.GLSRelatedDiagnostic.DIs.Value, Option[Double]] = {

    val scanMs2 = scansMs(source, index,start,end, 2)
      .filter(scanMs2 => {
        (scanMs2.getRt - p.rt).abs < precisionRtTime
      })

    dis.map(
      di => {
        di.name->searchIons(source,scanMs2,di.distance,precisionPeakDetection)
      }
    ).toMap
  }
}
