package fr.inrae.metabolomics.p2m2.builder

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
    //source.setExcludeEmptyScans(true)

    val index : MZXMLIndex = source.fetchIndex()
    println("==========================================================")
    println("FILE            : "+f.getAbsolutePath)
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
                              idxIsotope3: Option[Int],
                            ) : PeakIdentification = {
    val lIdxPeaks = Seq(Some(idxIsotope0),idxIsotope1,idxIsotope2,idxIsotope3)
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
              spectrum.getMZs()(idx)
            ))
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
        case Some(v) => v <= scan.getRt
        case None => true
      })
      .filter(scan => end match {
        case Some(v) => v >= scan.getRt
        case None => true
      })
  }

  def calculBackgroundNoisePeak(
                                 source: MZXMLFile,
                                 index: MZXMLIndex,
                                 start: Option[Double],
                                 end: Option[Double],
                                 startDurationTime : Double = 0.05
                               ): Double = {
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
    mean
  }

  def getScanIdxAndSpectrumM0M2WithDelta(
                                      source: MZXMLFile,
                                      index: MZXMLIndex,
                                      start : Option[Double] = None,
                                      end : Option[Double] = None,
                                      thresholdAbundanceM0Filter : Double,
                                      intensityFilter : Int,
                                      filteringOnNbSulfur : Int = 0,
                                      minAbundanceM1: Double,
                                      maxAbundanceM1: Double,
                                      precision: Double = 0.01,
                                      deltaMOM2 : Double
                                    ): Seq[PeakIdentification] = {
    println("\n== Search for isotopes sulfur == ")
    // the file using those numbers. We need the raw scan numbers (the numbers
    // as they're used in the file). The internal scan numbering scheme always
    // renumbers all scans starting from 1 and increasing by 1 consecutively.
    val allScans = scansMs(source,index,start,end,1)

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
            .map { case (mz0, idx0) =>
              val mz_ms_p2 = mz0 + deltaMOM2
              val idx2 = spectrum.findClosestMzIdx(mz_ms_p2)
              val mz_p2 = spectrum.getMZs()(idx2)
              (mz0,idx0,mz_p2,idx2)
            }
            .filter { case (_,_,_,idx2) => spectrum.getIntensities()(idx2) > intensityFilter  }
            /* filtering on presence of souffer is too restrictive....*/
            .filter { case (_,idx0,_,_) => (spectrum.getIntensities()(idx0)/scan.getBasePeakIntensity)*(25.0) > filteringOnNbSulfur.toDouble  }
            /* abundance filter */
        /*    .filter { case (_,idx1,_,idx2) =>
              (spectrum.getIntensities()(idx1) + spectrum.getIntensities()(idx2))/scan.getBasePeakIntensity > 0.1  }*/
            .filter { case (mz, idx1,mz_p2,idx2) => {
              ((mz - mz_p2).abs - 1.99).abs < precision
            }}
            .map {
              case (mz0, idx0  ,mz2 ,idx2 ) =>
                val idx1 = spectrum.findClosestMzIdx(mz0+1.0)
                val idx3 = spectrum.findClosestMzIdx(mz0+3.0)
                (mz0, idx0  ,mz2 ,idx2, idx1, idx3 )
            }
            /* Gestion d'une abondance minimum a deporter dans les critere de resultats */
            .filter { /* M+1   min abundance */
              case (_, idx0  ,_ ,_, idx1,_ ) =>
                (spectrum.getIntensities()(idx1) / spectrum.getIntensities()(idx0) >= minAbundanceM1) &&
                  (spectrum.getIntensities()(idx1) / spectrum.getIntensities()(idx0) <= maxAbundanceM1)
            }
            /* --- fin critere abondance*/
            .map { case (_, idx0,_,idx2, idx1, idx3) => fillPeakIdentification(scan,spectrum,idx0,Some(idx1),Some(idx2),Some(idx3))
              //PeakIdentification(scan.getNum, Seq(idx1,idx2))
            }
        }}.toSeq
  }

  def getDeltaNeutralLossesFromPeak(
                                          source: MZXMLFile,
                                          index: MZXMLIndex,
                                          peak :PeakIdentification,
                                          intensityFilter: Int,
                                          precision: Double = 0.02
                                        ): Seq[Double] = {

    val allScans = scansMs(source,index,Some(peak.rt-precision),Some(peak.rt+precision),2)
    allScans.zipWithIndex.flatMap {
      case (basicScan, i) => {
        print(s"\r===>$i/${allScans.size}")
        val scan = source.parseScan(basicScan.getNum, true)
        val spectrum = scan.fetchSpectrum()
        val mzValues = spectrum.getMZs

        mzValues
          .zipWithIndex
          .filter { case (mz, _) => mz < peak.peaks.head.mz }
          .filter { case (_, idx) => spectrum.getIntensities()(idx) > intensityFilter }
          // mz ares now Ion Diag Frag
          // check if dela is a Ion Diag Frag
          .map { case (mz, idx) => (peak.peaks.head.mz - mz, idx) }
          .filter {
            case (delta, idx) =>
              val idx1 = spectrum.findClosestMzIdx(delta)
              spectrum.getIntensities()(idx1) <= intensityFilter
          }
          .map( _._1)
      }
    }
  }

  /**
   * Merge all M/z in a short Windows RT (features) and keep the Ions with the maximum abundance
   * @param peaks
   * @return
   */
  def keepSimilarMzWithMaxAbundance(peaks: Seq[PeakIdentification],precisionMzh : Int): Seq[PeakIdentification] = {
    peaks.map {
      p =>
        val mz = (p.peaks.head.mz * precisionMzh).round / precisionMzh.toDouble
        val rt =  (p.rt * 3).round / 3.toDouble // windows 0.6 sec ... to check
        (mz, rt, p)
    }.foldLeft(Map[(Double,Double), Seq[PeakIdentification]]()) {
      case (acc, (mz, rt, p)) if acc.contains( (mz,rt) ) => acc + ( (mz,rt) -> (acc( (mz,rt) ) ++ Seq(p)))
      case (acc, (mz, rt, p)) => acc + ( (mz,rt) -> Seq(p))

    }.map {
      case ( (_,_), listPeaks) => listPeaks.maxBy(_.peaks.head.abundance)
    }.toSeq
  }

  def filterOverRepresentedPeak(
                                 source: MZXMLFile,
                                 index: MZXMLIndex,
                                 start: Option[Double],
                                 end: Option[Double],
                                 peaks: Seq[PeakIdentification],
                                 intensityFilter : Double,
                                 threshold: Int,
                                 nls: Seq[(String, Double)],
                                 dis: Seq[(String, Double)],
                                 noiseIntensity : Double
                               ): IonsIdentificationBuilder = {

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
    /*
    println("\n=========================================================")
    println("== Number of Peak detected on MS1 scans by M/z selected    ")
    println("=========================================================\n")

    println(countAllPeak.zipWithIndex.map{  case (c,id) => s"${peaks(id).peaks.head.mz} m/z -> $c" }.mkString("\n"))

    println("\n\n=========================================================")
    println(" -- The thirty most detected peaks selected --")
    println(countAllPeak.sorted(Ordering[Int].reverse).distinct.slice(0,30))

     */
    /*
    val u = countAllPeak.foldLeft(Map[Int,Int]()) {
      case (acc, c) if acc.contains(c) => acc + (c -> (acc(c)+1))
      case (acc, c) => acc + (c -> 1)
    }.map( x => (x._1,x._2) ).toSeq.sortWith(
      (x,y) =>
        if (x._2 == y._2) {
          x._1 >= y._1
        } else (x._2 >= y._2)
    )

    println(u)*/

    val newL = peaks.zipWithIndex filter {
      case (_, i) => countAllPeak(i) < threshold
    } map {
      case (p, _) => p
    }

    println(s" new size:${newL.length}")
    IonsIdentificationBuilder(source, index,start,end,newL,nls,dis,noiseIntensity=noiseIntensity)
  }

  def searchIons(source: MZXMLFile,
                 l: Seq[IScan],
                 mzSearch:Double,
                 precisionPeakDetection: Double,
                 noiseIntensity : Double,
                ): Option[Double] = {
    l.flatMap {
      scanMs2 =>
        val scan2 = source.parseScan(scanMs2.getNum, true)
        scan2.getSpectrum match {
          case spectrum if (spectrum != null) =>
            val v = (spectrum.findClosestMzIdx(mzSearch))
            val mz = spectrum.getMZs()(v)
            if ( (mzSearch - mz).abs < precisionPeakDetection) {
              //println(spectrum.getIntensities()(v),noiseIntensity)
              if (spectrum.getIntensities()(v)>noiseIntensity)
                Some(mz)
              else
                None
            } else None
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
                         p : PeakIdentification,
                         nls : Seq[(String,Double)], /* name, distance */
                         precisionPeakDetection: Double = 0.9,
                         precisionRtTime : Double = 0.02,
                         noiseIntensity : Double,
                       ) : Map[String,Option[Double]] = {

    val sc = source.parseScan(p.numScan, false)
    val scanMs2: Seq[IScan] = ScanLoader.scansMs(
      source, index, Some(sc.getRt - precisionRtTime), Some(sc.getRt + precisionRtTime), 2
    )

  //  val scanMs2 : Seq[IScan]= Seq(source.parseScan(p.numScan, true))

    val mz = p.peaks.head.mz

    nls.map (
      nl => {
        nl._1->searchIons(source,scanMs2,mz - nl._2,precisionPeakDetection,noiseIntensity)
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
                         p: PeakIdentification,
                         dis: Seq[(String,Double)], /* name , mz */
                         precisionPeakDetection: Double = 0.3,
                         precisionRtTime: Double = 0.02,
                         noiseIntensity : Double,
                       ): Map[String, Option[Double]] = {

    val sc = source.parseScan(p.numScan, false)
    val scanMs2: Seq[IScan] = ScanLoader.scansMs(
      source, index, Some(sc.getRt - precisionRtTime), Some(sc.getRt + precisionRtTime), 2
    )
    //val scanMs2 : Seq[IScan]= Seq(source.parseScan(p.numScan, true))

    dis.map(
      di => {
        di._1->searchIons(source,scanMs2,di._2,precisionPeakDetection,noiseIntensity)
      }
    ).toMap
  }
}
