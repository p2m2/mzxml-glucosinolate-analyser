package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.database.ChemicalUtils
import fr.inrae.metabolomics.p2m2.fileformats.{MzXmlReader, SpectrumFileReaderFactory, SpectrumData, Peak => SpectrumPeak, Scan => SpectrumScan}
import umich.ms.datatypes.scan.IScan
import umich.ms.datatypes.spectrum.ISpectrum
import umich.ms.fileio.filetypes.mzxml._

import java.io.File
import scala.jdk.CollectionConverters._
import scala.math.sqrt
import scala.util.{Success, Try}

case object ScanLoader {

  /**
   * Reads a MZXML File and gives the raw MZXML structures (legacy API).
   * @param f mzxml file
   * @return (source, index) mzxml structures
   */
  def read(f: File): (MZXMLFile, MZXMLIndex) = {
    val source: MZXMLFile = new MZXMLFile(f.getPath)
    println(source.parseRunInfo())

    val index: MZXMLIndex = source.fetchIndex()
    println("==========================================================")
    println("FILE            : " + f.getAbsolutePath)
    println("MS1 size        : " + scansMs(source, index, None, None, 1).size)
    println("MS2 size        : " + scansMs(source, index, None, None, 2).size)
    println(s"Instruments    : \n${source.parseRunInfo().getInstruments.values().asScala.map(
      k =>
        "Model:" + k.getModel + " Analyzer:" + k.getAnalyzer + "\n" +
          "Detector:" + k.getDetector + " Ionisation:" + k.getIonisation + "\n" +
          "Manufacturer:" + k.getManufacturer + " S/N:" + k.getSerialNumber
    ).mkString("\n\n") } ")

    (source, index)
  }

  /**
   * Reads a mass spectrometry file (mzXML, mzML, xcms) and extracts it
   * into a unified [[SpectrumData]] structure.
   * @param f file to read
   * @return extracted spectral data
   */
  def readSpectrumData(f: File): SpectrumData = {
    SpectrumFileReaderFactory.getReaderForFile(f) match {
      case Some(reader) => reader.read(f)
      case None         => new MzXmlReader().read(f) // fallback to the mzXML parser
    }
  }

  /**
   * Get Scan according MS Type.
   * Spectrum are not loaded.
   *
   * @param source : source of MZXML
   * @param index  : index of MZXML
   * @param ms     :1 or 2 MS Type
   * @return available scans
   */
  def scansMs(
               source: MZXMLFile,
               index: MZXMLIndex,
               start: Option[Double],
               end: Option[Double],
               ms: Integer
             ): Seq[IScan] = {
    index
      .getMapByRawNum
      .keySet()
      .asScala
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
   * Build a [[PeakIdentification]] from a unified [[SpectrumScan]] and its peaks.
   *
   * @param scan             : scan of the spectrum
   * @param mz               : m/z values sorted by increasing m/z
   * @param intensities      : associated intensities
   * @param basePeakIntensity: intensity of the base peak of the scan
   * @param idxIsotope0      : index of M+0 in the spectrum
   * @param idxIsotope1      : index of M+1 in the spectrum - could be None if the method don't use M+1
   * @param idxIsotope2      : index of M+2 in the spectrum - could be None if the method don't use M+2
   * @return PeakIdentification
   */
  def fillPeakIdentification(
                              scan: SpectrumScan,
                              mz: Array[Double],
                              intensities: Array[Double],
                              basePeakIntensity: Double,
                              idxIsotope0: Int,
                              idxIsotope1: Option[Int],
                              idxIsotope2: Option[Int],
                              idxIsotope3: Option[Int],
                            ): PeakIdentification = {
    val lIdxPeaks = Seq(Some(idxIsotope0), idxIsotope1, idxIsotope2, idxIsotope3)
    PeakIdentification(
      scan.num,
      lIdxPeaks.flatten,
      lIdxPeaks.zipWithIndex.flatMap {
        case (idxOption, isotopeNum) => idxOption match {
          case Some(idx) =>
            Some(Peak(
              isotopeNum,
              intensities(idx),
              intensities(idx) / basePeakIntensity,
              mz(idx)
            ))
          case None => None
        }
      },
      scan.rt
    )
  }

  /**
   * Index of the m/z value closest to `target` in a sorted ascending m/z array.
   */
  private def findClosestMzIdx(mzValues: Array[Double], target: Double): Int = {
    if (mzValues.isEmpty) 0
    else {
      var lo = 0
      var hi = mzValues.length - 1
      while (lo < hi) {
        val mid = (lo + hi) >>> 1
        if (mzValues(mid) < target) lo = mid + 1 else hi = mid
      }
      if (lo == 0) 0
      else if (math.abs(mzValues(lo) - target) < math.abs(mzValues(lo - 1) - target)) lo
      else lo - 1
    }
  }

  def calcBackgroundNoisePeak(
                                 source: MZXMLFile,
                                 index: MZXMLIndex,
                                 startDurationTime : Double
                               ): Double = {
    val allScans =
      scansMs(source,index,Some(0),Some(startDurationTime),2)
        .map {
          scanMs2 =>source.parseScan(scanMs2.getNum, true)
        }
        .filter {
          scanObj => scanObj != null
        }
        .map {
          scanObj => scanObj.fetchSpectrum()
        }
        .filter {
          spectrum => spectrum !=  null
        }
        .map {
          spectrum => spectrum.getSumInt/spectrum.getIntensities.length
        }

    val mean = allScans.size match {
      case _ if allScans.nonEmpty=> allScans.sum/allScans.size
      case _ => 0
    }
    val std = sqrt(allScans.map( v => (v - mean)*(v - mean) ).sum / allScans.size)
    println(" ======= BackgroundNoisePeak ==========")
    println(s"=====   mean = $mean std = $std =========")
    if (mean != mean) {
      throw new Exception("Can not compute Mean with durationTime (seconds):"+startDurationTime)
    }
    mean
  }

  /**
   * Compute the background noise intensity using the MS2 spectra acquired
   * during the warm-up period (RT between 0 and startDurationTime).
   *
   * When no MS2 spectrum falls in the warm-up window (e.g. the acquisition
   * of the file starts later than `startDurationTime`), the whole MS2 scan
   * set of the run is used as a fallback so that a meaningful (non-zero)
   * noise estimate is still computed instead of silently returning 0.
   */
  def calcBackgroundNoisePeak(spectrumData: SpectrumData, startDurationTime: Double): Double = {
    val windowedMs2: Seq[Double] =
      spectrumData.scans
        .filter(_.msLevel == 2)
        .filter(scan => scan.rt >= 0.0 && scan.rt <= startDurationTime)
        .filter(_.peaks.nonEmpty)
        .map(scan => scan.peaks.map(_.intensity).sum / scan.peaks.size)

    val allScans: Seq[Double] =
      if (windowedMs2.nonEmpty) windowedMs2
      else spectrumData.scans
        .filter(_.msLevel == 2)
        .filter(_.peaks.nonEmpty)
        .map(scan => scan.peaks.map(_.intensity).sum / scan.peaks.size)

    val mean = allScans.size match {
      case _ if allScans.nonEmpty => allScans.sum / allScans.size
      case _ => 0.0
    }
    val std = sqrt(allScans.map(v => (v - mean) * (v - mean)).sum / allScans.size)
    println(" ======= BackgroundNoisePeak ==========")
    println(s"=====   mean = $mean std = $std =========")
    if (mean != mean) {
      throw new Exception("Can not compute Mean with durationTime (seconds):" + startDurationTime)
    }
    mean
  }

  def selectEligibleIons(
                                          source: MZXMLFile,
                                          index: MZXMLIndex,
                                          start : Option[Double] = None,
                                          end : Option[Double] = None,
                                          noiseIntensity : Double,
                                          nbCarbonMin: Double,
                                          nbCarbonMax: Double,
                                          nbSulfurMin : Double,
                                          nbSulfurMax : Double,
                                          precision: Double = 0.01,
                                          deltaMOM2 : Double
                                        ): Seq[PeakIdentification] = {
    println("\n== Search for isotopes sulfur == ")
    val allScans = scansMs(source,index,start,end,1)

    allScans.zipWithIndex.flatMap {
      case (basicScan, i) =>
        print(s"\r===>$i/${allScans.size}")
        val scan = source.parseScan(basicScan.getNum, true)
        val spectrum = scan.fetchSpectrum()
        val mzValues = spectrum.getMZs

        // remove the first one to compute Delta M
        mzValues
          .zipWithIndex
          .filter { case (_, idx) =>
            spectrum.getIntensities()(idx) > noiseIntensity
          }
          .map { case (mz0, idx0) =>
            val mz_ms_p2 = mz0 + deltaMOM2
            val idx2 = spectrum.findClosestMzIdx(mz_ms_p2)
            val mz_p2 = spectrum.getMZs()(idx2)
            val idx1 = spectrum.findClosestMzIdx(mz0 + 1.0)
            val mz1 = spectrum.getMZs()(idx1)
            (mz0, idx0, mz1, idx1, mz_p2, idx2)
          }
          .filter { case (mz, _, _, _, mz2, _) =>
            ((mz - mz2).abs - deltaMOM2).abs < precision
          }
          /* criteria M1 of Isotope C are present at 1.1 and S are present 4.4 % */
          .filter { case (_, idx0, _, idx1, _, _) =>

            spectrum.getIntensities()(idx1) >= spectrum.getIntensities()(idx0) *
              (ChemicalUtils.abundanceIsotope("C")(1) * nbCarbonMin +
                ChemicalUtils.abundanceIsotope("S")(1) * nbSulfurMin)
          }
          .filter { case (_, idx0, _, idx1, _, _) =>
            spectrum.getIntensities()(idx1) <
              spectrum.getIntensities()(idx0) *
                (ChemicalUtils.abundanceIsotope("C")(1) * nbCarbonMax +
                  ChemicalUtils.abundanceIsotope("S")(1) * nbSulfurMax)
          }
          /* criteria M2 of Isotope S are present 4.4 % */
          .filter { case (_, idx0, _, _, _, idx2) =>
            spectrum.getIntensities()(idx2) >= spectrum.getIntensities()(idx0) *
              ChemicalUtils.abundanceIsotope("S")(2) * nbSulfurMin
          }
          .filter { case (_, idx0, _, _, _, idx2) =>
            spectrum.getIntensities()(idx2) < spectrum.getIntensities()(idx0) *
              ChemicalUtils.abundanceIsotope("S")(2) * nbSulfurMax
          }
          .map {
            case (mz0, idx0, _, idx1, _, idx2) =>
              val idx3 = spectrum.findClosestMzIdx(mz0 + 3.0)
              (idx0,idx1,idx2,idx3)
          }
          .map { case (idx0,idx1,idx2,idx3) =>
            fillPeakIdentification(scan, spectrum, idx0, Some(idx1), Some(idx2), Some(idx3))
          }
    }
  }

  /**
   * Same as the MZXML based [[selectEligibleIons]] but working on a
   * unified [[SpectrumData]] structure.
   */
  def selectEligibleIons(
                          spectrumData: SpectrumData,
                          start: Option[Double],
                          end: Option[Double],
                          noiseIntensity: Double,
                          nbCarbonMin: Double,
                          nbCarbonMax: Double,
                          nbSulfurMin: Double,
                          nbSulfurMax: Double,
                          precision: Double,
                          deltaMOM2: Double
                        ): Seq[PeakIdentification] = {
    println("\n== Search for isotopes sulfur == ")
    val allScans = spectrumData.scans
      .filter(_.msLevel == 1)
      .filter(scan => start.forall(_ <= scan.rt))
      .filter(scan => end.forall(_ >= scan.rt))

    allScans.zipWithIndex.flatMap {
      case (scan, i) =>
        print(s"\r===>$i/${allScans.size}")
        if (scan.peaks.isEmpty) Seq.empty
        else {
          val sortedPeaks = scan.peaks.sortBy(_.mz)
          val intensities = sortedPeaks.map(_.intensity).toArray
          val mzValues = sortedPeaks.map(_.mz).toArray
          val basePeakIntensity = intensities.max

          mzValues.zipWithIndex
            .filter { case (_, idx) => intensities(idx) > noiseIntensity }
            .map { case (mz0, idx0) =>
              val mz_ms_p2 = mz0 + deltaMOM2
              val idx2 = findClosestMzIdx(mzValues, mz_ms_p2)
              val mz_p2 = mzValues(idx2)
              val idx1 = findClosestMzIdx(mzValues, mz0 + 1.0)
              val mz1 = mzValues(idx1)
              (mz0, idx0, mz1, idx1, mz_p2, idx2)
            }
            .filter { case (mz, _, _, _, mz2, _) =>
              ((mz - mz2).abs - deltaMOM2).abs < precision
            }
            /* criteria M1 of Isotope C are present at 1.1 and S are present 4.4 % */
            .filter { case (_, idx0, _, idx1, _, _) =>
              intensities(idx1) >= intensities(idx0) *
                (ChemicalUtils.abundanceIsotope("C")(1) * nbCarbonMin +
                  ChemicalUtils.abundanceIsotope("S")(1) * nbSulfurMin)
            }
            .filter { case (_, idx0, _, idx1, _, _) =>
              intensities(idx1) <
                intensities(idx0) *
                  (ChemicalUtils.abundanceIsotope("C")(1) * nbCarbonMax +
                    ChemicalUtils.abundanceIsotope("S")(1) * nbSulfurMax)
            }
            /* criteria M2 of Isotope S are present 4.4 % */
            .filter { case (_, idx0, _, _, _, idx2) =>
              intensities(idx2) >= intensities(idx0) *
                ChemicalUtils.abundanceIsotope("S")(2) * nbSulfurMin
            }
            .filter { case (_, idx0, _, _, _, idx2) =>
              intensities(idx2) < intensities(idx0) *
                ChemicalUtils.abundanceIsotope("S")(2) * nbSulfurMax
            }
            .map {
              case (mz0, idx0, _, idx1, _, idx2) =>
                val idx3 = findClosestMzIdx(mzValues, mz0 + 3.0)
                (idx0, idx1, idx2, idx3)
            }
            .map { case (idx0, idx1, idx2, idx3) =>
              fillPeakIdentification(scan, mzValues, intensities, basePeakIntensity,
                idx0, Some(idx1), Some(idx2), Some(idx3))
            }
            .toSeq
        }
    }
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
      case (basicScan, i) =>
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
            case (delta, _) =>
              val idx1 = spectrum.findClosestMzIdx(delta)
              spectrum.getIntensities()(idx1) <= intensityFilter
          }
          .map( _._1)
    }
  }

  /**
   * Merge all M/z in a short Windows RT (features) and keep the Ions with the maximum abundance
   * @param peaks peak list to analyse and merge features
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
                                 peaks: Seq[PeakIdentification],
                                 noiseIntensity : Double,
                                 threshold: Int,
                                 nls: Seq[(String, Double)],
                                 dis: Seq[(String, Double)]
                               ): IonsIdentificationBuilder = {

    println(s"\n=== filterOverRepresentedPeak == threshold=$threshold size=${peaks.length}")

    val allScans = scansMs(source, index,None,None, 1)

    val countAllPeak: Seq[Int] =
      allScans
        .zipWithIndex
        .map {
          case (scanR,i) =>
            val scan = source.parseScan(scanR.getNum, true)
            print(s"\r===>$i/${allScans.size}")
            val spectrum = scan.fetchSpectrum()

            peaks.map(_.peaks.head.mz)
              .map(mz => {
                val idx = spectrum.findClosestMzIdx(mz)
                if (spectrum.getIntensities()(idx) > noiseIntensity)
                  1
                else
                  0
              })
        }
        /* count all peak over the chromatogram */
        .foldLeft(peaks.indices.map(_ => 0))(
          (s, elt) => {
            s.zipWithIndex.map { case (e, i) => e + elt(i) }
          }
        )

    val newL = peaks.zipWithIndex filter {
      case (_, i) => countAllPeak(i) < threshold
    } map {
      case (p, _) => p
    }

    println(s" new size:${newL.length}")
    IonsIdentificationBuilder(source, index,newL,nls,dis,noiseIntensity=noiseIntensity)
  }

  /**
   * Same as the MZXML based [[filterOverRepresentedPeak]] but working on a
   * unified [[SpectrumData]] structure.
   */
  def filterOverRepresentedPeak(
                                 spectrumData: SpectrumData,
                                 peaks: Seq[PeakIdentification],
                                 noiseIntensity: Double,
                                 threshold: Int,
                                 nls: Seq[(String, Double)],
                                 dis: Seq[(String, Double)]
                               ): IonsIdentificationBuilder = {

    println(s"\n=== filterOverRepresentedPeak == threshold=$threshold size=${peaks.length}")

    val allScans = spectrumData.scans.filter(_.msLevel == 1)

    val countAllPeak: Seq[Int] =
      allScans
        .zipWithIndex
        .map {
          case (scan, i) =>
            print(s"\r===>$i/${allScans.size}")
            if (scan.peaks.isEmpty) peaks.indices.map(_ => 0)
            else {
              val sortedPeaks = scan.peaks.sortBy(_.mz)
              val intensities = sortedPeaks.map(_.intensity).toArray
              val mzValues = sortedPeaks.map(_.mz).toArray
              peaks.map(_.peaks.head.mz)
                .map(mz => {
                  val idx = findClosestMzIdx(mzValues, mz)
                  if (intensities(idx) > noiseIntensity)
                    1
                  else
                    0
                })
            }
        }
        /* count all peak over the chromatogram */
        .foldLeft(peaks.indices.map(_ => 0))(
          (s, elt) => s.zipWithIndex.map { case (e, i) => e + elt(i) }
        )

    val newL = peaks.zipWithIndex filter {
      case (_, i) => countAllPeak(i) < threshold
    } map {
      case (p, _) => p
    }

    println(s" new size:${newL.length}")
    IonsIdentificationBuilder.fromSpectrumData(spectrumData, newL, nls, dis, noiseIntensity)
  }

  def round100(v : Double) : Double = (v*100).round / 100.toDouble

  def searchIons(source: MZXMLFile,
                 l: Seq[IScan],
                 mzSearch:Double,
                 tolMzh: Double,
                 noiseIntensity : Double,
                ): Option[(Double,Double)] = {
    l.flatMap {
      scanMs2 =>
        val scan2 = source.parseScan(scanMs2.getNum, true)
        scan2.getSpectrum match {
          case spectrum if spectrum != null =>
            val v = spectrum.findClosestMzIdx(mzSearch)
            val mz = spectrum.getMZs()(v)
            if ( (mzSearch - mz).abs < tolMzh) {
              if (spectrum.getIntensities()(v)>noiseIntensity)
                Some((round100(mz),round100(spectrum.getIntensities()(v)/scan2.getBasePeakIntensity)))
              else
                None
            } else None
          case _ => None
        }
    }.sorted.lastOption // take the biggest value
  }

  /**
   * Same as the MZXML based [[searchIons]] but working on a unified
   * [[SpectrumData]] structure.
   */
  def searchIons(spectrumData: SpectrumData,
                 l: Seq[SpectrumScan],
                 mzSearch: Double,
                 tolMzh: Double,
                 noiseIntensity: Double,
                ): Option[(Double, Double)] = {
    l.flatMap {
      scan =>
        if (scan.peaks.isEmpty) None
        else {
          val sortedPeaks = scan.peaks.sortBy(_.mz)
          val intensities = sortedPeaks.map(_.intensity).toArray
          val mzValues = sortedPeaks.map(_.mz).toArray
          val v = findClosestMzIdx(mzValues, mzSearch)
          val mz = mzValues(v)
          if ((mzSearch - mz).abs < tolMzh) {
            if (intensities(v) > noiseIntensity)
              Some((round100(mz), round100(intensities(v) / intensities.max)))
            else
              None
          } else None
        }
    }.sorted.lastOption // take the biggest value
  }

  def detectNeutralLoss(
                         source: MZXMLFile,
                         index : MZXMLIndex,
                         p : PeakIdentification,
                         nls : Seq[(String,Double)], /* name, distance */
                         tolMzh: Double,
                         precisionRtTime : Double = 0.02,
                         noiseIntensity : Double,
                       ) : Map[String,Option[(String,Double,Double)]] = {

    val sc = source.parseScan(p.numScan, false)
    val scanMs2: Seq[IScan] = ScanLoader.scansMs(
      source, index, Some(sc.getRt - precisionRtTime), Some(sc.getRt + precisionRtTime), 2
    )

    val mz = p.peaks.head.mz

    nls.map (
      nl => {
        nl._1->
          searchIons(source,scanMs2,mz - nl._2,tolMzh,noiseIntensity)
          .map( x => (nl._1,x._1,x._2) )
      }
    ).toMap
  }

  /**
   * Same as the MZXML based [[detectNeutralLoss]] but working on a unified
   * [[SpectrumData]] structure.
   */
  def detectNeutralLoss(
                         spectrumData: SpectrumData,
                         p: PeakIdentification,
                         nls: Seq[(String, Double)], /* name, distance */
                         tolMzh: Double,
                         precisionRtTime: Double,
                         noiseIntensity: Double,
                       ): Map[String, Option[(String, Double, Double)]] = {

    val scanMs2: Seq[SpectrumScan] = spectrumData.scans
      .filter(_.msLevel == 2)
      .filter(scan => scan.rt >= p.rt - precisionRtTime && scan.rt <= p.rt + precisionRtTime)

    val mz = p.peaks.head.mz

    nls.map(
      nl => {
        nl._1 ->
          searchIons(spectrumData, scanMs2, mz - nl._2, tolMzh, noiseIntensity)
            .map(x => (nl._1, x._1, x._2))
      }
    ).toMap
  }

  def detectDaughterIons(
                          source: MZXMLFile,
                          index: MZXMLIndex,
                          p: PeakIdentification,
                          dis: Seq[(String,Double)], /* name , mz */
                          tolMzh: Double,
                          precisionRtTime: Double = 0.02,
                          noiseIntensity : Double,
                        ): Map[String, Option[(String,Double,Double)]] = {

    val sc = source.parseScan(p.numScan, false)
    val scanMs2: Seq[IScan] = ScanLoader.scansMs(
      source, index, Some(sc.getRt - precisionRtTime), Some(sc.getRt + precisionRtTime), 2
    )

    dis.map(
      di => {
        di._1->
          searchIons(source,scanMs2,di._2,tolMzh,noiseIntensity)
          .map( x => (di._1,x._1,x._2) )
      }
    ).toMap
  }

  /**
   * Same as the MZXML based [[detectDaughterIons]] but working on a unified
   * [[SpectrumData]] structure.
   */
  def detectDaughterIons(
                          spectrumData: SpectrumData,
                          p: PeakIdentification,
                          dis: Seq[(String, Double)], /* name , mz */
                          tolMzh: Double,
                          precisionRtTime: Double,
                          noiseIntensity: Double,
                        ): Map[String, Option[(String, Double, Double)]] = {

    val scanMs2: Seq[SpectrumScan] = spectrumData.scans
      .filter(_.msLevel == 2)
      .filter(scan => scan.rt >= p.rt - precisionRtTime && scan.rt <= p.rt + precisionRtTime)

    dis.map(
      di => {
        di._1 ->
          searchIons(spectrumData, scanMs2, di._2, tolMzh, noiseIntensity)
            .map(x => (di._1, x._1, x._2))
      }
    ).toMap
  }
}
