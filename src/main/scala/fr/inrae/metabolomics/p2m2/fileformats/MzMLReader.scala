package fr.inrae.metabolomics.p2m2.fileformats

import java.io.File
import uk.ac.ebi.jmzml.model.mzml.{BinaryDataArray, MzML, Spectrum => JmzMlSpectrum}
import uk.ac.ebi.jmzml.xml.io.MzMLUnmarshaller

import scala.jdk.CollectionConverters._
import scala.util.Try

class MzMLReader extends SpectrumFileReader {

  override def read(file: File): SpectrumData = {
    try {
      val mzML: MzML = new MzMLUnmarshaller(file).unmarshall()

      val spectra = Option(mzML.getRun)
        .flatMap(run => Option(run.getSpectrumList))
        .map(_.getSpectrum.asScala.toSeq)
        .getOrElse(Seq.empty)

      val scans = spectra.flatMap(extractScan)

      SpectrumData(scans, extractMetadata(mzML, file))
    } catch {
      case ex: Throwable =>
        println(s"Error reading MZML file ${file.getAbsolutePath}: ${ex.getMessage}")
        SpectrumData(Seq.empty, Metadata("", "", file.getName, file.getAbsolutePath))
    }
  }

  override def supports(file: File): Boolean = {
    file.getName.toLowerCase.endsWith(".mzml")
  }

  private def extractScan(spectrum: JmzMlSpectrum): Option[Scan] = {
    getMsLevel(spectrum) match {
      case Some(msLevel) =>
        val (mzs, intensities) = getPeaks(spectrum)
        val peaks = mzs.zip(intensities).sortBy(_._1).map { case (mz, intensity) =>
          Peak(mz, intensity)
        }
        Some(Scan(getScanNumber(spectrum), getRetentionTimeInMinutes(spectrum), msLevel, peaks))
      case None => None
    }
  }

  /**
   * MS level (MS:1000511), defaulting to 1 when it can not be determined.
   *
   * Most writers put the cvParam on the `spectrum` element, but some converters
   * (e.g. custom mzML exporters) only declare it inside the `<scan>` element —
   * both locations are checked so that such files do not end up with all their
   * spectra mis-detected as MS1 (which silently breaks noise/detection).
   *
   * Spectra that do not carry any ms-level cvParam and are flagged as non-MS
   * (e.g. UV/vis traces from a second controller, MS:1000804) are rejected so
   * they do not pollute the MS1/MS2 pools.
   */
  private def getMsLevel(spectrum: JmzMlSpectrum): Option[Int] = {
    val spectrumParams: Seq[uk.ac.ebi.jmzml.model.mzml.CVParam] =
      Option(spectrum.getCvParam).map(_.asScala.toSeq).getOrElse(Seq.empty)

    val scanParams: Seq[uk.ac.ebi.jmzml.model.mzml.CVParam] =
      Option(spectrum.getScanList)
        .flatMap(sl => Option(sl.getScan))
        .map(_.asScala.toSeq.flatMap(scan => Option(scan.getCvParam).map(_.asScala.toSeq).getOrElse(Seq.empty)))
        .getOrElse(Seq.empty)

    val allParams = spectrumParams ++ scanParams
    val accessions = allParams.flatMap(p => Option(p.getAccession))
    val msLevel = allParams
      .find(p => Option(p.getAccession).contains("MS:1000511"))
      .flatMap(p => Option(p.getValue))
      .flatMap(v => Try(v.toInt).toOption)
      .filter(l => l == 1 || l == 2)

    msLevel.orElse {
      // No ms level declared: only keep genuine MS spectra (MS:1000804 =
      // electromagnetic radiation spectrum, e.g. UV/vis from a diode array).
      if (accessions.contains("MS:1000804")) None
      else Some(1)
    }
  }

  /** Scan start time (MS:1000016), expressed in minutes (RT is handled in minutes internally). */
  private def getRetentionTimeInMinutes(spectrum: JmzMlSpectrum): Double = {
    val scanTime = Option(spectrum.getScanList)
      .flatMap(sl => Option(sl.getScan))
      .flatMap(_.asScala.headOption)
      .flatMap(scan => Option(scan.getCvParam).map(_.asScala.toSeq).getOrElse(Seq.empty)
        .find(p => Option(p.getAccession).contains("MS:1000016")))

    scanTime match {
      case Some(p) =>
        Option(p.getValue)
          .flatMap(v => Try(v.toDouble).toOption)
          .map { value =>
            // The default unit for MS:1000016 is "second" (UO:0000010); minutes are used internally.
            val unit =
              Option(p.getUnitName).orElse(Option(p.getUnitAccession)).getOrElse("").toLowerCase
            if (unit.contains("min")) value else value / 60.0
          }
          .getOrElse(0.0)
      case None => 0.0
    }
  }

  private def getScanNumber(spectrum: JmzMlSpectrum): Int = {
    Option(spectrum.getIndex)
      .map(_.intValue())
      .getOrElse(extractNumberFromId(spectrum.getId))
  }

  private def extractNumberFromId(id: String): Int = {
    Try("""\d+""".r.findAllIn(Option(id).getOrElse("")).toSeq.last.toInt).getOrElse(0)
  }

  /** Extract the m/z and intensity arrays (MS:1000514 / MS:1000515). */
  private def getPeaks(spectrum: JmzMlSpectrum): (Seq[Double], Seq[Double]) = {
    val arrays = Option(spectrum.getBinaryDataArrayList)
      .map(_.getBinaryDataArray.asScala.toSeq)
      .getOrElse(Seq.empty)

    var mzs: Seq[Double] = Seq.empty
    var intensities: Seq[Double] = Seq.empty
    arrays.foreach { array =>
      // The accession identifying the array (m/z vs intensity) may appear in any
      // position of the cvParam list (writers order it differently, some also add
      // data-type/compression cvParams first), so search all of them.
      val accessions = Option(array.getCvParam)
        .map(_.asScala.toSeq.flatMap(p => Option(p.getAccession)))
        .getOrElse(Seq.empty)
      val values = Try(Option(array.getBinaryDataAsNumberArray)
        .map(_.map(_.doubleValue()).toSeq)
        .getOrElse(Seq.empty[Double])).getOrElse(Seq.empty[Double])
      if (accessions.contains(BinaryDataArray.MS_MZ_VALUE_AC)) mzs = values
      else if (accessions.contains(BinaryDataArray.MS_INTENSITY_AC)) intensities = values
    }
    (mzs, intensities)
  }

  private def extractMetadata(mzML: MzML, file: File): Metadata = {
    val instrument = Option(mzML.getRun)
      .flatMap(run => Option(run.getDefaultInstrumentConfiguration))
      .map(_.getId)
      .getOrElse("")

    Metadata(instrument, "", file.getName, file.getAbsolutePath)
  }
}
