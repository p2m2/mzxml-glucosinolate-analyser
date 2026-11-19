package fr.inrae.metabolomics.p2m2.fileformats

import java.io.File
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}
import umich.ms.datatypes.scan.IScan
import umich.ms.datatypes.spectrum.ISpectrum
import scala.jdk.CollectionConverters._
import scala.util.{Success, Try}

class MzXmlReader extends SpectrumFileReader {
  
  override def read(file: File): SpectrumData = {
    val (source, index) = readMzXmlFile(file)
    
    val scans = extractScans(source, index)
    val metadata = extractMetadata(source, file)
    
    SpectrumData(scans, metadata)
  }
  
  override def supports(file: File): Boolean = {
    file.getName.toLowerCase.endsWith(".mzxml")
  }
  
  private def readMzXmlFile(file: File): (MZXMLFile, MZXMLIndex) = {
    val source: MZXMLFile = new MZXMLFile(file.getPath)
    val index: MZXMLIndex = source.fetchIndex()
    (source, index)
  }
  
  private def extractScans(source: MZXMLFile, index: MZXMLIndex): Seq[Scan] = {
    // Des scans MS1 et MS2 : la structure est d'abord sondée avec STRUCTURE_ONLY
    // (parseScan(num, false)) puis le spectre complet est re-parsé (parseScan(num, true))
    // avant fetchSpectrum, comme le font les méthodes historiques de ScanLoader.
    val ms1Scans = getScansByMsLevel(source, index, 1)
    val ms2Scans = getScansByMsLevel(source, index, 2)

    // Fusionner les scans
    (ms1Scans ++ ms2Scans).flatMap(scan => {
      val spectrum = Try(source.parseScan(scan.getNum, true)).toOption
        .flatMap(s => Option(s.fetchSpectrum()))
      spectrum match {
        case None => None
        case Some(s) =>
          val peaks = s.getMZs.zipWithIndex.map { case (mz, idx) =>
            Peak(mz, s.getIntensities()(idx))
          }.toSeq.sortBy(_.mz)
          Some(Scan(scan.getNum, scan.getRt, scan.getMsLevel, peaks))
      }
    })
  }
  
  private def getScansByMsLevel(source: MZXMLFile, index: MZXMLIndex, msLevel: Int): Seq[IScan] = {
    index.getMapByRawNum
      .keySet()
      .asScala
      .flatMap(scanNumRaw => Try(source.parseScan(scanNumRaw, false)) match {
        case Success(scan) => Some(scan)
        case _ => None
      })
      .filter(_.getMsLevel == msLevel)
      .toSeq
  }
  
  private def extractMetadata(source: MZXMLFile, file: File): Metadata = {
    val runInfo = source.parseRunInfo()
    val instrument = runInfo.getInstruments.values().asScala.map { instrument =>
      s"${instrument.getModel} - ${instrument.getManufacturer}"
    }.mkString(", ")
    
    Metadata(instrument, "", file.getName, file.getAbsolutePath)
  }
}
