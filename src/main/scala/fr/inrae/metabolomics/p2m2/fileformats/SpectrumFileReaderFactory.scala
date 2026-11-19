package fr.inrae.metabolomics.p2m2.fileformats

import java.io.File

object SpectrumFileReaderFactory {
  
  private val readers: Seq[SpectrumFileReader] = Seq(
    new MzXmlReader(),
    new MzMLReader(),
    new XcmsReader()
  )
  
  def getReaderForFile(file: File): Option[SpectrumFileReader] = {
    readers.find(_.supports(file))
  }
  
  def getAllReaders: Seq[SpectrumFileReader] = readers
}