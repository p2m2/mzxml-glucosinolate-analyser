package fr.inrae.metabolomics.p2m2.builder

import umich.ms.datatypes.scan.IScan
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

case class MetaboliteIdentification(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     peaks : Seq[PeakIdentification]
                                   ) {

  def getInfo( p :PeakIdentification) : Seq[Any] = {
    val scan : IScan = source.parseScan(p.numScan, true)

    val gluconolactone_178 = ScanLoader.detectNeutralLoss(source,p,178)
    val sulfureTrioxide_80 = ScanLoader.detectNeutralLoss(source,p,80)
    val anhydroglucose_162 = ScanLoader.detectNeutralLoss(source,p,162)
    val thioglucose_s03_242 = ScanLoader.detectNeutralLoss(source,p,242)
    val thioglucose_196 = ScanLoader.detectNeutralLoss(source,p,196)
    val last_223 = ScanLoader.detectNeutralLoss(source,p,223)

    val spectrum = scan.fetchSpectrum()

    val intSpec0 = spectrum.getIntensities()(p.indexFirstIsotopeInSpectrum)
    val intSpec1 = spectrum.getIntensities()(p.indexFirstIsotopeInSpectrum + 1)
    val intSpec2 = spectrum.getIntensities()(p.indexFirstIsotopeInSpectrum + 2)

    Seq(
      spectrum.getMZs()(p.indexFirstIsotopeInSpectrum),
      intSpec0,
      (intSpec0/scan.getBasePeakIntensity) * 100.0,
      spectrum.getMZs()(p.indexFirstIsotopeInSpectrum+1),
      intSpec1,
      (intSpec1 / scan.getBasePeakIntensity) * 100.0,
      spectrum.getMZs()(p.indexFirstIsotopeInSpectrum+2),
      intSpec2,
      (intSpec2 / scan.getBasePeakIntensity) * 100.0,
      gluconolactone_178,
      sulfureTrioxide_80,
      anhydroglucose_162,
      thioglucose_s03_242,
      thioglucose_196,
      last_223)
  }

  def getInfos() : Seq[Seq[Any]] = {
    peaks.map( getInfo )
  }
}
