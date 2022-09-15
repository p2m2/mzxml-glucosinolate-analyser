package fr.inrae.metabolomics.p2m2.builder

import fr.inrae.metabolomics.p2m2.builder.GLSRelatedDiagnostic.GLSRelatedDiagnostic.NLs
import fr.inrae.metabolomics.p2m2.output.CsvMetabolitesIdentification
import umich.ms.datatypes.scan.IScan
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

case class MetaboliteIdentification(
                                     source : MZXMLFile,
                                     index : MZXMLIndex,
                                     peaks : Seq[PeakIdentification]
                                   ) {

  def getInfo( p :PeakIdentification) : CsvMetabolitesIdentification = {
    val scan : IScan = source.parseScan(p.numScan, true)
/*
    val gluconolactone_178 = ScanLoader.detectNeutralLoss(source,index,p,178)
    val sulfureTrioxide_80 = ScanLoader.detectNeutralLoss(source,index,p,80)
    val anhydroglucose_162 = ScanLoader.detectNeutralLoss(source,index,p,162)
    val thioglucose_s03_242 = ScanLoader.detectNeutralLoss(source,index,p,242)
    val thioglucose_196 = ScanLoader.detectNeutralLoss(source,index,p,196)
    val last_223 = ScanLoader.detectNeutralLoss(source,index,p,223)
*/

    val spectrum = scan.fetchSpectrum()
    val mz = p.indexIsotopeInSpectrum.map(idx => spectrum.getMZs()(idx))
    val intensities = p.indexIsotopeInSpectrum.map(idx => spectrum.getIntensities()(idx))
    val abundance = intensities.map( i => (i / scan.getBasePeakIntensity) )
    CsvMetabolitesIdentification(
      mz,
      intensities,
      abundance,
      scan.getRt,
      neutralLosses = Map()/* Map(
        NLs.gluconolactone -> gluconolactone_178,
        NLs.sulfureTrioxide -> sulfureTrioxide_80,
        NLs.anhydroglucose -> anhydroglucose_162,
        NLs.thioglucose_s03 -> thioglucose_s03_242,
        NLs.thioglucose -> thioglucose_196,
        NLs.glucosinolate_223 -> last_223
      )*/,
      daughterIons = Map()
    )
  }

  def getInfos() : Seq[CsvMetabolitesIdentification] = {
    peaks.map( getInfo ).sortBy(_.rt)
  }
}
