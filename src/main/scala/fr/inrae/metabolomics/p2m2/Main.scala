package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.`export`.{CsvIonsIdentificationFile, IonsIdentificationFile}
import fr.inrae.metabolomics.p2m2.builder.{IonsIdentificationBuilder, PeakIdentification, ScanLoader}
import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

import java.io.File
import scala.io.Source

object Main extends App {

  import scopt.OParser

  case class Config(
                     mzfiles: Seq[File] = Seq(),
                     jsonFamilyMetabolitesDetection: Option[File] = None,
                     thresholdIntensityFilter: Option[Int] = None,
                     thresholdAbundanceM0Filter: Double = 0.1,
                     overrepresentedPeakFilter: Int = 800,
                     startRT: Option[Double] = None,
                     endRT: Option[Double] = None,
                     precisionMzh: Int = 1000,
                     toleranceMz: Double = 0.01,
                     outfile: Option[File] = None,
                     verbose: Boolean = false,
                     debug: Boolean = false
                   )

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("mzxml-glucosinolates-phenolics-analyser"),
      head("mzxml-glucosinolates-phenolics-analyser", "1.0"),
      opt[File]('j', "jsonFamilyMetabolitesDetection")
        .optional()
        .action((x, c) => c.copy(jsonFamilyMetabolitesDetection = Some(x)))
        .text(s"json configuration to detect metabolite family."),
      opt[Int]('i', "thresholdIntensityFilter")
        .optional()
        .action((x, c) => c.copy(thresholdIntensityFilter = Some(x)))
        .text(s"Keep ions above a x intensity (calculation on start-up time)"),
      opt[Int]('p', "overrepresentedPeakFilter")
        .optional()
        .action((x, c) => c.copy(overrepresentedPeakFilter = x))
        .text(s"filter about over represented peaks. default ${Config().overrepresentedPeakFilter}"),
      opt[Double]('s', "startRT")
        .optional()
        .action((x, c) => c.copy(startRT = Some(x)))
        .text(s"start RT"),
      opt[Double]('e', "endRT")
        .optional()
        .action((x, c) => c.copy(endRT = Some(x)))
        .text(s"start RT"),
      opt[Int]('m', "precisionMzh")
        .optional()
        .action((x, c) => c.copy(precisionMzh = x))
        .text(s"precision/rounded Mzh (number to the right of the decimal point) . ${Config().precisionMzh}"),
      opt[Double]('t', "toleranceMz")
        .optional()
        .action((x, c) => c.copy(toleranceMz = x))
        .text(s"tolerance accepted. ${Config().toleranceMz}"),
      opt[File]('o', "outputFile")
        .optional()
        .action((x, c) => c.copy(outfile = Some(x)))
        .text(s"output path file."),
      opt[Unit]("verbose")
        .optional()
        .action((_, c) => c.copy(verbose = true))
        .text("verbose is a flag"),
      opt[Unit]("debug")
        .hidden()
        .action((_, c) => c.copy(debug = true))
        .text("this option is hidden in the usage text"),

      arg[File]("<file>...")
        .unbounded()
        .action((x, c) => c.copy(mzfiles = c.mzfiles :+ x)),
      help("help").text("prints this usage text"),
      note("some notes." + sys.props("line.separator")),
      checkConfig(_ => success)
    )
  }

  // OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      process(config)
    // do something
    case _ => System.err.println("Ko")
    // arguments are bad, error message will have been displayed
  }

  def process(config: Config): Unit = {

    val confJson = config.jsonFamilyMetabolitesDetection match {
      case Some(jsonFilePath) =>
        val s = Source.fromFile(jsonFilePath)
        val res = ConfigReader.read(s.getLines().mkString)
        s.close()
        res
      case None =>
        ConfigReader.read(
          Source.fromInputStream(
            getClass.getResource("/default.json")
              .openStream()).getLines().mkString)
    }

    confJson.metabolites.foreach(
      family => {
        config.mzfiles.foreach {
          mzFile =>
            val (source, index) = ScanLoader.read(mzFile)

            val intensityFilter = config.thresholdIntensityFilter match {
              case Some(v) => v
              case None => ScanLoader.calculBackgroundNoisePeak(source, index, config.startRT, config.endRT)
            }

            val values = analyse_metabolite(
              config,
              source,
              index,
              intensityFilter,
              confJson.deltaMp0Mp2(family),
              confJson.numberSulfurMin(family),
              confJson.minMzCoreStructure(family),
              confJson.neutralLoss(family),
              confJson.daughterIons(family)
            )

            val baseName = mzFile.getName.split("\\.").dropRight(1).mkString(".")

            val f = config.outfile.getOrElse(new File(s"${baseName}_$family.csv"))
            f.delete()
            CsvIonsIdentificationFile.build(values, family, confJson, f)

            val f2 = config.outfile.getOrElse(new File(s"${baseName}_$family"))
            IonsIdentificationFile.save(values, family, confJson, f2)
            println(s"========= check ${f.getPath},${f2.getPath} ===============")
        }

      })
  }


  def analyse_metabolite(
                          config: Config,
                          source: MZXMLFile,
                          index: MZXMLIndex,
                          intensityFilter: Int,
                          deltaMp0Mp2: Double,
                          numberSulfurMin: Double,
                          mzCoreStructure : Double,
                          neutralLoss: Map[String, Double],
                          daughterIons: Map[String, Double]
                        ): Seq[IonsIdentification] = {

    val listSulfurMetabolites: Seq[PeakIdentification] =
      ScanLoader.
        getScanIdxAndSpectrumM0M2WithDelta(
          source,
          index,
          config.startRT,
          config.endRT,
          config.thresholdAbundanceM0Filter,
          intensityFilter,
          filteringOnNbSulfur = numberSulfurMin.toInt,
          config.toleranceMz,
          deltaMOM2 = deltaMp0Mp2)

    /* Diagnostics : Ions frequency on selected Scan peak detected ! */

    val frequencyOfMz: Seq[(Int, Int)] = Seq() // DaughterIonsDiag.IonsFrequencyOnSelectedScanPeakDetected(source,index,listSulfurMetabolites)
    println(frequencyOfMz)
    /* Attention c est lent..... peut etre a faire en option !!*/
    println("\n\n\n==============   Twenty Ions frequency on selected Scan peak detected =========================")
    println(frequencyOfMz.reverse.slice(1, 20).map {
      case (mz, freq) => (mz.toString + " m/z -> " + freq)
    }.mkString(" , "))

    val listSulfurMetabolitesSelected: Seq[PeakIdentification] = // listSulfurMetabolites
      ScanLoader.keepSimilarMzWithMaxAbundance(listSulfurMetabolites, config.precisionMzh)

    val m: IonsIdentificationBuilder =
      ScanLoader.filterOverRepresentedPeak(
        source,
        index,
        config.startRT,
        config.endRT,
        listSulfurMetabolitesSelected,
        intensityFilter,
        config.overrepresentedPeakFilter,
        neutralLoss.toSeq,
        daughterIons.toSeq
      )
    m.findDiagnosticIonsAndNeutralLosses(config.precisionMzh,mzCoreStructure)
  }
}
