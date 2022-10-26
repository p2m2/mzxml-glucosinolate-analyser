package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.`export`.{CandidateResume, CsvIonsIdentificationFile, IonsIdentificationFile}
import fr.inrae.metabolomics.p2m2.builder.{IonsIdentificationBuilder, PeakIdentification, ScanLoader}
import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import umich.ms.fileio.filetypes.mzxml.{MZXMLFile, MZXMLIndex}

import java.io.File
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.ParSeq
import scala.io.Source
import scala.util.{Failure, Success, Try}

object MainDetection extends App {

  import scopt.OParser

  case class Config(
                     mzfiles: Seq[File] = Seq(),
                     jsonFamilyMetabolitesDetection: Option[File] = None,
                     noiseIntensityFilter: Option[Double] = None,
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
      programName("detection-analyser"),
      head("detection-analyser", "1.0"),
      opt[File]('j', "jsonFamilyMetabolitesDetection")
        .optional()
        .action((x, c) => c.copy(jsonFamilyMetabolitesDetection = Some(x)))
        .text(s"json configuration to detect metabolite family."),
      opt[Double]('i', "noiseIntensityFilter")
        .optional()
        .action((x, c) => c.copy(noiseIntensityFilter = Some(x)))
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
        val allSelectedIons : Seq[(Double, ParSeq[(IonsIdentification,String)])] =
          config.mzfiles
            .par
            .flatMap {
            mzFile =>
              val baseName = getBaseName(mzFile.getName,family)

              val values :  ParSeq[IonsIdentification] = {
                (new File(s"$baseName").exists() && new File(s"$baseName.csv").exists())
                match {
                  case true =>
                    Try(IonsIdentificationFile.load(new File(s"$baseName"))._1.par) match {
                      case Success(v) => v
                      case Failure(e) =>
                        System.err.println("========== ===========")
                        System.err.println(e.getMessage()) ;
                        System.err.println(s"delete file : $baseName")
                        ParSeq()

                    }
                  case false =>

                    val (source, index) = ScanLoader.read(mzFile)

                    /**
                     * intensity noise is computed with the warm up time or give by the user .
                     */
                    val noiseIntensity : Double = config.noiseIntensityFilter match {
                      case Some(v) => v
                      case None => ScanLoader.calculBackgroundNoisePeak(source, index, startDurationTime = 0.20)
                    }

                    val values =
                      ionsDetection(family,config,confJson, source, index,noiseIntensity)

                    val f = config.outfile.getOrElse(new File(s"${baseName}.csv"))
                    f.delete()
                    CsvIonsIdentificationFile.build(values, family, confJson, f)
                    val f2 = config.outfile.getOrElse(new File(s"${baseName}"))
                    IonsIdentificationFile.save(values, family, confJson, f2)
                    println(s"========= check ${f.getPath},${f2.getPath} ===============")
                    values.par
                }
                //
              }
              val t = ((confJson.di(family).keys.size+confJson.nl(family).keys.size)*0.1).round
              values.filter( _.scoreIdentification > t ).map( (_,mzFile.getName) )
            }
            .groupBy{  case (ion : IonsIdentification,_: String) =>
            (ion.ion.peaks.head.mz*100).round/100.toDouble }
            .toList
            .sortBy { case (mz : Double , s: ParSeq[(IonsIdentification,String)]) => mz }
      //    .sortBy(_._1)

        CandidateResume.build(allSelectedIons,family,confJson,new File(s"resume_${family}.txt"))
      })
  }

  def getBaseName(name : String,family : String) : String =
    name.split("\\.").dropRight(1).mkString(".")+"_"+family


  def ionsDetection(
                     family : String,
                     config: Config,
                     confJson : ConfigReader,
                     source: MZXMLFile,
                     index: MZXMLIndex,
                     noiseIntensity : Double): Seq[IonsIdentification] = {

    /**
     * Get Peaks with criteria DeltaM0M2, number of carbon min/max,Max, number of sulfur min/max
     */
    val listSulfurMetabolites: Seq[PeakIdentification] =
      ScanLoader.
        getScanIdxAndSpectrumM0M2WithDelta(
          source,
          index,
          config.startRT,
          config.endRT,
          noiseIntensity,
          nbCarbonMin = confJson.numberCarbonMin(family),
          nbCarbonMax = confJson.numberCarbonMax(family),
          nbSulfurMin = confJson.numberSulfurMin(family),
          nbSulfurMax = confJson.numberSulfurMax(family),
          config.toleranceMz,
          deltaMOM2 = confJson.deltaMp0Mp2(family))

    /**
     * Merge All features (Ion/RT) that looks like !
     */
    val listSulfurMetabolitesSelected: Seq[PeakIdentification] =
      ScanLoader.keepSimilarMzWithMaxAbundance(listSulfurMetabolites, config.precisionMzh)

    /**
     * remove over represented peaks
     */
    val m: IonsIdentificationBuilder =
      ScanLoader.filterOverRepresentedPeak(
        source,
        index,
        config.startRT,
        config.endRT,
        listSulfurMetabolitesSelected,
        noiseIntensity,
        config.overrepresentedPeakFilter,
        confJson.neutralLoss(family).toSeq,
        confJson.daughterIons(family).toSeq,
        noiseIntensity
      )

    /**
     * find Neutral loses and Diagnostic Ion
     */
    m.findDiagnosticIonsAndNeutralLosses(config.precisionMzh,confJson.minMzCoreStructure(family))
  }
}
