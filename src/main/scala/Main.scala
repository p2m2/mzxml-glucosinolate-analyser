import fr.inrae.metabolomics.p2m2.`export`.CsvMetabolitesIdentificationFile
import fr.inrae.metabolomics.p2m2.builder.{MetaboliteIdentification, PeakIdentification, ScanLoader}

import java.io.File

object Main extends App {

  import scopt.OParser

  case class Config(
                     mzfiles : Seq[File] = Seq(),
                     thresholdIntensityFilter : Option[Int] = None,
                     thresholdAbundanceM0Filter : Double = 0.1,
                     overrepresentedPeakFilter : Int = 800,
                     startRT : Option[Double] = None,
                     endRT : Option[Double] = None,
                     precisionMzh : Int = 1000,
                     toleranceMz : Double = 0.01,
                     outfile : Option[File] = None,
                     verbose: Boolean = false,
                     debug: Boolean = false
                   )

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("mzxml-glucosinolates-phenolics-analyser"),
      head("mzxml-glucosinolates-phenolics-analyser", "1.0"),
      opt[Int]('i',"thresholdIntensityFilter")
        .optional()
        .action((x, c) => c.copy(thresholdIntensityFilter = Some(x)))
        .text(s"Keep ions above a x intensity (calculation on start-up time)"),
      opt[Int]('p',"overrepresentedPeakFilter")
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
      opt[Double]('t',"toleranceMz")
        .optional()
        .action((x, c) => c.copy(toleranceMz = x))
        .text(s"tolerance accepted. ${Config().toleranceMz}"),
      opt[File]('o',"outputFile")
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

  def process(config : Config): Unit = {

    val values = config.mzfiles.flatMap {
      mzFile =>
        val (source,index) = ScanLoader.read(mzFile)

        val intensityFilter = config.thresholdIntensityFilter match {
          case Some(v) => v
          case None => ScanLoader.calculBackgroundNoisePeak(source, index, config.startRT, config.endRT)
        }

        val listSulfurMetabolites: Seq[PeakIdentification] =
          ScanLoader.
            getScanIdxAndSpectrumM0M2WithDelta(
              source,
              index,
              config.startRT,
              config.endRT,
              config.thresholdAbundanceM0Filter,
              intensityFilter,
              filteringOnNbSulfur = 2,
              config.toleranceMz,
              deltaMOM2=1.996)

        val listSulfurMetabolitesSelected : Seq[PeakIdentification] =//listSulfurMetabolites
          ScanLoader.keepSimilarMzWithMaxAbundance(listSulfurMetabolites,config.precisionMzh)

        val m : MetaboliteIdentification = /*MetaboliteIdentification(source, index, config.startRT,
          config.endRT,listSulfurMetabolitesSelected)*/

          ScanLoader.filterOverRepresentedPeak(
            source,
            index,
            config.startRT,
            config.endRT,
            listSulfurMetabolitesSelected,
            intensityFilter,
            config.overrepresentedPeakFilter)

          m.getInfos(config.precisionMzh)
    }

    val f = config.outfile.getOrElse(new File("output.csv"))
    f.delete()
    CsvMetabolitesIdentificationFile.build(values,f)

    println(s"========= check ${f.getPath} ===============")
  }
}