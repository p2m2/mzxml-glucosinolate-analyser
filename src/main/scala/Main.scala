import fr.inrae.metabolomics.p2m2.builder.{MetaboliteIdentification, PeakIdentification, ScanLoader}

import java.io.{BufferedWriter, File, FileWriter}

object Main extends App {

  import scopt.OParser

  case class Config(
                   mzfiles : Seq[File] = Seq(),
                     verbose: Boolean = false,
                     debug: Boolean = false
                   )

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("msd-metdisease-database-pmid-cid-builder"),
      head("msd-metdisease-database-pmid-cid-builder", "1.0"),
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
        val listSulfurMetabolites: Seq[PeakIdentification] = ScanLoader.getScanIdxAndSpectrum3IsotopesSulfurContaining(source, index)
        MetaboliteIdentification(source,index,listSulfurMetabolites).getInfos()
    }

    values.slice(0,3) foreach {
      case l => println("=========1,3");println(l.mkString(";"))
    }

    new File("test.csv").delete()

    val bw = new BufferedWriter(new FileWriter(new File("test.csv")))
    bw.write("ms0;intensity0;abundance0;ms1;intensity1;abundance1;ms2;intensity2;abundance2;gluconolactone_178;sulfureTrioxide_80;anhydroglucose_162;thioglucose_s03_242;thioglucose_196;last_223\n")

    values foreach {
      (list : Seq[Any]) => bw.write(list.map {
        case Some(o)  => o
        case None => ""
        case v => v.toString
      } . mkString(";") + "\n")
    }

    bw.close()
    println("========= check test.csv ===============")
  }
}