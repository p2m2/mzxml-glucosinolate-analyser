package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.`export`.IonsIdentificationFile
import fr.inrae.metabolomics.p2m2.builder.ScanLoader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import java.io.File

object MainClustering extends App {

  import scopt.OParser

  case class Config(
                     mzFiles: Seq[File] = Seq()
                   )

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("clustering-analyser"),
      head("clustering-analyser", "1.0"),
      arg[File]("<file>...")
        .unbounded()
        .action((x, c) => c.copy(mzFiles = c.mzFiles :+ x)),
      help("help").text("prints this usage text"),
      note("some notes." + sys.props("line.separator")),
      checkConfig(_ => success)
    )
  }

  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      process(config)
    // do something
    case _ => System.err.println("Ko")
    // arguments are bad, error message will have been displayed
  }

  def precision(mz:Double,precision: Int) : Double = (mz * precision).round / precision.toDouble

  def process(config : Config) = {
    val r = config.mzFiles.zipWithIndex.map {
      case (f, idx) => (idx, IonsIdentificationFile.load(f))
    }

    val N_FILTER=4
    println(s"=============== DI >$N_FILTER =================")

    /* Index file, Seq(IonsIdentification) */
    val res = r.map {
      case (idxFile, v) => (idxFile, v._1)
    }.flatMap {
      case (idxF, v : Seq[IonsIdentification]) => {
        v
          .filter( _.daughterIons.nonEmpty )
          .filter( x=> x.daughterIons.values.flatten.size > N_FILTER)
          .map(
          ii => {

            val mz = precision(ii.ion.peaks.head.mz,100)
            val rt = precision(ii.ion.rt,10)
           // (ii.daughterIons.foreach(print(_)))
         //   (mz, rt, ii.daughterIons.keys.mkString(","))
            (idxF,mz, rt)
          }
        )
      }
    }.distinct

    res.foreach(
      row =>
        println(f"${row._2}%4.2f \t ${row._3}%3.2f\t${row._1}")

    )
    val ion : IonsIdentification = r.head._2._1.head

    val v = ScanLoader.read(new File(ion.pathFile))
    val r2 = ScanLoader.getDeltaNeutralLossesFromPeak(v._1,v._2,ion.ion,100)
    println("=======R2=================")
    println(r2)
    println("========================")

    //ScanLoader.getDeltaNeutralLossesFromPeak()

    println("original size:"+r.map {
      case (idxFile, v) => v._1.size
    })
    println("size:"+res.length)
  }
  println("OK")

}
