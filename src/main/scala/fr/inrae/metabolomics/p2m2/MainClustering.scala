package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.MainDetection.{Config, args, parser1, process}
import fr.inrae.metabolomics.p2m2.`export`.IonsIdentificationFile
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

  def process(config : Config) = {
    val r = config.mzFiles.zipWithIndex.map {
      case (f, idx) => (idx, IonsIdentificationFile.load(f))
    }

    println("=============== DI =================")

    /* Index file, Seq(IonsIdentification) */
    val res = r.map {
      case (idxFile, v) => (idxFile, v._1)
    }.flatMap {
      case (_, v : Seq[IonsIdentification]) => {
        v
          .filter( _.daughterIons.size>10)
          .map(
          ii => {
            val mz = (ii.ion.peaks.head.mz * 100).round / 100.toDouble
            val rt = (ii.ion.rt * 10).round / 10.toDouble
            println(ii.neutralLosses.size)
         //   (mz, rt, ii.daughterIons.keys.mkString(","))
            (mz, rt)
          }
        )
      }
    }.distinct

/*
    res.foreach(
      row => println(row._1,row._2)
    )*/
    println("original size:"+r.map {
      case (idxFile, v) => v._1.size
    })
    println("size:"+res.length)
  }
  println("OK")

}
