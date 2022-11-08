package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.`export`.IonsIdentificationFile
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import java.io.File

object MainRdfGenerator extends App {

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

  def process(config: Config) = {
    val r = config.mzFiles.zipWithIndex.map {
      case (f, idx) => (idx, IonsIdentificationFile.load(f))
    }

    val N_FILTER = 1
    println(s"=============== DI >$N_FILTER =================")

    /* Index file, Seq(IonsIdentification) */
    val res = r.map {
      case (idxFile, v) => (idxFile, v._1)
    }.flatMap {
      case (idxF, v: Seq[IonsIdentification]) => {
        v
          .filter(_.daughterIons.nonEmpty)
          .filter(x => x.daughterIons.values.flatten.size > N_FILTER)
          .map(
            ii => {

             ii.daughterIons
            }
          )
      }
    }.distinct

    /*
      p2m2 : https://p2m2.github.io/resource/ontologies/2022/2/p2m2-ontology-3#
      <file mzxml> is a http://semanticscience.org/resource/SIO_000396 / p2m2:MassSpectrometerOutputFile

      <file mzxml> p2m2:has_elligible_ions <anion>
      <anion> is a <http://purl.obolibrary.org/obo/CHEBI_22563>
      <anion> p2m2:has_daughter_ion <v>
      <v> rdfs:label "some"
      <anion> p2m2:has_neutral_loss <v2>
      <v2> rdfs:label "some"
      <v2> p2m2:mz "..."
      <anion> oboInOwl:hasDbXRef <some>

     */

    res.foreach(
      row =>
        println(row)

    )

    println("original size:" + r.map {
      case (idxFile, v) => v._1.size
    })
    println("size:" + res.length)
  }
}
