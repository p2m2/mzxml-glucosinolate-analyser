package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.`export`.IonsIdentificationFile
import fr.inrae.metabolomics.p2m2.database.{BraChemDb, Chebi}
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import org.eclipse.rdf4j.model.IRI
import org.eclipse.rdf4j.model.util.{ModelBuilder, Values}
import org.eclipse.rdf4j.model.vocabulary.{RDF, RDFS, XSD}
import org.eclipse.rdf4j.rio.{RDFFormat, Rio, WriterConfig}

import java.io.{BufferedWriter, File, FileOutputStream, FileWriter, StringWriter}
import scala.util.{Failure, Success, Try}

object MainRdfGenerator extends App {

  val repository : String = "https://github.com/p2m2/mzxml-glucosinolate-analyser"

  import scopt.OParser

  case class Config(
                     mzFiles: Seq[File] = Seq(),
                     minScoreThreshold : Int = 4,
                     outfile : String = "export.ttl"

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
      opt[String]('o', "outputFile")
        .optional()
        .action((x, c) => c.copy(outfile = x))
        .text(s"output path file."),
      opt[Int]('m', "minScoreThreshold")
        .optional()
        .action((x, c) => c.copy(minScoreThreshold = x))
        .text(s"minimal score threshold to build RDF file default=${Config().minScoreThreshold}"),
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

    val mapPrefix = Map(
      "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
      "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "xsd" -> "http://www.w3.org/2000/10/XMLSchema#",
      "owl" -> "http://www.w3.org/2002/07/owl#",
      "prov" -> "http://www.w3.org/ns/prov#",
      "p2m2" -> "https://p2m2.github.io/resource/ontologies/2022/2/p2m2-ontology-3#",
      "sio" -> "http://semanticscience.org/resource/",
      "oboInOwl" -> "http://www.geneontology.org/formats/oboInOwl#"
    )

    val builder : ModelBuilder = new ModelBuilder()

    mapPrefix map { case (k, v) =>
      builder.setNamespace(k, v)
    }
    builder
      .subject(repository)
      .add(RDF.TYPE,"p2m2:SoftwareRepository")
      .add(RDFS.LABEL,"p2m2/mzxml-glucosinolate-analyser")

  //  val analyzer =

    /* Index file, Seq(IonsIdentification) */
    config.mzFiles.zipWithIndex.map {
      case (f, idx) => (idx, IonsIdentificationFile.load(f))
    }.map {
      case (idxFile, v) => (idxFile, v._1)
    }.foreach {
      case (_, v: Seq[IonsIdentification]) =>
        v
          .filter(x => x.scoreIdentification > config.minScoreThreshold )
          .foreach(
            ii => {
              val analyze = Values.bnode

              builder
                .subject((s"file:/${ii.pathFile}"))
                .add("p2m2:has_in_silico_analyze",analyze)

              val ion = Values.bnode

              builder
                .subject(analyze)
                .add(RDF.TYPE, "p2m2:ResultsTools")
                .add(RDFS.LABEL,s"Glucosinolate Ion detection")
                .add("p2m2:has_repository",Values.iri(repository))
                .add("p2m2:has_eligible_ion",ion)
                .add("p2m2:configMinScoreThreshold",config.minScoreThreshold)


              val mzApprox= (ii.ion.peaks.head.mz*1000).round/1000.toDouble
              val rtsApprox =  (ii.ion.rt*1000).round/1000.toDouble


              builder
                .subject(ion)
                .add(RDF.TYPE, "p2m2:Anion")
                .add(RDFS.LABEL,s"Anion ($mzApprox)")
                .add(RDF.TYPE, "sio:SIO_000396")
                .add("p2m2:mz",Values.literal(ii.ion.peaks.head.mz))
                .add("p2m2:abundance",Values.literal(ii.ion.peaks.head.abundance))
                .add("p2m2:rt",Values.literal(ii.ion.rt))
                .add("p2m2:score",Values.literal(ii.scoreIdentification))

              Chebi.getEntries(mzApprox) foreach {
                m =>
                  val chebiId = Values.iri("https://identifiers.org/" + m("ID"))
                  builder
                  .subject(ion)
                  .add("p2m2:mzSimilarity",chebiId)

                  builder
                    .subject(chebiId)
                    .add(RDF.TYPE, "p2m2:AnnotationDatabase")
                    .add(RDFS.LABEL,m("NAME"))
              }

              BraChemDb.getEntries(mzApprox) foreach {
                name =>
                  val id = "p2m2:"+name
                  builder
                    .subject(ion)
                    .add("p2m2:mzSimilarity", id)
              }

              ii.daughterIons.foreach {
                case (_, Some((name,mz,abundance))) =>
                  val d = Values.bnode()
                  builder
                    .subject(d)
                    .add(RDF.TYPE, "p2m2:DaughterIon")
                    .add(RDFS.LABEL,s"DI_$name")
                    .add("p2m2:mz",Values.literal(mz))
                    .add("p2m2:abundance",Values.literal(abundance))

                  builder
                    .subject(ion)
                    .add("p2m2:has_daughterIon",d)

                case _ =>
              }

              ii.neutralLosses.foreach {
                case (_, Some((name, mz, abundance))) =>
                  val d = Values.bnode()
                  builder
                    .subject(d)
                    .add(RDF.TYPE, "p2m2:NeutralLoss")
                    .add(RDFS.LABEL, s"NL_$name")
                    .add("p2m2:mz", Values.literal(mz))
                    .add("p2m2:abundance", Values.literal(abundance))
                  builder
                    .subject(ion)
                    .add("p2m2:has_neutralLoss", d)
                case _ =>
              }

             //ii.daughterIons
            }
          )
    }
    /* delete file if exist */
    new File(config.outfile).delete()
    getStringFromModelBuilder(builder,config.outfile)
  }

  def getStringFromModelBuilder(builder: ModelBuilder, fileName: String): Unit = {

    val extension: String = fileName.split("\\.").last
    val out : FileOutputStream = new FileOutputStream(fileName)

    val format: RDFFormat = extension match {
      case "jsonld" => RDFFormat.JSONLD
      case "ttl" => RDFFormat.TURTLE
      case "trig" => RDFFormat.TRIG
      case "nt" => RDFFormat.NTRIPLES
      case "n3" => RDFFormat.N3
      case "rdf" => RDFFormat.RDFXML
      case _ => throw new IllegalArgumentException(s"Unknown extension : $extension ")
    }

    Try(Rio.write(builder.build(), out, format)) match {
      case Success(_) => out.close()
      case Failure(exception) => out.close();System.err.println(exception)
    }

  }

}
