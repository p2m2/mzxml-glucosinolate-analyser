package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.`export`.IonsIdentificationFile
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import org.eclipse.rdf4j.model.util.{ModelBuilder, Values}
import org.eclipse.rdf4j.model.vocabulary.{RDF, RDFS, XSD}
import org.eclipse.rdf4j.rio.{RDFFormat, Rio, WriterConfig}

import java.io.{File, StringWriter}

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

    val mapPrefix = Map(
      "rdfs" -> "http://www.w3.org/2000/01/rdf-schema#",
      "rdf" -> "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
      "xsd" -> "http://www.w3.org/2000/10/XMLSchema#",
      "owl" -> "http://www.w3.org/2002/07/owl#",
      "prov" -> "http://www.w3.org/ns/prov#",
      "p2m2" -> "https://p2m2.github.io/resource/ontologies/2022/2/p2m2-ontology-3#",
      "sio" -> " http://semanticscience.org/resource/"
    )

    val builder : ModelBuilder = new ModelBuilder()

    mapPrefix map { case (k, v) =>
      builder.setNamespace(k, v)
    }

    /* Index file, Seq(IonsIdentification) */
    r.map {
      case (idxFile, v) => (idxFile, v._1)
    }.foreach {
      case (_, v: Seq[IonsIdentification]) =>
        v
          .filter(x => x.daughterIons.nonEmpty || x.neutralLosses.nonEmpty )
          .foreach(
            ii => {
              println(ii.pathFile)
              val ion = Values.bnode
              builder
                .subject((s"file:${ii.pathFile}"))
                .add(RDF.TYPE, "sio:SIO_000396")
                .add(RDF.TYPE,"p2m2:MassSpectrometerOutputFile")
                .add("p2m2:has_eligible_ions",ion)

              builder
                .subject(ion)
                .add(RDF.TYPE, "sio:SIO_000396")
                .add("p2m2:mz",Values.literal(ii.ion.peaks.head.mz))
                .add("p2m2:abundance",Values.literal(ii.ion.peaks.head.abundance))
                .add("p2m2:rt",Values.literal(ii.ion.rt))
                .add("p2m2:score",Values.literal(ii.scoreIdentification))

              ii.daughterIons.foreach {
                case (_, Some((name,mz,abundance))) =>
                  val d = Values.bnode()
                  builder
                    .subject(d)
                    .add(RDF.TYPE, "sio:SIO_000396")
                    .add(RDFS.LABEL,s"DI_$name")
                    .add("p2m2:mz",Values.literal(mz))
                    .add("p2m2:abundance",Values.literal(abundance))
                    .add("p2m2:target",s"p2m2:DL_$name")
                case _ =>
              }

              ii.neutralLosses.foreach {
                case (_, Some((name, mz, abundance))) =>
                  val d = Values.bnode()
                  builder
                    .subject(d)
                    .add(RDF.TYPE, "sio:SIO_000396")
                    .add(RDFS.LABEL, s"NL_$name")
                    .add("p2m2:mz", Values.literal(mz))
                    .add("p2m2:abundance", Values.literal(abundance))
                    .add("p2m2:target",s"p2m2:NL_$name")
                case _ =>
              }

             //ii.daughterIons
            }
          )
    }
    println(getStringFromModelBuilder(builder))

    /*
      p2m2 : https://p2m2.github.io/resource/ontologies/2022/2/p2m2-ontology-3#
      <file mzxml> is a http://semanticscience.org/resource/SIO_000396 / p2m2:MassSpectrometerOutputFile

      <file mzxml> p2m2:has_elligible_ions <anion>
      <anion> is a <http://purl.obolibrary.org/obo/CHEBI_22563>

      <anion> prov:wasGeneratedBy <https://github.com/p2m2/mzxml-glucosinolate-analyser>

      ......     <http://purl.obolibrary.org/obo/CHEBI_24279> (Glucosinolate)


      <anion> p2m2:has_daughter_ion <v>
      <v> rdfs:label "some"
      <anion> p2m2:has_neutral_loss <v2>
      <v2> rdfs:label "some"
      <v2> p2m2:mz "..."
      <anion> oboInOwl:hasDbXRef <some>

     */
  }

  def getStringFromModelBuilder(builder: ModelBuilder, extension: String = "ttl"): String = {
    val config: WriterConfig = new WriterConfig()
    // config.set(BasicWriterSettings.PRETTY_PRINT, true)

    val stringWriter = new StringWriter()

    val format: RDFFormat = extension match {
      case "jsonld" => RDFFormat.JSONLD
      case "ttl" => RDFFormat.TURTLE
      case "trig" => RDFFormat.TRIG
      case "nt" => RDFFormat.NTRIPLES
      case "n3" => RDFFormat.N3
      case "rdf" => RDFFormat.RDFXML
      case _ => throw new IllegalArgumentException(s"Unknown extension : $extension ")
    }

    Rio.write(builder.build(), stringWriter, format, config)
    stringWriter.toString
  }

}
