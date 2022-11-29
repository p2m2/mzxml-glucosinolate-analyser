package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.database.BraChemDb
import org.eclipse.rdf4j.model.util.ModelBuilder
import org.eclipse.rdf4j.model.vocabulary.{RDF, RDFS}
import org.eclipse.rdf4j.rio.{RDFFormat, Rio}

import java.io.FileOutputStream
import scala.util.{Failure, Success, Try}

object MainRdfBrachemDb extends App {

  val repository: String = "https://github.com/p2m2/mzxml-glucosinolate-analyser"

  import scopt.OParser

  case class Config(outfile: String = "brachemdb.ttl")

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("brachemdb-rdf"),
      head("brachemdb", "1.0"),
      opt[String]('o', "outputFile")
        .optional()
        .action((x, c) => c.copy(outfile = x))
        .text(s"output path file."),
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
      "p2m2" -> "https://p2m2.github.io/resource/ontologies/2022/2/p2m2-ontology-3#"
    )
    /* BraChemDb */
    val b: ModelBuilder = new ModelBuilder()

    mapPrefix map { case (k, v) =>
      b.setNamespace(k, v)
    }

    0.until(1)//.until(BraChemDb.size)
      .map {
        id =>
          val m = BraChemDb.get(id)

          val uri = {
            m("DOI/PMID/SOURCE") match {
              case None => "p2m2:" + m("BraChemBD_ID").get
              case Some(v) => "([\\d\\.]+/[\\d\\.\\w]+)".r.findFirstMatchIn(v) match {
                case Some(u) => println("TOTOTO=>", u.group(1)); "p2m2:" + m("BraChemBD_ID").get
                case _ => println("TOTO"); "p2m2:" + m("BraChemBD_ID").get
              }
            }
          }

          println(uri)

          //https://doi.org/10.1016/j.bse.2018.03.003


          val subject = b.subject("p2m2:" + m("BraChemBD_ID").get)
          subject.add(RDF.TYPE, "p2m2:AnnotationDatabase")
          subject.add(RDFS.LABEL, m("TRIVAL_NAME").getOrElse(m("BraChemBD_ID").get))

          addCond(subject, "p2m2:family", m("FAMILY/PATHWAYS"))
          addCond(subject, "p2m2:trivialName", m("TRIVAL_NAME"))
          addCond(subject, "p2m2:SemiSystematicName", m("SEMI-SYSTEMATIC_NAME"))
          addCond(subject, "p2m2:formula", m("MOLECULAR_FORMULA"))
          addCond(subject, "p2m2:isotopicMass", m("ISOTOPIC_MASS"))
          addCond(subject, "p2m2:m_minsh", m("[M-H]"))
          addCond(subject, "p2m2:m_mplush", m("[M+H]"))
          addCond(subject, "p2m2:m_minh", m("[M-Na]"))
          addCond(subject, "p2m2:m_plusna", m("[M+Na]"))
          addCond(subject, "p2m2:m_mink", m("[M-K]"))
          addCond(subject, "p2m2:m_plusk", m("[M+K]"))
          addCond(subject, "p2m2:source", m("DOI/PMID/SOURCE"))
          addCond(subject, "p2m2:organism", m("ORGANISM"))
          addCond(subject, "p2m2:smile", m("SMILES"))
          addCond(subject, "p2m2:inchi", m("InChI"))
          addCond(subject, "p2m2:inchikey", m("InChIKey"))
          addCond(subject, "p2m2:has_kegg_compound_identifier", m("KEGG_compound_identifier"))
          addCond(subject, "p2m2:has_chebi_id", m("ChEBI_ID"))
          addCond(subject, "p2m2:has_charge", m("Charge"))
      }
    //http://purl.obolibrary.org/obo/chebi/formula

    getStringFromModelBuilder(b, "brachemdb.ttl")

    def addCond[T <: String](builder: ModelBuilder, p: T, s: Option[String]): ModelBuilder = {
      s.nonEmpty match {
        case true => builder.add(p, s.get)
        case _ =>
      }
      builder
    }

    def getStringFromModelBuilder(builder: ModelBuilder, fileName: String): Unit = {

      val extension: String = fileName.split("\\.").last
      val out: FileOutputStream = new FileOutputStream(fileName)

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
        case Failure(exception) => out.close(); System.err.println(exception)
      }
    }
  }
}
