package fr.inrae.metabolomics.p2m2.database

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

/**
 * Management of ChEBI Glucosinolate information
 *
 * Generation of src/main/resources/glucosinolate_ChEBI.tsv
 *
 * 1) https://www.ebi.ac.uk/chebi/advancedSearchFT.do?searchString=glucosinolate
 * 2) "Download your results"
 * ChEBI_Results.tsv
 */

case object Chebi {
  private val r = getClass.getResource("/glucosinolate_ChEBI.tsv")

  private val entries : Seq[Map[String,String]] = Try(r.getPath) match {
    case Success(_) =>
      build(Source.fromInputStream(r.openStream()))
    case Failure(_) =>
      System.err.println("Unable to find the export of glucosinolate entries from the ChEBI database .")
      Seq()
  }

  def build(b : BufferedSource): Seq[Map[String,String]] = {
    val header = b.getLines().take(1).mkString.split("\t")

    b.getLines().map(
      line => line.split("\t").zipWithIndex.map {
        case (record,idx) => header(idx) -> record
      }.toMap
    )
      .toSeq
      .filter {
        entry => entry.contains("MONOISOTOPIC MASS") && entry("MONOISOTOPIC MASS").trim.nonEmpty
      }
  }

  def getEntries(monoIsotopicMassSearch : Double, tolerance : Double = 0.01): Seq[Map[String,String]] = {
    entries.filter {
      entry =>
        val m = entry("MONOISOTOPIC MASS").toDouble
        (monoIsotopicMassSearch > (m-tolerance)) && (monoIsotopicMassSearch < (m+tolerance))
    }
  }

}
