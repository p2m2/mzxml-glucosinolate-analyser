package fr.inrae.metabolomics.p2m2.database

import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

case object BraChemDb {
  private val r = getClass.getResource("/BraChemDB.csv")

  private val entries: Map[String, String] = Try(r.getPath) match {
    case Success(_) =>
      build(Source.fromInputStream(r.openStream()))
    case Failure(_) =>
      System.err.println("Unable to find the export of BraChemDB entries.")
      Map()
  }

  def build(b: BufferedSource): Map[String, String] = {

    b.getLines().flatMap(
      line => line.split("\t") match {
       case l if l.length==2 => Some(l(0),l(1))
       case _ => None
      }
    ).toMap
  }

  def getEntries(monoIsotopicMassSearch: Double, tolerance: Double = 0.002): Seq[String] = {
    entries.filter {
      entry =>
        val m: Double = entry._2.toDouble - ChemicalUtils.massProton
        (monoIsotopicMassSearch > (m - tolerance)) && (monoIsotopicMassSearch < (m + tolerance))
    }.keys.toSeq
  }
}
