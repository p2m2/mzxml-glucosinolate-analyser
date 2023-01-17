package fr.inrae.metabolomics.p2m2.database

import fr.inrae.metabolomics.p2m2.database.BraChemDb.header
import org.apache.poi.hssf.usermodel.HSSFWorkbook
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import java.io.{File, FileInputStream, InputStream}
import scala.io.{BufferedSource, Source}
import scala.util.{Failure, Success, Try}

case object BraChemDb {
  private val r = getClass.getResourceAsStream("/20210205_BraChemDB_vcorrect.xlsx")

  private val sep=";"

  private val tmp = Try(r) match {
    case Success(_) =>
      build(r)
    case Failure(_) =>
      System.err.println("Unable to find the export of BraChemDB entries.")
      (Seq(),Seq())
  }

  private val header : Seq[String] = tmp._1
  private val entries : Seq[Map[Int, String]] = tmp._2


  def build(b: InputStream): (Seq[String],Seq[Map[Int, String]]) = {
    val workbook: XSSFWorkbook = new XSSFWorkbook(b)
    /* Get first Page */
    val sheet = workbook.getSheetAt(0)
    /* header management */
    val rowIndex = sheet.getFirstRowNum
    val header =
      sheet.getRow(rowIndex).getFirstCellNum.to(sheet.getRow(rowIndex).getLastCellNum)
        .filter(sheet.getRow(rowIndex).getCell(_) != null)
        .map(sheet.getRow(rowIndex).getCell(_).toString)

    val values : Seq[Map[Int, String]] = (sheet.getFirstRowNum+1).to(sheet.getLastRowNum)
      .map(
        rowIndex => sheet.getRow(rowIndex).getFirstCellNum.to(sheet.getRow(rowIndex).getLastCellNum)
          .map { sheet.getRow(rowIndex).getCell(_) }
          .zipWithIndex
          .flatMap {
            case (value,idx) if value != null => Some(idx->value.toString)
            case _ => None
          }.toMap
      )

    (header,values)
  }

  def size : Int = entries.length
  def get(idx : Int) : Map[String,Option[String]] = {
    val r : Map[String,Option[String]] = entries(idx).map {
      case (i, v) if v.trim != "NA" && v.trim != "" => header(i) -> Some(v)
      case (i, _) => header(i) -> None
    }.toMap

    header.map {
      case headerVal if r.contains(headerVal) => headerVal -> r(headerVal)
      case headerVal =>   headerVal -> None
    }.toMap
  }

  def getEntries(monoIsotopicMassSearch: Double, tolerance: Double = 0.002): Seq[String] = {
    entries.filter {
      entry =>
        val idx = header.indexOf("ISOTOPIC_MASS")
        val m: Double = entry(idx).toDouble - ChemicalUtils.massProton
        (monoIsotopicMassSearch > (m - tolerance)) && (monoIsotopicMassSearch < (m + tolerance))
    }.map {
      entry =>
        val idx = header.indexOf("BraChemBD_ID")
        entry(idx)
    }
  }

}
