package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.database.Chebi
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Calendar
import scala.collection.parallel.ParSeq

case object CandidateResume {
  def build(list: Seq[(Double, ParSeq[(IonsIdentification,String)])],
            familyMetabolite: String,
            configJson: ConfigReader, out: File): Unit = {

    println(" => RESUME:"+out.getAbsolutePath)

    val bw = new BufferedWriter(new FileWriter(out))
    val values = list.sortBy(_._2.size).reverse

    bw.write(s" ====== Resume $familyMetabolite : ${Calendar.getInstance().getTime} =======\n")

    /** Most representative DI */
    val m = configJson.di(familyMetabolite).map {
      case (key, _) =>
        key -> values.flatMap(v => v._2.flatMap(i => i._1.daughterIons.get(key)).flatten)
    }.toSeq.sortBy(_._2.size).reverse.filter {
      case (_, l) => l.nonEmpty
    } map {
      case (k, l) => k + s"(${l.size})"
    }
    bw.write("NB FILES         %20s%s\n".format(" ",list.flatMap(_._2.map(_._2)).distinct.size))
    bw.write("FILES            %20s%s\n".format(" ",list.flatMap(_._2.map(_._2)).distinct.mkString(", ")))
    bw.write("NB CANDIDATE     %20s%s\n".format(" ",list.size))
    bw.write("NB DAUGHTER IONS %20s%s\n".format(" ",m.size))
    bw.write("DAUGHTER IONS    %20s%s\n".format(" ",m.mkString(", ")))
    bw.write("M/Z              %20s%s\n".format(" ",values.map(_._1).sorted.mkString(", ")))
    bw.write("\n\n")


    for ( ( mz, (lIonsAndFileName) ) <- values ) {

      val m = configJson.di(familyMetabolite).map {
        case (key, _ ) =>
          key -> lIonsAndFileName.flatMap(i => i._1.daughterIons.get(key)).flatten

      } filter {
        case (_,l) => l.nonEmpty
      } map {
        case (k,l) => k+s"(${l.size})"
      }

      val m2 = configJson.nl(familyMetabolite).map {
        case (key, _) =>
          key -> lIonsAndFileName.flatMap(i => i._1.neutralLosses.get(key)).flatten

      } filter {
        case (_, l) => l.nonEmpty
      } map {
        case (k, l) => k + s"(${l.size})"
      }

      val rts = lIonsAndFileName.map(
        ion => (ion._1.ion.rt*1000).round/1000.toDouble
      ).mkString(", ")

      val metabolitesDBChebi = lIonsAndFileName.flatMap(metabolitesIdentificationId => Chebi.getEntries(
        metabolitesIdentificationId._1.ion.peaks.head.mz) map {
        m =>
          m("ID")
      }).distinct.mkString(", ")

      val namesAndR = lIonsAndFileName.flatMap(metabolitesIdentificationId =>
        configJson.getEntriesBaseRef(familyMetabolite, metabolitesIdentificationId._1.ion.peaks.head.mz) map {
          m =>
            (m.name match {
              case Some(n) => n
              case _ => m.id
            })
        }).distinct.mkString(", ")

      bw.write(s"========================== $mz m/z ======================\n")
      bw.write("HIT             %20s%s\n".format(" ",lIonsAndFileName.size))
      bw.write("FILES           %20s%s\n".format(" ",lIonsAndFileName.map(_._2).distinct.mkString(", ")))
      bw.write("RT              %20s%s\n".format(" ",rts))
      bw.write("NB DI           %20s%s\n".format(" ",m.size))
      bw.write("NB NL           %20s%s\n".format(" ",m2.size))
      bw.write("DAUGHTER IONS   %20s%s\n".format(" ",m.mkString(",")))
      bw.write("NEUTRAL LOSSES  %20s%s\n".format(" ",m2.mkString(",")))
      bw.write("CANDIDATE CHEBI %20s%s\n".format(" ",metabolitesDBChebi))
      bw.write("CANDIDATE %s15  %15s%s\n".format(familyMetabolite.toUpperCase," ",namesAndR))
    }

    bw.close()
  }
}
