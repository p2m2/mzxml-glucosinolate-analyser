package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.database.{Chebi, ChemicalUtils}
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import java.io.{BufferedWriter, File, FileWriter}
import java.util.Calendar

case object CandidateResume {
  def build(list: Seq[(Double, Seq[IonsIdentification])],
            familyMetabolite: String,
            configJson: ConfigReader, out: File): Unit = {
    val bw = new BufferedWriter(new FileWriter(out))

    bw.write(s" ====== Resume $familyMetabolite : ${Calendar.getInstance().getTime} =======\n")
    bw.write(s"Numb. Candidate : ${list.size}\n\n")

    val values = list.sortBy(_._2.size).reverse

    for ( ( mz, lIons) <- values ) {
      bw.write(s"========================== $mz m/z ======================\n")
      bw.write(s"hit:${lIons.size}\n")

      val m = configJson.di(familyMetabolite).map {
        case (key, _ ) =>
          key -> lIons.flatMap(i => i.daughterIons.get(key)).flatten

      } filter {
        case (_,l) => l.nonEmpty
      } map {
        case (k,l) => k+s"(${l.size})"
      }

      val rts = lIons.map(
        ion => ion.ion.rt
      ).mkString(", ")

      bw.write(s"= Retention time =\n$rts\n")


    //  val m = lIons.map( i => i.daughterIons.keys )
      bw.write(s"Numb. Daughter ions:${m.size}\n")
      bw.write(s"=Daughter ions=\n${m.mkString(",")}\n")

      val m2 = configJson.nl(familyMetabolite).map {
        case (key, _) =>
          key -> lIons.flatMap(i => i.neutralLosses.get(key)).flatten

      } filter {
        case (_, l) => l.nonEmpty
      } map {
        case (k, l) => k + s"(${l.size})"
      }

      //  val m = lIons.map( i => i.daughterIons.keys )
      bw.write(s"Numb. Neutral losses:${m2.size}\n")
      bw.write(s"=Neutral losses=\n${m2.mkString(",")}\n")

      val metabolitesDBChebi = lIons.flatMap( metabolitesIdentificationId => Chebi.getEntries(
        metabolitesIdentificationId.ion.peaks.head.mz) map {
        m =>
          m("ID") + "[R=" + ChemicalUtils.correlation(m("FORMULA"), metabolitesIdentificationId.ion.peaks.map(p => p.abundance)) + "]"
      }).mkString(", ")


      bw.write(s"= candidate chebi =\n$metabolitesDBChebi\n")

      val namesAndR =  lIons.flatMap ( metabolitesIdentificationId =>
        configJson.getEntriesBaseRef(familyMetabolite,metabolitesIdentificationId.ion.peaks.head.mz) map {
        m =>
          (m.name match {
            case Some(n) => n
            case _ => m.id
          }) + "[R=" + ChemicalUtils.correlation(m.formula, metabolitesIdentificationId.ion.peaks.map(p => p.abundance)) + "]"
      }).distinct.mkString(", ")
      bw.write(s"= candidate $familyMetabolite =\n$namesAndR\n")
    }

    bw.write(s"\n\n===================== TOTAL HIT =================================\n\n")

    /** Most representative DI  */
    val m = configJson.di(familyMetabolite).map {
      case (key, _) =>
        key -> values.flatMap( v => v._2.flatMap(i => i.daughterIons.get(key)).flatten )
    }.toSeq.sortBy(_._2.size).reverse.filter {
      case (_, l) => l.nonEmpty
    } map {
      case (k, l) => k + s"(${l.size})"
    }

    bw.write(s"Numb. Daughter ions:${m.size}\n")
    bw.write(s"=Daughter ions =\n${m.mkString(",")}\n")
    bw.close()
  }
}
