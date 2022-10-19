package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.database.{Chebi, ChemicalUtils}
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import java.io.{BufferedWriter, File, FileWriter}

case object CsvIonsIdentificationFile {

  def build(list : Seq[IonsIdentification],
            familyMetabolite : String,
            configJson : ConfigReader, out: File) : Unit = {
    if ( list.nonEmpty ) {

      val neutralLosses = configJson.neutralLoss(familyMetabolite).keys
      val daughterIons = configJson.daughterIons(familyMetabolite).keys

      val bw = new BufferedWriter(new FileWriter(out))

      /* Header */
      val size =  4
      0.until(size).foreach(
        i=> bw.write(s"ms+$i;intensity+$i;abundance+$i;")
      )

     // bw.write(s"ms+0;intensity+0;abundance+0;ms+1;intensity+1;abundance+1;ms+1;intensity+2;abundance+2;ms+3;intensity+3;abundance+3;")
      bw.write(s"CHEBI;")
      //bw.write(s"BRACHEMDB;")
      bw.write(s"BRASSICA;")
      bw.write("RT;")
      bw.write("mz threshold;")
      bw.write("Nb (NL+DI);")

      neutralLosses.foreach {  name => bw.write(s"NL_$name;")}
      daughterIons.foreach {  name => bw.write(s"DI_$name;")}

      bw.write("\n")

      list
        .filter( _.ion.peaks.nonEmpty)
        .foreach(
        metabolitesIdentificationId => {
          0.until(size).foreach(
            i => bw.write(s"${metabolitesIdentificationId.ion.peaks(i).mz};"+
              s"${metabolitesIdentificationId.ion.peaks(i).intensity};"+
              s"${metabolitesIdentificationId.ion.peaks(i).abundance};")
          )

          val metabolitesDBChebi = Chebi.getEntries(
            metabolitesIdentificationId.ion.peaks.head.mz) map {
            m =>
              m("ID") + "[R=" + ChemicalUtils.correlation(m("FORMULA"), metabolitesIdentificationId.ion.peaks.map(p => p.abundance)) + "]"
          }

          bw.write(metabolitesDBChebi.mkString(",") +";")
/*
          bw.write(
            BraChemDb.getEntries(
              metabolitesIdentificationId.ion.peaks.head.mz).mkString(",") + ";")
*/
          val metabolitesDB = configJson.getEntriesBaseRef(familyMetabolite,metabolitesIdentificationId.ion.peaks.head.mz)

          /* metabolites name from database and R computed with isotopic distribution (formula) and experimental abundance */
          val namesAndR : Seq[String] = metabolitesDB map {
            m =>
              (m.name match {
                case Some(n) => n
                case _ => m.id
              }) + "[R="+ ChemicalUtils.correlation(m.formula,metabolitesIdentificationId.ion.peaks.map( p=> p.abundance)) + "]"
          }

          bw.write(namesAndR.mkString(",")+";")
          bw.write(s"${metabolitesIdentificationId.ion.rt};")

          if ( metabolitesIdentificationId.ion.peaks.head.mz < configJson.minMzCoreStructure(familyMetabolite) ) {
            bw.write("*;")
          } else
            bw.write(";")

          bw.write(s"${metabolitesIdentificationId.scoreIdentification};")

          neutralLosses
            .map( name => metabolitesIdentificationId.neutralLosses.getOrElse(name,None).getOrElse(""))
            .foreach {  value => bw.write(s"$value;")}
          daughterIons
            .map( name => metabolitesIdentificationId.daughterIons.getOrElse(name,None).getOrElse(""))
            .foreach {  value => bw.write(s"$value;")}
          bw.write("\n")
        }
      )
      bw.close()
    }
  }
}
