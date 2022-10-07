package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.database.{BraChemDb, Chebi}
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import java.io.{BufferedWriter, File, FileWriter}

case object CsvIonsIdentificationFile {

  def build(list : Seq[IonsIdentification],
            familyMetabolite : String,
            configJson : ConfigReader, out: File) : Unit = {
    if ( list.nonEmpty ) {
      val size = list.last.mz.length
      val neutralLosses = configJson.neutralLoss(familyMetabolite).keys
      val daughterIons = configJson.daughterIons(familyMetabolite).keys

      val bw = new BufferedWriter(new FileWriter(out))

      /* Header */

      bw.write(s"ms+0;intensity+0;abundance+0;ms+2;intensity+2;abundance+2;")
      bw.write(s"CHEBI;")
      bw.write(s"BRACHEMDB;")
      bw.write(s"BRASSICA;")
      bw.write("RT;")
      bw.write("mz threshold;")
      bw.write("Nb (NL+DI);")

      neutralLosses.foreach {  name => bw.write(s"NL_$name;")}
      daughterIons.foreach {  name => bw.write(s"DI_$name;")}

      bw.write("\n")

      list.foreach(
        metabolitesIdentificationId => {
          0.until(size).foreach(
            i => bw.write(s"${metabolitesIdentificationId.mz(i)};"+
              s"${metabolitesIdentificationId.intensity(i)};"+
              s"${metabolitesIdentificationId.abundance(i)};")
          )

          bw.write(
            Chebi.getEntries(
              metabolitesIdentificationId.mz.head).map( entry => entry("ID")).mkString(",")
              +";")

          bw.write(
            BraChemDb.getEntries(
              metabolitesIdentificationId.mz.head).mkString(",") + ";")

          bw.write(configJson.getEntriesBaseRef(familyMetabolite,metabolitesIdentificationId.mz.head).mkString(",")+";")
          bw.write(s"${metabolitesIdentificationId.rt};")

          if ( metabolitesIdentificationId.mz.head < configJson.minMzCoreStructure(familyMetabolite) ) {
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
