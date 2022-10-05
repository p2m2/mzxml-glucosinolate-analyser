package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.database.Chebi
import fr.inrae.metabolomics.p2m2.output.CsvMetabolitesIdentification

import java.io.{BufferedWriter, File, FileWriter}

case object CsvMetabolitesIdentificationFile {

  val mzCoreStructureGlucosinolate : Double = 317.995896

  def build(list : Seq[CsvMetabolitesIdentification],
            familyMetabolite : String,
            configJson : ConfigReader, out: File) : Unit = {
    if ( list.nonEmpty ) {
      val size = list.last.mz.length
      val neutralLosses = list.last.neutralLosses.keys
      val daughterIons = list.last.daughterIons.keys

      val bw = new BufferedWriter(new FileWriter(out))

      /* Header */

      bw.write(s"ms+0;intensity+0;abundance+0;ms+2;intensity+2;abundance+2;")
      bw.write(s"CHEBI ID;")
      bw.write(s"BRASSICA ID;")
      bw.write("RT;")
      bw.write("Hyp. Identification Sulfur Polypeptide;")
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

          bw.write(Chebi.getEntries(metabolitesIdentificationId.mz.head).map( entry => entry("ID")).mkString(",")+";")
          bw.write(configJson.getEntriesBaseRef(familyMetabolite,metabolitesIdentificationId.mz.head).mkString(",")+";")
          bw.write(s"${metabolitesIdentificationId.rt};")

          if ( metabolitesIdentificationId.mz.head < mzCoreStructureGlucosinolate ) {
            bw.write("*;")
          } else
            bw.write(";")
          val c : Int = (metabolitesIdentificationId.neutralLosses.values.flatten.size)+(metabolitesIdentificationId.daughterIons.values.flatten.size)
          bw.write(s"$c;")

          neutralLosses.foreach {  name => bw.write(s"${metabolitesIdentificationId.neutralLosses(name).getOrElse("")};")}
          daughterIons.foreach {  name => bw.write(s"${metabolitesIdentificationId.daughterIons(name).getOrElse("")};")}
          bw.write("\n")
        }
      )
      bw.close()
    }
  }
}
