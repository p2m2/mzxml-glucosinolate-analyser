package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.output.CsvMetabolitesIdentification

import java.io.{BufferedWriter, File, FileWriter}

case object CsvMetabolitesIdentificationFile {
  def build(list : Seq[CsvMetabolitesIdentification],out: File) : Unit = {
    if ( list.nonEmpty ) {
      val size = list.last.mz.length
      val neutralLosses = list.last.neutralLosses.keys
      val daughterIons = list.last.daughterIons.keys

      val bw = new BufferedWriter(new FileWriter(out))

      /* Header */

      0.until(size).foreach(
        i => bw.write(s"ms+0;intensity+0;abundance+0;ms+2;intensity+2;abundance+2;")
      )
      bw.write("RT;")
      neutralLosses.foreach {  name => bw.write(s"$name;")}
      daughterIons.foreach {  name => bw.write(s"$name;")}
      bw.write("\n")

      list.foreach(
        metabolitesIdentificationId => {
          0.until(size).foreach(
            i => bw.write(s"${metabolitesIdentificationId.mz(i)};"+
              s"${metabolitesIdentificationId.intensity(i)};"+
              s"${metabolitesIdentificationId.abundance(i)};")
          )
          bw.write(s"${metabolitesIdentificationId.rt};")
          neutralLosses.foreach {  name => bw.write(s"${metabolitesIdentificationId.neutralLosses(name).getOrElse("")};")}
          daughterIons.foreach {  name => bw.write(s"${metabolitesIdentificationId.daughterIons(name).getOrElse("")};")}
          bw.write("\n")
        }
      )
      bw.close()
    }
  }
}
