package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification

import java.io.File

case object JsonIonsIdentificationFile {
  def build(list: Seq[IonsIdentification],
            familyMetabolite: String,
            configJson: ConfigReader, out: File): Unit = {

  }
}
