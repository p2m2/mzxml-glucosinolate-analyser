package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import upickle.default._

object FormatIonsIdentification {
  implicit val rw: ReadWriter[FormatIonsIdentification] = macroRW
}

case class FormatIonsIdentification(list: Seq[IonsIdentification],
                                    familyMetabolite: String,
                                    configJson: ConfigReader)
