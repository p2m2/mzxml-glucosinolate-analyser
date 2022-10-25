package fr.inrae.metabolomics.p2m2.`export`

import fr.inrae.metabolomics.p2m2.config.ConfigReader
import fr.inrae.metabolomics.p2m2.output.IonsIdentification
import upickle.default.{read, write}

import java.io.{BufferedReader, BufferedWriter, File, FileReader, FileWriter}
import scala.io.Source
import scala.util.{Failure, Success, Try}

case object IonsIdentificationFile {
  def save(list: Seq[IonsIdentification],
            familyMetabolite: String,
            configJson: ConfigReader, out: File): Unit = {
    val bw = new BufferedWriter(new FileWriter(out))
    bw.write(write(FormatIonsIdentification(list, familyMetabolite, configJson)))
    bw.close()
  }

  def load(in: File) : (Seq[IonsIdentification],String,ConfigReader) = {
    val s = Source.fromFile(in.getPath)
    println(in.getPath)
    Try(read[FormatIonsIdentification](s.getLines().mkString(""))) match {
      case Success(value) => (value.list,value.familyMetabolite,value.configJson)
      case Failure(exception) => throw exception
    }
  }

}
