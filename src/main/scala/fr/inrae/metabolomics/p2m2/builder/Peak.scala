package fr.inrae.metabolomics.p2m2.builder

import upickle.default._
object Peak {
  implicit val rw: ReadWriter[Peak] = macroRW
}

case class Peak(isotope : Int , intensity : Double, abundance : Double, mz : Double)
