package fr.inrae.metabolomics.p2m2.config

import fr.inrae.metabolomics.p2m2.config.ConfigReader.Params
import upickle.default._

case object ConfigReader {
  implicit val rw: ReadWriter[ConfigReader] = macroRW
  object Params extends Enumeration {
    implicit val rw: ReadWriter[Params] = readwriter[Int].bimap[Params](x => x.id, Params(_))
    private type Params = Value
    val
    deltaMp0Mp2,
    numberSulfurMin,
    minMzCoreStructure
     = Value
  }

  def read(config : String) : ConfigReader = {
    val u = ujson.read(config)

    val metabolites: Map[String, Map[Params.Value, String]] = u.obj.map(
      k => k._1 -> Map(
        Params.deltaMp0Mp2 -> k._2("deltaMp0Mp2").value.toString,
        Params.numberSulfurMin -> k._2("numberSulfurMin").value.toString,
        Params.minMzCoreStructure -> k._2("minMzCoreStructure").value.toString
      )
    ).toMap

    val nl: Map[String,Map[String,Double]] = u.obj.map(
      k => k._1 -> k._2("neutralLoss").obj.map { case (key, value) => key -> value.value.toString.toDouble }.toMap
      ).toMap

    val di: Map[String, Map[String, Double]] = u.obj.map(
      k => k._1 -> k._2("daughterIons").obj.map { case (key, value) => key -> value.value.toString.toDouble }.toMap
    ).toMap

    /* Reference database */
    val baseRef : Map[String,Map[String,Double]] = u.obj.map(
      k => k._1 -> k._2("databaseReference").obj.map { case (key, value) => key -> value.value.toString.toDouble }.toMap
    ).toMap

    ConfigReader(metabolites,nl,di,baseRef)
  }

}

case class ConfigReader(
                         metabolitesMap: Map[String, Map[Params.Value, String]],
                         nl : Map[String,Map[String,Double]],
                         di:Map[String,Map[String,Double]],
                         baseRef:Map[String,Map[String,Double]]) {

  def metabolites : Seq[String] = metabolitesMap.keys.toSeq
  def deltaMp0Mp2(m: String) : Double = metabolitesMap(m)(Params.deltaMp0Mp2).toString.toDouble
  def numberSulfurMin(m: String) : Double = metabolitesMap(m)(Params.numberSulfurMin).toString.toDouble
  def minMzCoreStructure(m: String) : Double = metabolitesMap(m)(Params.minMzCoreStructure).toString.toDouble
  def neutralLoss(m: String) : Map[String,Double] = nl(m)
  def daughterIons(m: String) : Map[String,Double] = di(m)
  def getEntriesBaseRef(m: String,monoIsotopicMassSearch: Double, tolerance: Double = 0.01): Seq[String] = {
    baseRef(m).filter {
      entry =>
        val m = entry._2
        (monoIsotopicMassSearch > (m - tolerance)) && (monoIsotopicMassSearch < (m + tolerance))
    }.keys.toSeq
  }
}
