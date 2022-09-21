package fr.inrae.metabolomics.p2m2.parser

import scala.io.Source

case class OboParser(oboFile : String) extends Iterator[String]{

    private val file = io.Source.fromFile(oboFile)
    private val itr = file.getLines().buffered
    private val header = "\\[.*\\]".r //adjust to taste

    def nextHeader() : String = {
      while (itr.hasNext && !header.matches(itr.head)) itr.next()
      itr.next()
    }

    def next(): String = {
      val sb = new StringBuilder()
      sb.append(itr.next() + "\n")
      while (itr.hasNext && !header.matches(itr.head))
        sb.append(itr.next() + "\n")
      sb.mkString
    }

    def hasNext: Boolean =
      if (itr.hasNext) true else {
        file.close(); false
      }

    def termFilter(
                    chargeRegx : Option[String] = None,
                    formulaRegx : Option[String] = None ) : Option[Map[String,String]] = {
      while ( nextHeader() != "[Term]" ) next()
      val toParse = next().split("\n")

      val m : Map[String, String] = (toParse flatMap {
        case s"id: $chebid" => Some("id" -> chebid)
        case s"name: $name" => Some("name" -> name)
        case s"subset: $subset" => Some("subset" -> subset)
        case s"synonym: $synonym" => None //not managed
        case s"""property_value: http://purl.obolibrary.org/obo/chebi/mass "${mass}" xsd:string""" => Some("mass"->mass)
        case s"""property_value: http://purl.obolibrary.org/obo/chebi/formula "${formula}" xsd:string""" => Some("formula"->formula)
        case s"""property_value: http://purl.obolibrary.org/obo/chebi/inchi "${inchi}" xsd:string""" => Some("inchi"->inchi)
        case s"""property_value: http://purl.obolibrary.org/obo/chebi/inchikey "${inchikey}" xsd:string""" => Some("inchikey"->inchikey)
        case s"""property_value: http://purl.obolibrary.org/obo/chebi/smiles "${smiles}" xsd:string""" => Some("smiles"->smiles)
        case s"""property_value: http://purl.obolibrary.org/obo/chebi/charge "${charge}" xsd:string""" => Some("charge" -> charge)
        case s"""property_value: http://purl.obolibrary.org/obo/chebi/monoisotopicmass "${monoisotopicmass}" xsd:string"""
        => Some("monoisotopicmass" -> monoisotopicmass)
        case u => None
      }).toSeq.toMap

      val v1 = chargeRegx match {
        case Some(v) => m.getOrElse("charge","").contains(v)
        case None => true
      }

      val v2 = formulaRegx match {
        case Some(v) => m.getOrElse("formula", "").contains(v)
        case None => true
      }
      if (v1 && v2)
        Some(m)
      else None
    }
  }
  /*
  val sFile = "/home/ofilangi/workspace/GITHUB/test-mzxml-analyser/mzxml-analyser-test/src/test/resources/unipath.obo"

  val s2 = Source.fromFile(sFile)

  val indexTerm = Source.fromFile(sFile).getLines().zipWithIndex.filter {
    case (l, _) => (l == "[Term]")
  } map( _._2 )


//  println(indexTerm.slice(0,10).mkString(","))
  val indexes : Seq[Int] = indexTerm.toArray.toSeq
//  println("========>",indexes  )
  //s2.getLines().slice(0,indexes(0)).toSeq
  (indexes zip indexes.drop(1)).slice(0,2).foreach {
    case (x,y) =>
      println("OKKKK  ",x,y);
      s2.getLines().slice(x,y).toSeq.foreach(println)
  }

}*/
