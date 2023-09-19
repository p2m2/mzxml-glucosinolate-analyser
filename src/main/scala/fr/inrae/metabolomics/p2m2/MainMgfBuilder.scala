package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.builder.ScanLoader

import java.io.{BufferedWriter, File, FileWriter}

object MainMgfBuilder extends App {

  import scopt.OParser

  case class Config(
                     mzFile: Option[File] = None,
                     noiseIntensityMS1 : Double = 5000.0,
                     percentKeepMs2Fragment : Double = 0.1,
                     diffTime : Double = 10000, //5 sec
                     precisionMz : Int = 2,
                     precisionMzFragment : Int = 0,
                     precisionIntensityFragment : Int = 1,
                     lowerBoundRtWindowsMS2 : Double = 0.05,
                     upperBoundRtWindowsMS2 : Double = 0.05,
                   )

  val builder = OParser.builder[Config]
  val parser1 = {
    import builder._
    OParser.sequence(
      programName("mgf-builder"),
      head("mgf-builder", "1.0"),
      opt[Double]('n', "noiseIntensityMS1")
        .optional()
        .action((x, c) => c.copy(noiseIntensityMS1 = x))
        .text(s"tolerance accepted in MS1. ${Config().noiseIntensityMS1}"),
      opt[Double]('m', "percentKeepMs2Fragment")
        .optional()
        .action((x, c) => c.copy(percentKeepMs2Fragment = x))
        .text(s"tolerance accepted in MS2. ${Config().percentKeepMs2Fragment}"),
      opt[Double]('d', "diffTime")
        .optional()
        .action((x, c) => c.copy(diffTime = x))
        .text(s"RT Windows size. ${Config().diffTime} "),
      opt[Int]('p', "precision")
        .optional()
        .action((x, c) => c.copy(precisionMz = x))
        .text(s"MZ precision. ${Config().precisionMz} "),
      opt[Int]('q', "precisionFragment")
        .optional()
        .action((x, c) => c.copy(precisionMzFragment = x))
        .text(s"MZ precisionMzFragment. ${Config().precisionMzFragment} "),
      opt[Int]('r', "precisionIntensityFragment")
        .optional()
        .action((x, c) => c.copy(precisionIntensityFragment = x))
        .text(s"MZ precisionIntensityFragment. ${Config().precisionIntensityFragment} "),
      arg[File]("<file>...")
        .action((x, c) => c.copy(mzFile = Some(x))),
      help("help").text("prints this usage text"),
      note("some notes." + sys.props("line.separator")),
      checkConfig(_ => success)
    )
  }

  def round(v : Double, precision : Int)  = {
    val prec : Int = (0.until(precision)).map(_ => 10).product
    (v * prec).round / prec.toDouble
  }

  // OParser.parse returns Option[Config]
  OParser.parse(parser1, args, Config()) match {
    case Some(config) =>
      process(config)
    // do something
    case _ => 
      System.err.println("Ko")
    // arguments are bad, error message will have been displayed
      throw new java.lang.IllegalArgumentException("bad arguments")
  }

  def rtInMs(rt : Double) = rt*1000*60
  case class Peak(mz:Double, rt : Double, intensity: Double,scanId: Seq[Int],polarity : String)

  def process(config: Config): Unit = {
    println(
      s"""
        |==================================================
        |Build MGF
        |Path file                         : ${config.mzFile.head.getPath}
        |Noise intensity  MS1              : ${config.noiseIntensityMS1}
        |percentKeepMs2Fragment            : ${config.percentKeepMs2Fragment}
        |Threshold for diff time in ms     : ${config.diffTime} ms
        |Precision MZ                      : ${config.precisionMz}
        |Lower bound / Windows RT time MS2 : - ${config.lowerBoundRtWindowsMS2}
        |Upper bound / Windows RT time MS2 : - ${config.upperBoundRtWindowsMS2}
        |
        |==================================================
        |""".stripMargin)

    config.mzFile match {
      case Some(f) => {
        val (source, index) = ScanLoader.read(f)
        val noiseIntensityMS1 = config.noiseIntensityMS1
        val start: Option[Double] = None
        val end: Option[Double] = None

        val peaksMS1 = ScanLoader
          .scansMs(source, index, start, end, 1)
          .flatMap {
          case (basicScan) =>
            val scan = source.parseScan(basicScan.getNum, true)
            val spectrum = scan.fetchSpectrum()
            val mzValues = spectrum.getMZs

            mzValues
              .zipWithIndex
              .filter { case (_, idx) =>
                spectrum.getIntensities()(idx) > noiseIntensityMS1
              }
              .map { case (mz, idx) =>
                (
                //1 arg : round MZ
                round(mz,config.precisionMz),
                //2 arg : round RT in ms
                ((scan.getRt*60.0*1000.0).toInt / config.diffTime.toInt),
                //3 arg Peak
                Peak(
                  mz,
                  scan.getRt,
                  spectrum.getIntensities()(idx),
                  Seq(scan.getNum),
                  polarity=scan.getPolarity.toString
                )
              ) }
              // RT : we working in ms !!!
        }.groupBy( x  => {
          (x._1,x._2)
          })
        //    .filter(_._1._1>421.01)
        //    .filter(_._1._1<422.032)
            .map( f => (f._1,f._2.map(_._3).sortBy( (x : Peak)  => x.rt) ))

        // R contains (Mz , => List Of [Mz, RT (ms), Intensity]
         //mergin all windows that RT is in the same windows !
          .map {
            case ( (feature, rt),listPeakDetection) => {
              (feature,rt,listPeakDetection.reduceLeft((x,y) => if (x.intensity > y.intensity) x else y))
            }
          }

        val features =
          peaksMS1
          .zipWithIndex
          .map {
            case ((feature,rt, peak: Peak), idx) =>
              val ionsFragments = ScanLoader
                .scansMs(source, index,
                  Some(peak.rt-config.lowerBoundRtWindowsMS2), Some(peak.rt+config.upperBoundRtWindowsMS2), 2)
                .map {
                  case (basicScan) => source.parseScan(basicScan.getNum, true)
                }
                .filter {
                  case (scan) =>
                    (round(scan.getPrecursor.getMzTarget,config.precisionMz) - feature).abs == 0
                }
                .filter {
                  case (scan) =>
                     (scan.fetchSpectrum() != null)
                }
                .flatMap {
                case (scan) =>
                  println(feature, peak.rt,scan.getPrecursor.getMzTarget)
                  val spectrum = scan.fetchSpectrum()
                  val mzValues = spectrum.getMZs
                  mzValues
                    .zipWithIndex
                    .map {
                      case (mz: Double, idx2: Int) =>
                        (round(mz, config.precisionMzFragment), round(spectrum.getIntensities()(idx2), config.precisionIntensityFragment))
                    }

                }
                .groupBy( _._1 )
                .map( x => (x._1,x._2.reduceLeft((x,y) => if (x._2 > y._2) x else y)._2))
                .toSeq
                .sortBy( _._1 )

              val ionsFragments2 = if ( ionsFragments.nonEmpty ) {
                val maxIntensity = ionsFragments.maxBy(_._2)._2
                ionsFragments.filter( x => (x._2 / maxIntensity) > 0.1 )
              } else
                ionsFragments
              ( feature,rt, peak, idx, ionsFragments2 )
          }
          .filter {
            case (_, _, _, _, ionsFragments) => (ionsFragments.nonEmpty)
          }

        val bw = new BufferedWriter(new FileWriter(new File(s"${f.getName}.mgf")))
        features
          .foreach {
          case ( feature,rt, peak, idx, ionsFragments ) => {
            println(feature, peak.rt)

            bw.write("BEGIN IONS\n")
            bw.write(s"FEATURE_ID=$idx\n")
            bw.write(s"PEPMASS=$feature\n")
            bw.write(s"SCANS=${peak.scanId.mkString(" ")}\n")
            bw.write(s"RTINSECONDS=${peak.rt * 60}\n")
            bw.write(s"CHARGE=1${peak.polarity}\n")
            bw.write(s"MSLEVEL=2\n")

            ionsFragments.foreach {
              case (mz, intensity) =>
                bw.write(s"$mz $intensity\n")
            }
            bw.write("END IONS\n\n")
          }
        }
        bw.close()
      }

      case None => System.err.println("missing file !")
    }
  }



}
