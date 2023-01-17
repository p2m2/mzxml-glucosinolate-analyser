package fr.inrae.metabolomics.p2m2

import fr.inrae.metabolomics.p2m2.builder.ScanLoader

import java.io.{BufferedWriter, File, FileWriter}

object MainMgfBuilder extends App {

  import scopt.OParser

  case class Config(
                     mzFile: Option[File] = None,
                     noiseIntensityMS1 : Double = 1000.0,
                     noiseIntensityMS2 : Double = 100.0,
                     diffTime : Double = 5000, //5 sec
                     precision : Int = 2,
                     lowerBoundRtWindowsMS2 : Double = 0.01,
                     upperBoundRtWindowsMS2 : Double = 0.00
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
      opt[Double]('m', "noiseIntensityMS2")
        .optional()
        .action((x, c) => c.copy(noiseIntensityMS2 = x))
        .text(s"tolerance accepted in MS2. ${Config().noiseIntensityMS2}"),
      opt[Double]('d', "diffTime")
        .optional()
        .action((x, c) => c.copy(diffTime = x))
        .text(s"RT Windows size. ${Config().diffTime} "),
      opt[Int]('p', "precision")
        .optional()
        .action((x, c) => c.copy(diffTime = x))
        .text(s"MZ precision. ${Config().precision} "),
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
    case _ => System.err.println("Ko")
    // arguments are bad, error message will have been displayed
  }

  def rtInMs(rt : Double) = rt*1000*60
  case class Peak(mz:Double, rt : Double, intensity: Double,scanId: Int,polarity : String)

  def process(config: Config): Unit = {
    println(
      s"""
        |==================================================
        |Build MGF
        |Path file                         : ${config.mzFile.head.getPath}
        |Noise intensity  MS1              : ${config.noiseIntensityMS1}
        |Noise intensity  MS2              : ${config.noiseIntensityMS2}
        |Threshold for diff time in ms     : ${config.diffTime} ms
        |Precision MZ                      : ${config.precision}
        |Lower bound / Windows RT time MS2 : - ${config.lowerBoundRtWindowsMS2}
        |Upper bound / Windows RT time MS2 : - ${config.upperBoundRtWindowsMS2}
        |
        |==================================================
        |""".stripMargin)

    config.mzFile match {
      case Some(f) => {
        val (source, index) = ScanLoader.read(f)
        val noiseIntensityMS1 = config.noiseIntensityMS1
        val noiseIntensityMS2 = config.noiseIntensityMS2
        val start: Option[Double] = Some(0.5)
        val end: Option[Double] = None

        val bw = new BufferedWriter(new FileWriter(new File(s"${f.getName}.mgf")))

        ScanLoader
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
              .map { case (mz, idx) => (round(mz,config.precision),
                Peak(
                  mz,
                  scan.getRt,
                  spectrum.getIntensities()(idx),
                  scan.getNum,
                  polarity=scan.getPolarity.toString
                )
              ) }
              // RT : we working in ms !!!
        }.groupBy( x  => {
          x._1
          })
           // .filter(_._1>422.04)
          //  .filter(_._1<422.06)
            .map( f => (f._1,f._2.map(_._2).sortBy( (x : Peak)  => x.rt) ))

        // R contains (Mz , => List Of [Mz, RT (ms), Intensity]
         //mergin all windows that RT is in the same windows !
          .map {
            case (feature, listPeakDetection) => {

              val elapsedTimes =
                listPeakDetection.dropRight(1) zip listPeakDetection.drop(1) map {
                  case (peak1: Peak, peak2: Peak) => rtInMs(peak2.rt) - rtInMs(peak1.rt)
                }

              ((feature, listPeakDetection.head) +: (listPeakDetection.drop(1) zip elapsedTimes).filter {
                case (_, diffTime: Double) => diffTime > config.diffTime
              }.map {
                case (peak: Peak, _: Double) => (feature, peak)
              }).distinct
            }

          }
          .flatten
          .zipWithIndex
          .foreach {
            case ((feature, peak: Peak), idx) => {
              println(feature,peak.rt)

              bw.write("BEGIN IONS\n")
              bw.write(s"FEATURE_ID=$idx\n")
              bw.write(s"PEPMASS=$feature\n")
              bw.write(s"SCANS=${peak.scanId}\n")
              bw.write(s"RTINSECONDS=${peak.rt*60}\n")
              bw.write(s"CHARGE=1${peak.polarity}\n")
              bw.write(s"MSLEVEL=2\n")

              ScanLoader
                .scansMs(source, index,
                  Some(peak.rt-config.lowerBoundRtWindowsMS2), Some(peak.rt+config.upperBoundRtWindowsMS2), 2)
                .flatMap {
                case (basicScan) =>
                  val scan = source.parseScan(basicScan.getNum, true)
                  val spectrum = scan.fetchSpectrum()
                  val mzValues = spectrum.getMZs

                  mzValues
                    .zipWithIndex
                    .filter { case (_, idx2) =>
                      spectrum.getIntensities()(idx2) > noiseIntensityMS2
                    }
                    .map{
                      case (mz : Double,idx2 : Int) =>
                        (round(mz,config.precision),round(spectrum.getIntensities()(idx2),5) )
                    }
                }.sortBy(_._1)
                .foreach {
                  case (mz, intensity) =>
                    bw.write(s"${mz} ${intensity}\n")
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
