scalaVersion := "2.13.8"
name := "mzxml-glucosinolates-phenolics-analyser"
organization := "com.github.p2m2"
organizationName := "p2m2"
organizationHomepage := Some(url("https://www6.inrae.fr/p2m2"))
licenses := Seq("MIT License" -> url("http://www.opensource.org/licenses/mit-license.php"))
version := "1.0"

scmInfo := Some(
  ScmInfo(
    url("https://github.com/p2m2/mzxml-glucosinolate-analyser"),
    "scm:git@github.com:p2m2/mzxml-glucosinolate-analyser.git"
  )
)

developers := List(
  Developer("ofilangi", "Olivier Filangi", "olivier.filangi@inrae.fr",url("https://github.com/ofilangi"))
)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.1.0",
  "com.github.chhh" % "msftbx" % "1.8.8",
  "com.lihaoyi" %% "ujson" % "2.0.0",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
  "com.lihaoyi" %% "upickle" % "2.0.0",
 // "org.openscience.cdk" % "cdk-bundle" % "2.8",
  "com.lihaoyi" %% "utest" % "0.8.1" % Test,
  "org.slf4j" % "slf4j-simple" % "2.0.3" % Test,
)

credentials += {
  val realm = scala.util.Properties.envOrElse("REALM_CREDENTIAL", "" )
  val host = scala.util.Properties.envOrElse("HOST_CREDENTIAL", "" )
  val login = scala.util.Properties.envOrElse("LOGIN_CREDENTIAL", "" )
  val pass = scala.util.Properties.envOrElse("PASSWORD_CREDENTIAL", "" )

  val file_credential = Path.userHome / ".sbt" / ".credentials"

  if (reflect.io.File(file_credential).exists) {
    Credentials(file_credential)
  } else {
    Credentials(realm,host,login,pass)
  }
}

publishTo := {
  if (isSnapshot.value)
    Some("Sonatype Snapshots Nexus" at "https://oss.sonatype.org/content/repositories/snapshots")
  else
    Some("Sonatype Snapshots Nexus" at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
}
publishConfiguration := publishConfiguration.value.withOverwrite(true)
publishLocalConfiguration := publishLocalConfiguration.value.withOverwrite(true)
pomIncludeRepository := { _ => false }
publishMavenStyle := true

// Coverage

coverageMinimumStmtTotal := 70
coverageMinimumBranchTotal := 70
coverageMinimumStmtPerPackage := 70
coverageMinimumBranchPerPackage := 70
coverageMinimumStmtPerFile := 70
coverageMinimumBranchPerFile := 70
coverageFailOnMinimum := true
coverageHighlighting := true


testFrameworks += new TestFramework("utest.runner.Framework")
assembly / target := file("assembly")
assembly / assemblyJarName := "pack.jar"
assembly / assemblyMergeStrategy := {
  //case PathList("META-INF", xs @ _*) => MergeStrategy.last
  case "module-info.class"  => MergeStrategy.first
  case x =>
    val oldStrategy = (ThisBuild / assemblyMergeStrategy).value
    oldStrategy(x)
}

Global / onChangedBuildSource := ReloadOnSourceChanges