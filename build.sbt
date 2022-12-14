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
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  ("org.eclipse.rdf4j" % "rdf4j-storage" % "4.2.0")
    .exclude("commons-codec","commons-codec")
    .exclude("org.glassfish.jaxb","*")
    .exclude("jakarta.xml.bind","jakarta.xml.bind-api"),
  ("org.eclipse.rdf4j" % "rdf4j-rio" % "4.2.0"),
  "org.slf4j" % "slf4j-simple" % "2.0.3",
  "com.lihaoyi" %% "utest" % "0.8.1" % Test,

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
scalacOptions += "-opt:inline,simplify-jumps,compact-locals,nullness-tracking"

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
coverageMinimumBranchTotal := 30
coverageMinimumStmtPerPackage := 70
coverageMinimumBranchPerPackage := 30
coverageMinimumStmtPerFile := 70
coverageMinimumBranchPerFile := 30
coverageFailOnMinimum := true
coverageHighlighting := true


testFrameworks += new TestFramework("utest.runner.Framework")
assembly / target := file("assembly")
assembly / assemblyJarName := "pack.jar"


assembly / assemblyMergeStrategy := {
  case x if x.endsWith("module-info.class") => MergeStrategy.discard
  case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assembly / assemblyMergeStrategy ).value
    oldStrategy(x)
}


Global / onChangedBuildSource := ReloadOnSourceChanges