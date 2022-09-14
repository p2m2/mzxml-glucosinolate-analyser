scalaVersion := "2.13.8"
name := "mzxml-analyser-test"
organization := "com.github.p2m2"
version := "1.0"
libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt" % "4.1.0",
  "com.github.chhh" % "msftbx" % "1.8.8",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
  "com.lihaoyi" %% "utest" % "0.8.1" % Test,
)
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