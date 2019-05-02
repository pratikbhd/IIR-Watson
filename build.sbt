name := "watsonProject"

version := "1.0"

organization := "edu.arizona.cs"

scalaVersion := "2.12.8"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies ++= {
  val procVer = "7.4.2"
  
  Seq(
    "org.apache.lucene" % "lucene-core" % "7.7.1",
    "org.apache.lucene" % "lucene-queryparser" % "7.7.1",
    "org.apache.lucene" % "lucene-analyzers-common" % "7.7.1",
    "org.apache.lucene" % "lucene-highlighter" % "7.7.1",
    "org.scalactic" %% "scalactic" % "3.0.5",
    "org.scalatest" %% "scalatest" % "3.0.5" % "test",
    "org.slf4j" % "slf4j-api" % "1.7.10",
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-odin" % procVer,
    "org.clulab" %% "processors-modelsmain" % procVer,
    "org.clulab" %% "processors-modelscorenlp" % procVer,
  )
}
