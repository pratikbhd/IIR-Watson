name := "watsonProject"

version := "1.0"

organization := "edu.arizona.cs"

scalaVersion := "2.12.8"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies ++= Seq(
  "org.apache.lucene" % "lucene-core" % "7.7.1",
  "org.apache.lucene" % "lucene-queryparser" % "7.7.1",
  "org.apache.lucene" % "lucene-analyzers-common" % "7.7.1",
  "org.scalactic" %% "scalactic" % "3.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
