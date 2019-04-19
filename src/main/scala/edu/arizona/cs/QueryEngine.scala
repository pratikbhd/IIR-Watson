package edu.arizona.cs
import org.apache.lucene.analysis.standard.StandardAnalyzer
import java.io._

import org.apache.lucene.search.similarities.Similarity
import org.apache.lucene.search.similarities.ClassicSimilarity
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StringField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.queryparser.classic.ParseException
import org.apache.lucene.queryparser.classic.QueryParser
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.TopScoreDocCollector
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.search.Query
import org.apache.lucene.search.ScoreDoc
import org.apache.lucene.search.TopDocs
import org.apache.lucene.store.Directory

import scala.io.StdIn
import scala.collection.mutable.ListBuffer
import java.io.File
import java.io.FileReader
import java.io.FileInputStream
import java.nio.file.{Files, Paths}
import java.io.IOException
import java.security.KeyStore.TrustedCertificateEntry
import java.util.Scanner

import QueryEngine._
import org.apache.lucene.util.BytesRef

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer, StringBuilder}
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

class ibmWatson() {

  // Variable definitions
  // Indexing related
  val filePath = "/Users/pratikbhandari/Documents/UofA/Semester II/Classes/CSC 583 - Text Retrieval and Web Search/Project/Project1/testFolder"
  val indexDir = "indexDir"
  val questionFile = "questions.txt"
  var isIndexed = false
  var accuracyDoc = false

  // Flags for category and content indexing
  var printCategory = false
  var printContent = false

  // Document headings
  var title = new StringBuilder()
  var contents = new StringBuilder()
  var category = new StringBuilder()


  def main(): Unit =
  {
    var result = 0
    var loopFlag = false

    // For Indexing
    val analyzer = new StandardAnalyzer
    val index: FSDirectory = FSDirectory.open(Paths.get(indexDir))
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)
    val w = new IndexWriter(index, config)

      // First execute the indexing operation
      result = performIndexing(w)


    // Query the user about the task they want to perform
    while (!loopFlag) {

      // Ask the user about what they want to do
      println("\n1. Calculate the accuracy of the model.")
      println("2. Enter search query on naive model.")
      println("3. Calculate the accuracy with 'Categories' included.")
      println("4. Exit")
      println("Please enter the desired option number:")

      // Get the user's input
      val userInput = StdIn.readLine().toInt

      userInput match {
        case 1 => checkAccuracy(index, analyzer)
        case 2 => searchNaive(index, analyzer)
        case 3 => checkBetterAccuracy(index, analyzer)
        case 4 => loopFlag = true
        case invIp => println("Invalid input: " + invIp.toString)
      }
    }
  }

  def checkAccuracy(index: FSDirectory, analyzer: StandardAnalyzer): Unit =
  {
    if (!accuracyDoc)
    {
      // Create a new document
      val questionSource = Source.fromFile(questionFile)
      var nextUp = "cat"
      var categ = ""
      var answer = ""
      var query = ""
      var totalCount = 0
      var matchCount = 0
      for (line <- questionSource.getLines)
      {
        if (nextUp == "cat")
        {
          categ = line.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
          nextUp = "query"
        }
        else if (nextUp == "query")
        {
          query = line.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
          nextUp = "answer"
        }
        else if (nextUp == "answer")
        {
          answer = line.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
          nextUp = "blank"
        }
        else if (nextUp == "blank")
        {
//          println(answer)
//          println(categ)
//          println(query)
//          nextUp = "cat"
          val hitCount = 1
          val q = new QueryParser("CONTENTS", analyzer).parse(query)
          val reader: IndexReader = DirectoryReader.open(index)
          val searcher: IndexSearcher = new IndexSearcher(reader)
          val collector = TopScoreDocCollector.create(hitCount)
          searcher.search(q, collector)
          val hits = collector.topDocs().scoreDocs

          for (hit <- hits)
          {
            val docId = hit.doc
            val score = hit.score
            if (score > 0)
            {
              val doc = searcher.doc(docId)
              println(doc.get("TITLE"))
              println(answer)
              if (doc.get("TITLE") == answer)
              {
                matchCount += 1
              }
            }
          }
          totalCount += 1
          reader.close()
          nextUp = "cat"
        }
      }
      val accuracyPt = matchCount.toFloat/totalCount
      println(matchCount)
      println("Accuracy of the model using Precision at 1 (P@1) is: " + accuracyPt*100)
      questionSource.close
//      accuracyDoc = true
    }
  }

  def searchNaive(index: FSDirectory, analyzer: StandardAnalyzer): Unit =
  {
    val testQuery = "Indonesia's largest lizard, it's protected from poachers, though we wish it could breathe fire to do the job itself"
    val queryStr = testQuery.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
    val totalHits = 20

    val q = new QueryParser("CONTENTS", analyzer).parse(queryStr)
    val reader: IndexReader = DirectoryReader.open(index)
    val searcher: IndexSearcher = new IndexSearcher(reader)
    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)
    val hits = collector.topDocs().scoreDocs

    for (hit <- hits)
    {
      val docId = hit.doc
      val score = hit.score
      if (score > 0)
      {
        val doc = searcher.doc(docId)
        println(doc.get("TITLE") + "\t" + score)
      }
    }

    reader.close()

  }

  def performIndexing(w: IndexWriter): Int =
  {
    // First, check if the documents have already been indexed
    val indexDr = new File(indexDir)
    if (indexDr.exists)
    {
      isIndexed = true
    }

    // If documents have not been indexed yet, index the documents
    if (!isIndexed)
    {
      println("Indexing document collection for the first time. Please wait...")
      indexDocuments(w)
      println("Indexing Complete!")
    }
    else {
      println("Documents have already been indexed. Moving on. . .")
    }

    return 1
  }

  def indexDocuments(w: IndexWriter): Unit =
  {
    // The wikipedia entries are stored outside the project folder and will only be indexed once to create the index files
    val fileList = new File(filePath).listFiles()
    var fileNames = new ListBuffer[String]()

    // Go through each file inside the directory, check if they conform to the file standards and append them to the fileList
    for (filename <- fileList)
    {
      if (filename.isFile() && filename.getName().endsWith(".txt"))
      {
        fileNames += filename.getAbsolutePath()
      }
    }
    // Now use the fileList to parse the documents
    parseDocuments(fileNames, w)
    w.close()
  }

  def loadDocument(w: IndexWriter, title: String, category: String, contents: String): Unit =
  {
    val doc: Document = new Document()
    doc.add(new StringField("TITLE", title, Field.Store.YES))
    doc.add(new TextField("CATEGORY", category, Field.Store.YES))
    doc.add(new TextField("CONTENTS", contents, Field.Store.YES))
    w.addDocument(doc)
    println("Document "+title+" added!")
  }

  def parseLine(line: String, w: IndexWriter, file: String): Unit =
  {
    try
    {
      if(line.startsWith("[[") && line.endsWith("]]"))
      {
        val titleText = line.substring(2,line.length-2)
        if (title.nonEmpty && contents.nonEmpty)
        {
          if (category.isEmpty)
          {
            loadDocument(w, title.toString(), "", contents.toString())
          }
          else
          {
            loadDocument(w, title.toString(), category.toString(), contents.toString())
          }
          clearBuffers()
        }

        title = new StringBuilder(titleText)
        printCategory = true
        printContent = true
      }
      else if (line.isEmpty())
      {
        // We are neglecting blank lines
        return
      }
      else if (line.contains("#redirect"))
      {
        // For now, we are neglecting any lines that redirect to other topics
        // TODO Work on this
        return
      }
      else if (line.contains("categories"))
      {
        // TODO Perform lemmatization to the categories using Stanford's CoreNLP
        // For now, just removing punctuation marks
        if (printCategory)
        {
          val subCat = line.substring(line.indexOf(":")+2)
          val catStr = subCat.replaceAll("""[\p{Punct}&&[^.]]""", "")
          category = new StringBuilder(catStr)
          printCategory = false
        }
        else
        {
          val catStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          category.append(catStr)
        }
      }
      else if (line.matches("^==.*==$"))
      {
        // Neglecting all the text in the form of == SOME TEXT ==
        // TODO Do something about this?
        return
      }
      else //Everything else is the content
      {
        if(printContent)
        {
          // TODO Perform lemmatization to the categories using Stanford's CoreNLP
          // For now, just removing punctuation marks
          val contentStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents = new StringBuilder(contentStr)
          printContent = false
        }
        else
        // TODO Handle /tpl
        {
          val contentStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents.append(contentStr)
        }
      }
    }
    catch
      {
      case ex: Exception => println(ex.getMessage)
      }
  }

  def parseDocuments(fileNames: ListBuffer[String], w: IndexWriter): Unit =
  {
    for (file <- fileNames)
    {
      try
      {
        var line: String = null
        val fileObject = new FileReader(file)
        val fileBuffer = new BufferedReader(fileObject)
        val fileBr =  new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))
//        val outFile = new File("output.txt")
//        val fileOutStream = new FileOutputStream(outFile)
//        val writeBuffer = new BufferedWriter(null)
//        var skip = false

        while ( {line = fileBuffer.readLine; line != null})
        {
          parseLine(line.toLowerCase(), w, file)
        }
        fileBuffer.close()
      }
      catch
        {
        case ex: Exception => println(ex.getMessage)
        }
    }
  }

  def clearBuffers(): Unit = {

    if (title.nonEmpty)
    {
      title.setLength(0)
    }
    if (contents.nonEmpty)
    {
      contents.setLength(0)
    }
    if (category.nonEmpty)
    {
      category.setLength(0)
    }
  }

}


object QueryEngine
{
  def main(args: Array[String]): Unit = {

    // Welcome Message
    println("\n######################################################")
    println("######################################################\n")
    println("Welcome to IBM Watson - The Course Project for CSC 583\n")
    println("######################################################")
    println("######################################################\n")

    // Start: Create an object for class ibmWatson
    val objWatson: ibmWatson = new ibmWatson()

    // All computation done from ibmWatson's main
    objWatson.main()
  }
}