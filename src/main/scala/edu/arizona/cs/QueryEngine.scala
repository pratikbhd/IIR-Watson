package edu.arizona.cs

import org.apache.lucene.analysis.standard.StandardAnalyzer
import java.io._

import org.apache.lucene.search.similarities.{BooleanSimilarity, ClassicSimilarity, Similarity}
import org.apache.lucene.document.Document
import org.apache.lucene.document.Field
import org.apache.lucene.document.StringField
import org.apache.lucene.document.TextField
import org.apache.lucene.index.DirectoryReader
import org.apache.lucene.index.IndexReader
import org.apache.lucene.index.IndexWriter
import org.apache.lucene.index.IndexWriterConfig
import org.apache.lucene.queryparser.classic.{MultiFieldQueryParser, ParseException, QueryParser}
import org.apache.lucene.search.IndexSearcher
import org.apache.lucene.search.TopScoreDocCollector
import org.apache.lucene.store.FSDirectory
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.clulab.processors.corenlp.CoreNLPProcessor
import org.clulab.processors.shallownlp.ShallowNLPProcessor
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.processors.Processor
import org.apache.lucene.search.Query
import org.apache.lucene.search.ScoreDoc
import org.apache.lucene.search.TopDocs
import org.apache.lucene.store.Directory
import scala.collection.SortedMap
import scala.collection.immutable.TreeMap
import java.util.regex.Pattern
import scala.math.sqrt
import scala.collection.immutable.ListMap


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
import org.apache.lucene.analysis.core.WhitespaceAnalyzer
import org.apache.lucene.util.BytesRef

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer, StringBuilder}
import scala.collection.{immutable, mutable}
import scala.util.matching.Regex

class ibmWatson() {

  class MyDocument() {
    var d: Document = null
    var score = .0

    def MyDocument(d: Document, score: Float) {
      this.d = d
      this.score = score
    }
  }


  // Indexing related
  val filePath = "/Users/pratikbhandari/Documents/UofA/Semester II/Classes/CSC 583 - Text Retrieval and Web Search/Project/Project1/testFolder"
  val naiveindexDir = "naiveindexDir"
  val improvedindexDir = "improvedindexDir"
  val questionFile = "questions.txt"
  val posInclude = List("JJ", "NN", "NNS", "DT", "JJ", "CD", "NNS", "CC", "JJR", "JJS", "LS", "MD", "NNP", "NNPS", "PDT", "RB", "RBR", "RBS", "UH", "VB", "VBD", "VBG", "VBN", "VBP", "VBZ")
  var isNaiveIndexed = false
  var isImprovedndexed = false
  var accuracyDoc = false

  // Flags for category and content indexing
  var printCategory = false
  var printContent = false

  // Document headings
  var title = new StringBuilder()
  var contents = new StringBuilder()
  var contentsRaw = new StringBuilder()
  var category = new StringBuilder()
  var addedDocs: ListBuffer[String] = ListBuffer[String]()
  var stopWords: ListBuffer[String] = ListBuffer[String]()


  def checkAccuracy(): Unit = {

    var loopFlag: Boolean = false

    while (!loopFlag) {
      println("\nPlease choose a model:\n")
      println("1. Naive Model")
      println("2. Improved Model")
      println("3. Exit")

      // Get the user's input
      val userInput = StdIn.readLine()

      userInput match {
        case "1" => checkAccuracyNaive(); loopFlag = true
        case "2" => checkAccuracyImproved(); loopFlag = true
        case "3" => loopFlag = true
        case invIp => println("Invalid input: " + invIp.toString + " Please try again.")
      }
    }
  }

  def removeStops(query: String): String = {

    var shortQuery: ListBuffer[String] = ListBuffer[String]()
    val queryList = query.split(" ")
    for (token <- queryList) {
      if (!stopWords.contains(token)) {
        shortQuery += token
      }
    }
    val newQuery: String = shortQuery.mkString(" ")
    return newQuery
  }


  def checkAccuracyImproved(): Unit = {

    val indexDr = new File(improvedindexDir)

    if (indexDr.exists) {
      isImprovedndexed = true
    }

    // For Indexing
    val analyzer = new WhitespaceAnalyzer
    val index: FSDirectory = FSDirectory.open(Paths.get(improvedindexDir))
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)
    val w = new IndexWriter(index, config)

    // If documents have not been indexed yet, index the documents
    if (!isImprovedndexed) {
      println("Indexing document collection for the first time. Please wait...")
      indexDocuments(w, "Improved")
      w.close()
      println("Indexing Complete!")
    }
    else {
      println("Documents have already been indexed.")
    }

    if (!accuracyDoc) {
      // Create a new document
      val questionSource = Source.fromFile(questionFile)
      var nextUp = "cat"
      var categ = ""
      var query = ""
      var totalCount = 0
      var matchCount = 0
      var answerList: ArrayBuffer[String] = ArrayBuffer[String]()
      var originalQuery = ""
      var originalAns = ""
      var tempAnswer: Array[String] = Array[String]()
      val queryProc:Processor = new CoreNLPProcessor()
      var docScoreMap: mutable.HashMap[Double, String] = mutable.HashMap[Double, String]()

      val fieldstoSearch:Array[String] = Array("CONTENTS", "CATEGORY")
      val reader: IndexReader = DirectoryReader.open(index)
      val searcher: IndexSearcher = new IndexSearcher(reader)

      val source = Source.fromFile("stopwords.txt")

      for(line <- source.getLines) {
        stopWords += line
      }
      source.close()

      for (line <- questionSource.getLines) {

        if (nextUp == "cat") {

          categ = line.replaceAll(""" \(.*\)$""", "")
          categ = categ.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
          categ = lemmatize(categ, queryProc)
          nextUp = "query"
        }
        else if (nextUp == "query") {

          originalQuery = line
          query = line.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
          query = partOfSpeech(query, queryProc)
          query = lemmatize(query, queryProc)
          query = removeStops(query)

          nextUp = "answer"
        }
        else if (nextUp == "answer") {

          originalAns = line
          tempAnswer = Array[String]()
          answerList = ArrayBuffer[String]()

          if (line.contains("|")) {
            tempAnswer = line.split("\\|")
            for (element <- tempAnswer) {
              answerList += element.toLowerCase()
            }
          }
          else {
            answerList += line.toLowerCase()
          }
          nextUp = "blank"
        }
        else if (nextUp == "blank") {
          println("\n")

          val hitCount = 10
          var isMatched = false
          var reRankScore: Double = 0
          query = categ + " " + query

          val q = new MultiFieldQueryParser(fieldstoSearch, analyzer).parse(query)

          val collector = TopScoreDocCollector.create(hitCount)
          searcher.search(q, collector)
          val hits = collector.topDocs().scoreDocs

          println("Question: " + originalQuery)
          println("Expected answer: " + originalAns.toLowerCase)
          var index = 0
          for (hit <- hits) {
            val docId = hit.doc
            val score = hit.score
            if (score > 0) {
              val doc = searcher.doc(docId)
              println("Estimated answer: " + doc.get("TITLE"))
//              reRankScore = reRank(doc.get("TITLE"), doc.get("CONTENTSRAW"), query)
//              println(reRankScore)
//              docScoreMap(reRankScore) = doc.get("TITLE")
                if (answerList.contains(doc.get("TITLE")) & index == 0) {
                  isMatched = true
                  matchCount += 1
                }
                index += 1
              }
            }
          totalCount += 1
          nextUp = "cat"
        }
      }
      reader.close()
      val accuracyPt = matchCount.toFloat / totalCount
      println("\nTotal match count: " + matchCount)
      println("\nAccuracy of the model using Precision at 1 (P@1) is: " + accuracyPt * 100)
      questionSource.close
    }
  }


  def reRank(docTitle: String, docContents: String, query: String): Double = {

    var answerContext = ListBuffer[String]()
    var candidateLine = ListBuffer[String]()
    var contentList = ListBuffer[String]()
    val lineRegex = Pattern.compile("[^.!?\\s][^.!?]*(?:[.!?](?!['\"]?\\s|$)[^.!?]*)*[.!?]?['\"]?(?=\\s|$)", Pattern.MULTILINE | Pattern.COMMENTS)
    val reMatcher = lineRegex.matcher(docContents)
    while (reMatcher.find) {
      contentList += reMatcher.group()
    }

    for (line <- 0 until contentList.length) {
      if (contentList(line).contains(docTitle)) {
        candidateLine += contentList(line)
        if (line == 0) {
          if (contentList.length > 1) {
            answerContext += contentList(line) + contentList(line + 1)
          }
          else {
            answerContext += contentList(line)
          }
        }
        else if (line == contentList.length-1) {
          answerContext += contentList(line-1) + contentList(line)
        }
        else {
          answerContext += contentList(line-1) + contentList(line) + contentList(line + 1)
        }
      }
    }

    val h0: Int = calculateH0(answerContext, query)
    val h1: Int = calculateH1(answerContext)
    val h2: Int = calculateH2(candidateLine)
    val h3: Int = calculateH3(answerContext)

    val finScore: Double = h0.toFloat/answerContext.length + h1 + h2 - (0.25*sqrt(h3).toFloat)
    return finScore
  }

  def calculateH3(answerContext: ListBuffer[String]): Int = {

    val proc: Processor = new CoreNLPProcessor()
    val questionPOS: List[String] = List("WP", "WP$", "WRB")
    var posIndex: ListBuffer[Int] = ListBuffer[Int]()

    for (line <- answerContext) {
      var newLine = line.replaceAll("""\.""", "")
      val doc = proc.mkDocument(newLine)
      proc.tagPartsOfSpeech(doc)
      proc.lemmatize(doc)
      doc.clear()

      for (sentence <- doc.sentences) {
        val wordsLength = sentence.words.length
        for (index <- 0 to wordsLength - 1) {
          if (questionPOS.contains(sentence.tags.get(index))) {
            posIndex += index
          }
        }
      }
    }

    var spanCount: Int = 0
    if (posIndex.length > 0) {
      spanCount = posIndex(0) - posIndex(posIndex.length - 1) - 1
      if (spanCount < 0) {
        spanCount = 0
      }
    }
    else {
      spanCount = 0
    }

    return spanCount
  }

  def calculateH2(candidateLine: ListBuffer[String]): Int = {


    val proc: Processor = new CoreNLPProcessor()
    val questionPOS: List[String] = List("WP", "WP$", "WRB")
    var questionCount: Int = 0
    for (line <- candidateLine) {
      val doc = proc.mkDocument(line)
      proc.tagPartsOfSpeech(doc)
      proc.lemmatize(doc)
      doc.clear()

      for (sentence <- doc.sentences) {
        val wordsLength = sentence.words.length
        for (index <- 0 to wordsLength - 1) {
          if (questionPOS.contains(sentence.tags.get(index))) {
            questionCount += 1
          }
        }
      }
    }
    return questionCount
  }

  def calculateH1(answerContext: ListBuffer[String]): Int = {

    val proc: Processor = new CoreNLPProcessor()
    val questionPOS: List[String] = List("WP", "WP$", "WRB")
    var questionCount: Int = 0
    for (line <- answerContext) {
      val doc = proc.mkDocument(line)
      proc.tagPartsOfSpeech(doc)
      proc.lemmatize(doc)
      doc.clear()

      for (sentence <- doc.sentences) {
        val wordsLength = sentence.words.length
        for (index <- 0 to wordsLength - 1) {
          if (questionPOS.contains(sentence.tags.get(index))) {
            questionCount += 1
          }
        }
      }
    }
    return questionCount
  }

  def calculateH0(answerContext: ListBuffer[String], query: String): Int = {

    val proc:Processor = new CoreNLPProcessor()
    var wordCount: Int = 0

    var newQuery = lemmatize(query, proc)
    newQuery = removeStops(newQuery)
    val querLst = newQuery.split(" ")

    for (context <- answerContext) {

      var newWord = lemmatize(context, proc)
      newWord = removeStops(newWord)
      val wordLst = newWord.split(" ")

      for (i <- 0 until wordLst.length) {
        for (j <- 0 until querLst.length) {
          if (wordLst(i) == querLst(j)) {
            wordCount += 1
            var increment = 1
            var break = false
            while(!break) {
              if (i+increment < wordLst.length & j+increment < querLst.length) {
              if (wordLst(i+increment) == querLst(j+increment)) {
                increment += 1
                wordCount += 1
              }
              else {
                break = true
              }
            }
              else {
              break = true
              }
            }
          }
        }
      }
    }
    return wordCount
  }

  def checkAccuracyNaive() {

    val indexDr = new File(naiveindexDir)

    if (indexDr.exists) {
      isNaiveIndexed = true
    }

    // For Indexing
    val analyzer = new StandardAnalyzer
    val index: FSDirectory = FSDirectory.open(Paths.get(naiveindexDir))
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)
    val w = new IndexWriter(index, config)

    // If documents have not been indexed yet, index the documents
    if (!isNaiveIndexed) {
      println("Indexing document collection for the first time. Please wait...")
      indexDocuments(w, "Naive")
      w.close()
      println("Indexing Complete!")
    }
    else {
      println("Documents have already been indexed.")
    }

    if (!accuracyDoc) {
      // Create a new document
      val questionSource = Source.fromFile(questionFile)
      var nextUp = "cat"
      var categ = ""
      var answer = ""
      var query = ""
      var totalCount = 0
      var matchCount = 0
      var originalQuery = ""

      for (line <- questionSource.getLines) {
        if (nextUp == "cat") {
          categ = line.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
          nextUp = "query"
        }
        else if (nextUp == "query") {
          originalQuery = line
          query = line.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
          nextUp = "answer"
        }
        else if (nextUp == "answer") {
          answer = line.toLowerCase()
          nextUp = "blank"
        }
        else if (nextUp == "blank") {
          val hitCount = 1
          val q = new QueryParser("CONTENTS", analyzer).parse(query)
          val reader: IndexReader = DirectoryReader.open(index)
          val searcher: IndexSearcher = new IndexSearcher(reader)
          val collector = TopScoreDocCollector.create(hitCount)
          searcher.search(q, collector)
          val hits = collector.topDocs().scoreDocs

          println("Question: " + originalQuery)
          println("Expected answer: " + answer)
          for (hit <- hits) {
            val docId = hit.doc
            val score = hit.score
            if (score > 0) {
              val doc = searcher.doc(docId)
              println("Estimated answer: " + doc.get("TITLE"))
              println("\n")
              if (doc.get("TITLE") == answer) {
                matchCount += 1
              }
            }
          }
          totalCount += 1
          reader.close()
          nextUp = "cat"
        }
      }
      val accuracyPt = matchCount.toFloat / totalCount
      println("\nTotal match count: " + matchCount)
      println("\nAccuracy of the model using Precision at 1 (P@1) is: " + accuracyPt * 100)
      questionSource.close
    }
  }

  def searchQuery(): Unit = {

    var loopFlag: Boolean = false

    while (!loopFlag) {

      println("\nPlease choose a model:\n")
      println("1. Naive Model")
      println("2. Improved Model")
      println("3. Exit")

      // Get the user's input
      val userInput = StdIn.readLine().toInt

      userInput match {
        case 1 => searchNaive(); loopFlag = true
        case 2 => searchImproved(); loopFlag = true
        case 3 => loopFlag = true
        case invIp => println("Invalid input: " + invIp.toString + " Please try again.")
      }
    }
  }

  def searchImproved(): Unit = {

    val indexDr = new File(improvedindexDir)

    if (indexDr.exists) {
      isImprovedndexed = true
    }

    // For Indexing
    val analyzer = new WhitespaceAnalyzer
    val index: FSDirectory = FSDirectory.open(Paths.get(improvedindexDir))
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)
    val w = new IndexWriter(index, config)

    // If documents have not been indexed yet, index the documents
    if (!isImprovedndexed) {
      println("Indexing document collection for the first time. Please wait...")
      indexDocuments(w, "Improved")
      w.close()
      println("Indexing Complete!")
    }
    else {
      println("Documents have already been indexed.")
    }

    println("Please enter the query:")
    var userQuery = StdIn.readLine()

    println("Please enter the category:")
    var userCateg = StdIn.readLine()

    var originalQuery = ""
    var originalCateg = ""
    val queryProc:Processor = new CoreNLPProcessor()
    var docScoreMap: mutable.HashMap[Double, String] = mutable.HashMap[Double, String]()

    val fieldstoSearch:Array[String] = Array("CONTENTS", "CATEGORY")
    val reader: IndexReader = DirectoryReader.open(index)
    val searcher: IndexSearcher = new IndexSearcher(reader)

    val source = Source.fromFile("stopwords.txt")

    for(line <- source.getLines) {
      stopWords += line
    }
    source.close()

    originalCateg = userCateg
    userCateg = userCateg.replaceAll(""" \(.*\)$""", "")
    userCateg = userCateg.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
    userCateg = lemmatize(userCateg, queryProc)

    originalQuery = userQuery
    userQuery = userQuery.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
    userQuery = partOfSpeech(userQuery, queryProc)
    userQuery = lemmatize(userQuery, queryProc)
    userQuery = removeStops(userQuery)

    val hitCount = 10
    var reRankScore: Double = 0
    userQuery = userCateg + " " + userQuery

    val q = new MultiFieldQueryParser(fieldstoSearch, analyzer).parse(userQuery)

    val collector = TopScoreDocCollector.create(hitCount)
    searcher.search(q, collector)
    val hits = collector.topDocs().scoreDocs

    println("Estimated answers with their scores are:\n")
    for (hit <- hits) {
      val docId = hit.doc
      val score = hit.score
      if (score > 0) {
        val doc = searcher.doc(docId)
        println(doc.get("TITLE") + ":\t" + score)
//        reRankScore = reRank(doc.get("TITLE"), doc.get("CONTENTSRAW"), query)
//        println(reRankScore)
//        docScoreMap(reRankScore) = doc.get("TITLE")
      }
    }
    reader.close()
  }


  def searchNaive(): Unit = {

    val indexDr = new File(naiveindexDir)

    if (indexDr.exists) {
      isNaiveIndexed = true
    }

    // For Indexing
    val analyzer = new StandardAnalyzer
    val index: FSDirectory = FSDirectory.open(Paths.get(naiveindexDir))
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)
    val w = new IndexWriter(index, config)

    // If documents have not been indexed yet, index the documents
    if (!isNaiveIndexed) {
      println("Indexing document collection for the first time. Please wait...")
      indexDocuments(w, "Naive")
      w.close()
      println("Indexing Complete!")
    }
    else {
      println("Documents have already been indexed.")
    }

    println("Please enter the query:")
    val userQuery = StdIn.readLine()

    val queryStr = userQuery.toLowerCase().replaceAll("""[\p{Punct}&&[^.]]""", "")
    val totalHits = 10

    val q = new QueryParser("CONTENTS", analyzer).parse(queryStr)
    val reader: IndexReader = DirectoryReader.open(index)
    val searcher: IndexSearcher = new IndexSearcher(reader)
    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)
    val hits = collector.topDocs().scoreDocs

    println("Estimated answers with their scores are:\n")
    for (hit <- hits) {
      val docId = hit.doc
      val score = hit.score
      if (score > 0) {
        val doc = searcher.doc(docId)
        println(doc.get("TITLE") + ":\t" + score)
      }
    }
    reader.close()
  }

  def loadDocument(w: IndexWriter, title: String, category: String, contents: String, lemmaFlag: Boolean, contentRaw: String): Unit = {

    val doc: Document = new Document()
    val proc:Processor = new CoreNLPProcessor()

    var titleText = ""
    var categoryText = ""
    var contentsText = ""
    var contentsRaw = ""

    if (lemmaFlag) {
      titleText = title
      categoryText = lemmatize(category, proc)
      contentsText = lemmatize(contents, proc)
      contentsRaw = contentRaw
    }
    else {
      titleText = title
      categoryText = category
      contentsText = contents
      contentsRaw = contentRaw
    }

    println(titleText)
    println(categoryText)
    println(contentsText)
    println("\n")

    doc.add(new StringField("TITLE", titleText, Field.Store.YES))
    doc.add(new TextField("CATEGORY", categoryText, Field.Store.YES))
    doc.add(new TextField("CONTENTS", contentsText, Field.Store.YES))
    doc.add(new TextField("CONTENTSRAW", contentsRaw, Field.Store.YES))
    w.addDocument(doc)
    println("Document " + title + " added!")
  }

  def lemmatize(inputString: String, proc: Processor): String = {

    val doc = proc.mkDocument(inputString)
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)

    var lemmaWord = ListBuffer[String]()

    for (sentence <- doc.sentences) {
      for (lemma <- sentence.lemmas) {
        for (x <- lemma) {
//          if (x != ".") {
          lemmaWord += x
//          }
        }
      }
    }
    val stringLemma = lemmaWord.mkString(" ")
    doc.clear()
    return stringLemma
  }

  def partOfSpeech(inputString: String, proc: Processor): String = {

    val doc = proc.mkDocument(inputString)
    var posQuery: ListBuffer[String] = ListBuffer[String]()
    proc.tagPartsOfSpeech(doc)
    proc.lemmatize(doc)
    doc.clear()

    for (sentence <- doc.sentences) {
      val wordsLength = sentence.words.length
      //      for (tag <- sentence.tags) {
      //        for (x <- tag) {
      for (index <- 0 to wordsLength-1) {
        if (posInclude.contains(sentence.tags.get(index))) {
          posQuery += sentence.words(index)
        }
//        println("(" + sentence.words(index) + "," + sentence.tags.get(index) + ")")
      }
      //        }
      //      }
    }
    return posQuery.mkString(" ")
  }

  def parseDocumentsNaive(fileNames: ListBuffer[String], w: IndexWriter): Unit = {

    val excludes: List[String] = List("==references==", "==distinctions==", "")
    for (file <- fileNames) {
      try {
        var line: String = null
        val fileObject = new FileReader(file)
        val fileBuffer = new BufferedReader(fileObject)
        val fileBr = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))
        var skip = false

        while ( {
          line = fileBuffer.readLine; line != null
        }) {
          // Skipping a bunch of ==.*== names
          if (!skip && excludes.contains(line.toLowerCase)) {
            skip = true
          }
          if (skip && ((line.matches("==.*==") && !excludes.contains(line.toLowerCase)) || line.matches("^\\[\\[.*\\]\\]$"))) {
            skip = false
          }
          if (!skip) {
            parseLineNaive(line.toLowerCase(), w, file)
          }
        }
        fileBuffer.close()
      }
      catch {
        case ex: Exception => println(ex.getMessage)
      }
    }
  }


  def parseLineNaive(line: String, w: IndexWriter, file: String): Unit = {
    try {
      if (line.startsWith("[[") && line.endsWith("]]")) {
        val titleText = line.substring(2, line.length - 2)
        if (title.nonEmpty && contents.nonEmpty) {
          if (category.isEmpty) {
            loadDocument(w, title.toString(), "", contents.toString(), false, "")
          }
          else {
            loadDocument(w, title.toString(), category.toString(), contents.toString(), false, "")
          }
          clearBuffers()
        }

        title = new StringBuilder(titleText)
        printCategory = true
        printContent = true
      }
      else if (line.isEmpty()) {
        // We are neglecting blank lines
        return
      }
      else if (line.contains("#redirect")) {
        return
      }
      else if (line.contains("categories")) {
        if (printCategory) {
          val subCat = line.substring(line.indexOf(":") + 2)
          val catStr = subCat.replaceAll("""[\p{Punct}&&[^.]]""", "")
          category = new StringBuilder(catStr)
          printCategory = false
        }
        else {
          val catStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          category.append(catStr)
        }
      }
      else if (line.matches("^==.*==$")) {
        return
      }
      else //Everything else is the content
      {
        if (printContent) {
          val contentStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents = new StringBuilder(contentStr)
          printContent = false
        }
        else
        {
          val contentStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents.append(contentStr)
        }
      }
    }
    catch {
      case ex: Exception => println(ex.getMessage)
    }
  }

  def parseDocumentsImproved(fileNames: ListBuffer[String], w: IndexWriter): Unit = {

    val excludes: List[String] = List("==references==", "==distinctions==")
    for (file <- fileNames) {
      try {
        var line: String = null
        val fileObject = new FileReader(file)
        val fileBuffer = new BufferedReader(fileObject)
        val fileBr = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"))
        var skip = false

        while ( {
          line = fileBuffer.readLine; line != null
        }) {
          // Skipping a bunch of ==.*== names.
          if (!skip && excludes.contains(line.toLowerCase)) {
            skip = true
          }
          if (skip && ((line.matches("==.*==") && !excludes.contains(line.toLowerCase)) || line.matches("^\\[\\[.*\\]\\]$"))) {
            skip = false
          }
          if (!skip) {
            if (line != "") {
              parseLineImproved(line.toLowerCase(), w, file)
            }
          }
        }

        // For the last doc in each file
        if (!addedDocs.contains(title.toString())) {
          if (title.nonEmpty && contents.nonEmpty) {
            addedDocs += title.toString()
            if (category.isEmpty) {
              loadDocument(w, title.toString(), "", contents.toString(), true, contentsRaw.toString())
            }
            else {
              loadDocument(w, title.toString(), category.toString(), contents.toString(), true, contentsRaw.toString())
            }
            clearBuffers()
          }
        }

        fileBuffer.close()
      }
      catch {
        case ex: Exception => println(ex.getMessage)
      }
    }
  }

  def parseLineImproved(line: String, w: IndexWriter, file: String): Unit = {
    try {

      if (line.startsWith("[[") && line.endsWith("]]")) {
        val titleText = line.substring(2, line.length - 2)
        if (title.nonEmpty && contents.nonEmpty) {
          addedDocs += title.toString()
          if (category.isEmpty) {
            loadDocument(w, title.toString(), "", contents.toString(), true, contentsRaw.toString())
          }
          else {
            loadDocument(w, title.toString(), category.toString(), contents.toString(), true, contentsRaw.toString())
          }
          clearBuffers()
        }

        title = new StringBuilder(titleText)
        printCategory = true
        printContent = true
      }

      else if (line.contains("#redirect")) {
        // Redirects do not hold a lot of information and are neglected
        return
      }

      else if (line.matches("^categories:.*")) {
        if (printCategory) {
          val subCat = line.substring(line.indexOf(":") + 2)
          val catStr = subCat.replaceAll("""[\p{Punct}&&[^.]]""", "")
          category = new StringBuilder(catStr)
          printCategory = false
        }
        else {
          val catStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          category.append(catStr)
        }
      }

      else if (line.matches("^==.*==$")) {
        // This does not hold valuable information information, neglect it.
        return
      }

      else if (line.matches("^http://.*")) {

        // Lines starting with http do not hold valuable information, remove them
        return
      }

      // Remove all text within and including [tpl]...[/tpl]
      else if (line.contains("[tpl]") && line.contains("[/tpl]")) {

        val subString = new StringBuilder(line)
        var startIndex = 0
        var prevStartIndex = 0
        var endIndex = 0
        var prevendIndex = 0

        while ( { // [tpl]..[/tpl] can be nested. So remove everything.
          subString.toString.contains("[tpl]") && subString.toString.contains("[/tpl]")
        }) {
          startIndex = subString.toString.indexOf("[tpl]")
          endIndex = subString.toString.indexOf("[/tpl]")
          if ((endIndex + 6) <= startIndex) {
            startIndex = prevStartIndex
          }
          prevStartIndex = startIndex
          prevendIndex = endIndex
          subString.replace(startIndex, endIndex + 6, "")
        }
        if (printContent) {
          val content = subString.toString.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents = new StringBuilder(content)
          contentsRaw = new StringBuilder(subString.toString())
          printContent = false
        }
        else {
          val content = subString.toString.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents.append(content)
          contentsRaw.append(subString.toString())
        }
      }

      else //Everything else is the content
      {
        if (printContent) {
          val contentStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents = new StringBuilder(contentStr)
          contentsRaw = new StringBuilder(line)
          printContent = false
        }
        else
        {
          val contentStr = line.replaceAll("""[\p{Punct}&&[^.]]""", "")
          contents.append(contentStr)
          contentsRaw.append(line)
        }
      }
    }
    catch {
      case ex: Exception => println(ex.getMessage)
    }
  }

  def indexDocuments(w: IndexWriter, mode: String): Unit = {

    // The wikipedia entries are stored outside the project folder and will only be indexed once to create the index files
    val fileList = new File(filePath).listFiles()
    var fileNames = new ListBuffer[String]()

    // Go through each file inside the directory, check if they conform to the file standards and append them to the fileList
    for (filename <- fileList) {
      if (filename.isFile && filename.getName.endsWith(".txt")) {
        fileNames += filename.getAbsolutePath
      }
    }

    // Now use the fileList to parse the documents
    if (mode == "Naive") {
      parseDocumentsNaive(fileNames, w)
    }
    else if (mode == "Improved") {
      parseDocumentsImproved(fileNames, w)
    }
  }

  // This function only queries the user about the task they want to perform and passes control to other helper
  // functions respectively.
  def main(): Unit = {

    var loopFlag: Boolean = false

    while (!loopFlag) {

      // Ask the user about what they want to do
      println("\nWhich operation would you like to perform? Please entered the desired option number\n")
      println("1. Calculate the accuracy of a model.")
      println("2. Enter a search query on a model.")
      println("3. Exit")

      // Get the user's input
      val userInput = StdIn.readLine()

      userInput match {
        case "1" => checkAccuracy(); loopFlag = true
        case "2" => searchQuery(); loopFlag = true
        case "3" => loopFlag = true
        case invIp => println("Invalid input: " + invIp.toString + " Please try again.")
      }
    }
  }

  // Clears the title, content and category of the document to start for the next document
  def clearBuffers(): Unit = {

    if (title.nonEmpty) {
      title.setLength(0)
    }
    if (contents.nonEmpty) {
      contents.setLength(0)
    }
    if (category.nonEmpty) {
      category.setLength(0)
    }
  }
}

object QueryEngine
{
  def main(args: Array[String]): Unit = {

    println("\n######################################################")
    println("######################################################\n")
    println("Welcome to IBM Watson - The Course Project for CSC 583\n")
    println("######################################################")
    println("######################################################\n")

    val objWatson: ibmWatson = new ibmWatson()

    // Start the program
    objWatson.main()
  }
}