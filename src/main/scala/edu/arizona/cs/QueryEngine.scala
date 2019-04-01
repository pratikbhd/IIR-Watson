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
import org.apache.lucene.store.RAMDirectory
import org.apache.lucene.index.IndexWriterConfig.OpenMode
import org.apache.lucene.search.Query
import org.apache.lucene.search.ScoreDoc
import org.apache.lucene.search.TopDocs
import org.apache.lucene.store.Directory

import scala.collection.mutable.ListBuffer
import java.io.File
import java.nio.file.{Files, Paths}
import java.io.IOException
import java.util.Scanner

import QueryEngine._
import org.apache.lucene.util.BytesRef

import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.collection.immutable

object QueryEngine {

  def main(args: Array[String]): Unit = {
    try {
      println("********Welcome to  UofA Watson********")
      val objQueryEngine: QueryEngine = new QueryEngine()
      objQueryEngine.parseDocuments()
//      val ans2: ListBuffer[ResultClass] = objQueryEngine.runQ1(query13a)
    } catch {
      case ex: Exception => println(ex.getMessage)
    }
  }
}

class QueryEngine() {

  def parseDocuments(): Unit = {
    
  }

  def indexDocuments(w: IndexWriter): Unit = {
    val source = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream(input_file))
    // Read in the file line by line
    for(line <- source.getLines()) {
      val doc: Document = new Document()
      val wordList: Array[String] = line.split(" ")
      val docID = wordList(0)
      val textList: Array[String] = wordList.slice(1, wordList.length)
      val textString = textList.mkString(" ")
      doc.add(new TextField("title", textString, Field.Store.YES))
      doc.add(new StringField("docid", docID, Field.Store.YES))
      w.addDocument(doc)

    }
    source.close()
  }

  def runQ1(query: List[String]): ListBuffer[ResultClass] = {

    // USING DEFAULT SIMILARITY MODEL
    var doc_score_list = new ListBuffer[ResultClass]()
    val analyzer = new StandardAnalyzer

    val index = new RAMDirectory
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)

    val w = new IndexWriter(index, config)
    indexDocuments(w)

    w.close()

    val queryString = query.mkString(" ")
    val q = new QueryParser("title", analyzer).parse(queryString)


    val totalHits: Int = 10
    val reader: IndexReader = DirectoryReader.open(index)
    val searcher: IndexSearcher = new IndexSearcher(reader)

    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)

    val hits = collector.topDocs().scoreDocs

    var count = 1
    println("Query Results: Problem 1a")
    System.out.println("Found " + hits.length + " hits.")
    for (hit <- hits) {
      val objResultClass: ResultClass = new ResultClass()
      val docId: Int = hit.doc
      val score = hit.score
      if (score > 0) {
        val doc = searcher.doc(docId)
        objResultClass.DocName = doc
        objResultClass.doc_score = score
        doc_score_list += objResultClass
        println("Hit Number: " + count + " - Document Name: " + objResultClass.DocName.get("docid") + ", Score: " + score)
        count += 1
      }
    }
    println("\n")
    reader.close()

    // USING TFIDF SIMILARITY MODEL
    var doc_score_list_2 = new ListBuffer[ResultClass]()
    val analyzer_2 = new StandardAnalyzer

    val index_2 = new RAMDirectory
    val config_2 = new IndexWriterConfig(analyzer_2)
    config_2.setOpenMode(OpenMode.CREATE)
    config_2.setSimilarity(new ClassicSimilarity)
    val w_2 = new IndexWriter(index_2, config_2)
    indexDocuments(w_2)

    w_2.close()

    val queryString_2 = query.mkString(" ")
    val q_2 = new QueryParser("title", analyzer_2).parse(queryString_2)


    val totalHits_2: Int = 10
    val reader_2: IndexReader = DirectoryReader.open(index_2)
    val searcher_2: IndexSearcher = new IndexSearcher(reader_2)
    searcher_2.setSimilarity(new ClassicSimilarity)

    val collector_2 = TopScoreDocCollector.create(totalHits_2)
    searcher_2.search(q_2, collector_2)

    val hits_2 = collector_2.topDocs().scoreDocs

    var count_2 = 1
    println("Query Results: Problem 1a - TFIDF")
    System.out.println("Found " + hits_2.length + " hits.")
    for (hit_2 <- hits_2) {
      val objResultClass_2: ResultClass = new ResultClass()
      val docId_2: Int = hit_2.doc
      val score_2 = hit_2.score
      if (score_2 > 0) {
        val doc_2 = searcher_2.doc(docId_2)
        objResultClass_2.DocName = doc_2
        objResultClass_2.doc_score = score_2
        doc_score_list_2 += objResultClass_2
        println("Hit Number: " + count_2 + " - Document Name: " + objResultClass_2.DocName.get("docid") + ", Score: " + score_2)
        count_2 += 1
      }
    }
    println("\n")
    reader_2.close()

    return doc_score_list
  }


  def runQ13a(query: List[String]): ListBuffer[ResultClass] = {

    var doc_list = new ListBuffer[ResultClass]()
    val analyzer = new StandardAnalyzer

    val index = new RAMDirectory
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)

    val w = new IndexWriter(index, config)
    indexDocuments(w)

    w.close()

    val queryString = "\"" + query(0) + "\" AND \"" + query(1) + "\""
    val q = new QueryParser("title", analyzer).parse(queryString)

    val totalHits: Int = 10
    val reader: IndexReader = DirectoryReader.open(index)
    val searcher: IndexSearcher = new IndexSearcher(reader)

    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)

    val hits = collector.topDocs().scoreDocs

    var count = 1
    println("Query Results: Problem 13a")
    System.out.println("Found " + hits.length + " hits.")
    for (hit <- hits) {
      val objResultClass: ResultClass = new ResultClass()
      val docId: Int = hit.doc
      val score = hit.score
      if (score > 0) {
        val doc = searcher.doc(docId)
        objResultClass.DocName = doc
        objResultClass.doc_score = score
        doc_list += objResultClass
        println("Hit Number: " + count + " - Document Name: " + objResultClass.DocName.get("docid") + ", Score: " + score)
        count += 1
      }
    }
    println("\n")
    reader.close()

    return doc_list
  }

  def runQ13b(query: List[String]): ListBuffer[ResultClass] = {

    var doc_list = new ListBuffer[ResultClass]()
    val analyzer = new StandardAnalyzer

    val index = new RAMDirectory
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)

    val w = new IndexWriter(index, config)
    indexDocuments(w)

    w.close()

    val queryString = "\"" + query(0) + "\" NOT \"" + query(1) + "\""
    val q = new QueryParser("title", analyzer).parse(queryString)

    val totalHits: Int = 10
    val reader: IndexReader = DirectoryReader.open(index)
    val searcher: IndexSearcher = new IndexSearcher(reader)

    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)

    val hits = collector.topDocs().scoreDocs

    var count = 1
    println("Query Results: Problem 13b")
    System.out.println("Found " + hits.length + " hits.")
    for (hit <- hits) {
      val objResultClass: ResultClass = new ResultClass()
      val docId: Int = hit.doc
      val score = hit.score
      if (score > 0) {
        val doc = searcher.doc(docId)
        objResultClass.DocName = doc
        objResultClass.doc_score = score
        doc_list += objResultClass
        println("Hit Number: " + count + " - Document Name: " + objResultClass.DocName.get("docid") + ", Score: " + score)
        count += 1
      }
    }
    println("\n")
    reader.close()

    return doc_list
  }

  def runQ13c(query: List[String]): ListBuffer[ResultClass] = {

    var doc_list = new ListBuffer[ResultClass]()
    val analyzer = new StandardAnalyzer

    val index = new RAMDirectory
    val config = new IndexWriterConfig(analyzer)
    config.setOpenMode(OpenMode.CREATE)

    val w = new IndexWriter(index, config)
    indexDocuments(w)

    w.close()

    val queryString = "\"" + query(0) + " " + query(1) + "\"~1"
    val q = new QueryParser("title", analyzer).parse(queryString)

    val totalHits: Int = 10
    val reader: IndexReader = DirectoryReader.open(index)
    val searcher: IndexSearcher = new IndexSearcher(reader)

    val collector = TopScoreDocCollector.create(totalHits)
    searcher.search(q, collector)

    val hits = collector.topDocs().scoreDocs

    var count = 1
    println("Query Results: Problem 13c")
    System.out.println("Found " + hits.length + " hits.")
    for (hit <- hits) {
      val objResultClass: ResultClass = new ResultClass()
      val docId: Int = hit.doc
      val score = hit.score
      if (score > 0) {
        val doc = searcher.doc(docId)
        objResultClass.DocName = doc
        objResultClass.doc_score = score
        doc_list += objResultClass
        println("Hit Number: " + count + " - Document Name: " + objResultClass.DocName.get("docid") + ", Score: " + score)
        count += 1
      }
    }
    println("\n")
    reader.close()

    return doc_list
  }

}