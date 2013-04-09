package org.scalatest.examples.wordspec.loanfixture

import java.util.concurrent.ConcurrentHashMap

object DbServer { // Simulating a database server
  type Db = StringBuffer
  private val databases = new ConcurrentHashMap[String, Db]
  def createDb(name: String): Db = {
    val db = new StringBuffer
    databases.put(name, db)
    db
  }
  def removeDb(name: String) {
    databases.remove(name)
  }
}

import org.scalatest.WordSpec
import DbServer._
import java.util.UUID.randomUUID
import java.io._

class ExampleSpec extends WordSpec {

  def withDatabase(testCode: Db => Any) {
    val dbName = randomUUID.toString
    val db = createDb(dbName) // create the fixture
    try {
      db.append("ScalaTest is ") // perform setup
      testCode(db) // "loan" the fixture to the test
    }
    finally removeDb(dbName) // clean up the fixture
  }

  def withFile(testCode: (File, FileWriter) => Any) {
    val file = File.createTempFile("hello", "world") // create the fixture
    val writer = new FileWriter(file)
    try {
      writer.write("ScalaTest is ") // set up the fixture
      testCode(file, writer) // "loan" the fixture to the test
    }
    finally writer.close() // clean up the fixture
  }

  "Testing" should {
    // This test needs the file fixture
    "be productive" in withFile { (file, writer) =>
      writer.write("productive!")
      writer.flush()
      assert(file.length === 24)
    }
  }
  
  "Test code" should {
    // This test needs the database fixture
    "be readable" in withDatabase { db =>
      db.append("readable!")
      assert(db.toString === "ScalaTest is readable!")
    }

    // This test needs both the file and the database
    "be clear and concise" in withDatabase { db =>
      withFile { (file, writer) => // loan-fixture methods compose
        db.append("clear!")
        writer.write("concise!")
        writer.flush()
        assert(db.toString === "ScalaTest is clear!")
        assert(file.length === 21)
      }
    }
  }
}
