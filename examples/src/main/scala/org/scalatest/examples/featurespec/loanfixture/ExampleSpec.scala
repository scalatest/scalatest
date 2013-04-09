package org.scalatest.examples.featurespec.loanfixture

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

import org.scalatest.FeatureSpec
import DbServer._
import java.util.UUID.randomUUID
import java.io._

class ExampleSpec extends FeatureSpec {

  def withDatabase(testCode: Db => Any) {
    val dbName = randomUUID.toString
    val db = createDb(dbName) // create the fixture
    try {
      db.append("ScalaTest is designed to ") // perform setup
      testCode(db) // "loan" the fixture to the test
    }
    finally removeDb(dbName) // clean up the fixture
  }

  def withFile(testCode: (File, FileWriter) => Any) {
    val file = File.createTempFile("hello", "world") // create the fixture
    val writer = new FileWriter(file)
    try {
      writer.write("ScalaTest is designed to ") // set up the fixture
      testCode(file, writer) // "loan" the fixture to the test
    }
    finally writer.close() // clean up the fixture
  }

  feature("Simplicity") {
    // This test needs the file fixture
    scenario("User needs to read test code written by others") {
      withFile { (file, writer) =>
        writer.write("encourage clear code!")
        writer.flush()
        assert(file.length === 46)
      }
    }
    // This test needs the database fixture
    scenario("User needs to understand what the tests are doing") {
      withDatabase { db =>
        db.append("be easy to reason about!")
        assert(db.toString === "ScalaTest is designed to be easy to reason about!")
      }
    }
    // This test needs both the file and the database
    scenario("User needs to write tests") {
      withDatabase { db =>
        withFile { (file, writer) => // loan-fixture methods compose
          db.append("be easy to learn!")
          writer.write("be easy to remember how to write!")
          writer.flush()
          assert(db.toString === "ScalaTest is designed to be easy to learn!")
          assert(file.length === 58)
        }
      }
    }
  }
}
