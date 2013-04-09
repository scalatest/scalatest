package org.scalatest.examples.fixture.spec.sharing

import java.util.concurrent.ConcurrentHashMap
import org.scalatest._
import DbServer._
import java.util.UUID.randomUUID
 
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
 
trait DbFixture { this: fixture.Suite =>
 
  type FixtureParam = Db
 
  // Allow clients to populate the database after
  // it is created
  def populateDb(db: Db) {}
 
  def withFixture(test: OneArgTest): Outcome = {
    val dbName = randomUUID.toString
    val db = createDb(dbName) // create the fixture
    try {
      populateDb(db) // setup the fixture
      withFixture(test.toNoArgTest(db)) // "loan" the fixture to the test
    }
    finally removeDb(dbName) // clean up the fixture
  }
}
 
class ExampleSpec extends fixture.Spec with DbFixture {
 
  override def populateDb(db: Db) { // setup the fixture
    db.append("ScalaTest is ")
  }
 
  object `Testing with ScalaTest` {
    def `should be easy` (db: Db) {
      db.append("easy!")
      assert(db.toString === "ScalaTest is easy!")
    }
     
    def `should be fun` (db: Db) {
      db.append("fun!")
      assert(db.toString === "ScalaTest is fun!")
    }
  }
  
  // This test doesn't need a Db
  object `Test code` {
    def `should be clear` {
      val buf = new StringBuffer
      buf.append("ScalaTest code is ")
      buf.append("clear!")
      assert(buf.toString === "ScalaTest code is clear!")
    }
  }
}
