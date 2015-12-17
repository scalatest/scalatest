package org.scalatest.examples.fixture.asyncwordspec.sharing

import java.util.concurrent.ConcurrentHashMap
import org.scalatest._
import DbServer._
import java.util.UUID.randomUUID
import scala.concurrent.Future

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

trait DbFixture { this: fixture.AsyncSuite =&gt;

  type FixtureParam = Db

  // Allow clients to populate the database after
  // it is created
  def populateDb(db: Db) {}

  def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] = {
    val dbName = randomUUID.toString
    val db = createDb(dbName) // create the fixture
    withCleanup {
      populateDb(db) // setup the fixture
      withAsyncFixture(test.toNoArgAsyncTest(db)) // "loan" the fixture to the test
    } {
      removeDb(dbName) // ensure the fixture will be cleaned up
    }
  }
}

class ExampleSpec extends fixture.AsyncWordSpec with DbFixture {

  override def populateDb(db: Db) { // setup the fixture
    db.append("ScalaTest is ")
  }

  "Testing" should {
    "be easy" in { db =&gt;
      Future {
        db.append("easy!")
        assert(db.toString === "ScalaTest is easy!")
      }
    }

    "be fun" in { db =&gt;
      Future {
        db.append("fun!")
        assert(db.toString === "ScalaTest is fun!")
      }
    }
  }

  "Testing code" should {
    // This test doesn't need a Db
    "be clear" in { () =&gt;
      Future {
        val buf = new StringBuffer
        buf.append("ScalaTest code is ")
        buf.append("clear!")
        assert(buf.toString === "ScalaTest code is clear!")
      }
    }
  }
}
