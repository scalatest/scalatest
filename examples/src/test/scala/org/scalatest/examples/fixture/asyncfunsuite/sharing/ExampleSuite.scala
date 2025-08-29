/*
 * Copyright 2001-2025 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.examples.fixture.asyncfunsuite.sharing

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

trait DbFixture { this: fixture.AsyncTestSuite =>

  type FixtureParam = Db

  // Allow clients to populate the database after
  // it is created
  def populateDb(db: Db) {}

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    val dbName = randomUUID.toString
    val db = createDb(dbName) // create the fixture
    complete {
      populateDb(db) // setup the fixture
      withFixture(test.toNoArgAsyncTest(db)) // "loan" the fixture to the test
    } lastly {
      removeDb(dbName) // ensure the fixture will be cleaned up
    }
  }
}

class ExampleSuite extends fixture.AsyncFunSuite with DbFixture {

  override def populateDb(db: Db) { // setup the fixture
    db.append("ScalaTest is ")
  }

  test("testing should be easy") { db =>
    Future {
      db.append("easy!")
      assert(db.toString === "ScalaTest is easy!")
    }
  }

  test("testing should be fun") { db =>
    Future {
      db.append("fun!")
      assert(db.toString === "ScalaTest is fun!")
    }
  }

  // This test doesn't need a Db
  test("test code should be clear") { () =>
    Future {
      val buf = new StringBuffer
      buf.append("ScalaTest code is ")
      buf.append("clear!")
      assert(buf.toString === "ScalaTest code is clear!")
    }
  }
}
