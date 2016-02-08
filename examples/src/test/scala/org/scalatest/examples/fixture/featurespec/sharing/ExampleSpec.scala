/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalatest.examples.fixture.featurespec.sharing

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

trait DbFixture { this: fixture.TestSuite =>

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

class ExampleSpec extends fixture.FeatureSpec with DbFixture {

  override def populateDb(db: Db) { // setup the fixture
    db.append("ScalaTest is designed to ")
  }

  feature("Simplicity") {

    scenario("User needs to read test code written by others") { db =>
      db.append("encourage clear code!")
      assert(db.toString === "ScalaTest is designed to encourage clear code!")
    }
    
    scenario("User needs to understand what the tests are doing") { db =>
      db.append("be easy to reason about!")
      assert(db.toString === "ScalaTest is designed to be easy to reason about!")
    }

    scenario("User needs to write tests") { () =>
      val buf = new StringBuffer
      buf.append("ScalaTest is designed to be ")
      buf.append("easy to learn!")
      assert(buf.toString === "ScalaTest is designed to be easy to learn!")
    }
  }
}
