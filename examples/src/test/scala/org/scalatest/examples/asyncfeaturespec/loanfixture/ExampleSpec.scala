/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.examples.asyncfeaturespec.loanfixture

import java.util.concurrent.ConcurrentHashMap

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

object DbServer { // Simulating a database server
  type Db = StringBuffer
  private final val databases = new ConcurrentHashMap[String, Db]
  def createDb(name: String): Db = {
    val db = new StringBuffer // java.lang.StringBuffer is thread-safe
    databases.put(name, db)
    db
  }
  def removeDb(name: String): Unit = {
    databases.remove(name)
  }
}

// Defining actor messages
sealed abstract class StringOp
case object Clear extends StringOp
case class Append(value: String) extends StringOp
case object GetValue

class StringActor { // Simulating an actor
  private final val sb = new StringBuilder
  def !(op: StringOp): Unit =
    synchronized {
      op match {
        case Append(value) => sb.append(value)
        case Clear => sb.clear()
      }
    }
  def ?(get: GetValue.type)(implicit c: ExecutionContext): Future[String] =
    Future {
      synchronized { sb.toString }
    }
}

import org.scalatest._
import DbServer._
import java.util.UUID.randomUUID

class ExampleSpec extends AsyncFeatureSpec {

  def withDatabase(testCode: Future[Db] => Future[Assertion]) = {
    val dbName = randomUUID.toString // generate a unique db name
    val futureDb = Future { createDb(dbName) } // create the fixture
    withCleanup {
      val futurePopulatedDb =
        futureDb map { db =>
          db.append("ScalaTest is designed to ") // perform setup 
        }
      testCode(futurePopulatedDb) // "loan" the fixture to the test code
    } {
      removeDb(dbName) // ensure the fixture will be cleaned up
    }
  }

  def withActor(testCode: StringActor => Future[Assertion]) = {
    val actor = new StringActor
    withCleanup {
      actor ! Append("ScalaTest is designed to ") // set up the fixture
      testCode(actor) // "loan" the fixture to the test code
    } {
      actor ! Clear // ensure the fixture will be cleaned up
    }
  }

  feature("Simplicity") {
    // This test needs the actor fixture
    scenario("User needs to read test code written by others") {
      withActor { actor =>
        actor ! Append("encourage clear code!")
        val futureString = actor ? GetValue
        futureString map { s =>
          assert(s === "ScalaTest is designed to encourage clear code!")
        }
      }
    }
    // This test needs the database fixture
    scenario("User needs to understand what the tests are doing") {
      withDatabase { futureDb =>
        futureDb map { db =>
          db.append("be easy to reason about!")
          assert(db.toString === "ScalaTest is designed to be easy to reason about!")
        }
      }
    }
    // This test needs both the actor and the database
    scenario("User needs to write tests") {
      withDatabase { futureDb =>
        withActor { actor => // loan-fixture methods compose
          actor ! Append("be easy to remember how to write!")
          val futureString = actor ? GetValue
          val futurePair: Future[(Db, String)] =
            futureDb zip futureString
          futurePair map { case (db, s) =>
            db.append("be easy to learn!")
            assert(db.toString === "ScalaTest is designed to be easy to learn!")
            assert(s === "ScalaTest is designed to be easy to remember how to write!")
          }
        }
      }
    }
  }
}

