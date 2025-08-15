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
package org.scalatest.examples.asyncfunspec.loanfixture

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

class ExampleSpec extends AsyncFunSpec {

  def withDatabase(testCode: Future[Db] => Future[Assertion]) = {
    val dbName = randomUUID.toString // generate a unique db name
    val futureDb = Future { createDb(dbName) } // create the fixture
    complete {
      val futurePopulatedDb =
        futureDb map { db =>
          db.append("ScalaTest is ") // perform setup 
        }
      testCode(futurePopulatedDb) // "loan" the fixture to the test code
    } lastly {
      removeDb(dbName) // ensure the fixture will be cleaned up
    }
  }

  def withActor(testCode: StringActor => Future[Assertion]) = {
    val actor = new StringActor
    complete {
      actor ! Append("ScalaTest is ") // set up the fixture
      testCode(actor) // "loan" the fixture to the test code
    } lastly {
      actor ! Clear // ensure the fixture will be cleaned up
    }
  }

  describe("Testing") {
    // This test needs the actor fixture
    it("should be productive") {
      withActor { actor =>
        actor ! Append("productive!")
        val futureString = actor ? GetValue
        futureString map { s =>
          assert(s == "ScalaTest is productive!")
        }
      }
    }
  }

  describe("Test code") {
    // This test needs the database fixture
    it("should be readable") {
      withDatabase { futureDb =>
        futureDb map { db =>
          db.append("readable!")
          assert(db.toString == "ScalaTest is readable!")
        }
      }
    }

    // This test needs both the actor and the database
    it("should be clear and concise") {
      withDatabase { futureDb =>
        withActor { actor => // loan-fixture methods compose
          actor ! Append("concise!")
          val futureString = actor ? GetValue
          val futurePair: Future[(Db, String)] =
            futureDb zip futureString
          futurePair map { case (db, s) =>
            db.append("clear!")
            assert(db.toString == "ScalaTest is clear!")
            assert(s == "ScalaTest is concise!")
          }
        }
      }
    }
  }
}

