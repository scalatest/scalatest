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
package org.scalatest.examples.asyncfunsuite.loanfixture

import java.util.concurrent.ConcurrentHashMap

object DbServer { // Simulating a database server
  type Db = StringBuffer
  private val databases = new ConcurrentHashMap[String, Db]
  def createDb(name: String): Db = {
    val db = new StringBuffer
    databases.put(name, db)
    db
  }
  def removeDb(name: String): Unit = {
    databases.remove(name)
  }
}

import org.scalatest._
import DbServer._
import java.util.UUID.randomUUID
import java.io._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class ExampleSuite extends AsyncFunSuite {

  implicit val executionContext = ExecutionContext.Implicits.global

  def withDatabase(testCode: Future[Db] => Future[Assertion]) = {
    val dbName = randomUUID.toString
    val futureDb = Future { createDb(dbName) } // create the fixture
    val futurePopulatedDb =
      futureDb map { db =>
        db.append("ScalaTest is ") // perform setup 
      }
    val futureAssertion = testCode(futurePopulatedDb) // "loan" the fixture to the test
    futureAssertion onComplete { _ => removeDb(dbName) } // clean up the fixture
    futureAssertion
  }

  def withFile(testCode: (File, FileWriter) => Future[Assertion]) = {
    val file = File.createTempFile("hello", "world") // create the fixture
    val writer = new FileWriter(file)
    try {
      writer.write("ScalaTest is ") // set up the fixture
      val futureAssertion = testCode(file, writer) // "loan" the fixture to the test
      futureAssertion onComplete { _ => writer.close() } // clean up the fixture
      futureAssertion
    }
    catch {
      case ex: Throwable =>
        writer.close() // clean up the fixture
        throw ex
    }
  }

  // This test needs the file fixture
  test("Testing should be productive") {
    withFile { (file, writer) =>
      writer.write("productive!")
      writer.flush()
      assert(file.length === 24)
    }
  }

  // This test needs the database fixture
  test("Test code should be readable") {
    withDatabase { futureDb =>
      futureDb map { db =>
        db.append("readable!")
        assert(db.toString === "ScalaTest is readable!")
      }
    }
  }

  // This test needs both the file and the database
  test("Test code should be clear and concise") {
    withDatabase { futureDb =>
      withFile { (file, writer) => // loan-fixture methods compose
        futureDb map { db =>
          db.append("clear!")
          writer.write("concise!")
          writer.flush()
          assert(db.toString === "ScalaTest is clear!")
          assert(file.length === 21)
        }
      }
    }
  }
}

