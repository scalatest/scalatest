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
import java.io._

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

class ThreadSafeFileWriter(file: File) {
  private final val fw = new FileWriter(file)
  def write(s: String): Unit = synchronized { fw.write(s) }
  def close(): Unit = synchronized { fw.close() }
  def flush(): Unit = synchronized { fw.flush() }
}

import org.scalatest._
import DbServer._
import java.util.UUID.randomUUID
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class ExampleSuite extends AsyncFunSuite {

  implicit val executionContext = ExecutionContext.Implicits.global

  def withDatabase(testCode: Future[Db] => Future[Assertion]) = {
    val dbName = randomUUID.toString
    val futureDb = Future { createDb(dbName) } // create the fixture
    withCleanup {
      val futurePopulatedDb =
        futureDb map { db =>
          db.append("ScalaTest is ") // perform setup 
        }
      testCode(futurePopulatedDb) // "loan" the fixture to the test code
    } {
      removeDb(dbName) // ensure the fixture will be cleaned up
    }
  }

  def withFile(testCode: (File, ThreadSafeFileWriter) => Future[Assertion]) = {
    val file = File.createTempFile("hello", "world") // create the fixture
    val writer = new ThreadSafeFileWriter(file)
    withCleanup {
      writer.write("ScalaTest is ") // set up the fixture
      testCode(file, writer) // "loan" the fixture to the test code
    } {
      writer.close() // ensure the fixture will be cleaned up
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

