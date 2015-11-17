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
package org.scalatest.examples.asyncfunsuite.getfixture

import org.scalatest.AsyncFunSuite
import collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class ExampleSuite extends AsyncFunSuite {

  implicit val executionContext = ExecutionContext.Implicits.global

  class Fixture {
    val builder = new StringBuilder("ScalaTest is ")
    val buffer = new ListBuffer[String]
  }

  def fixture = new Fixture

  test("Testing should be easy") {
    val f = fixture
    f.builder.append("easy!")
    val fut = Future { (f.builder.toString, f.buffer.toList) }
    fut map { case (s, xs) =>
      assert(s === "ScalaTest is easy!")
      assert(xs.isEmpty)
      f.buffer += "sweet"
      succeed
    }
  }

  test("Testing should be fun") {
    val f = fixture
    f.builder.append("fun!")
    val fut = Future { (f.builder.toString, f.buffer.toList) }
    fut map { case (s, xs) =>
      assert(s === "ScalaTest is fun!")
      assert(xs.isEmpty)
    }
  }
}

