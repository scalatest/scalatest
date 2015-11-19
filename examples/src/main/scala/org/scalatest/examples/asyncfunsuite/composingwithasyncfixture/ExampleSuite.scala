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
package org.scalatest.examples.asyncfunsuite.composingwithfixture

import org.scalatest._
import org.scalatest.SuiteMixin
import collection.mutable.ListBuffer
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

class ThreadSafeListBufferOfString {
  private final val buf = ListBuffer.empty[String]
  def += (s: String): Unit = synchronized { buf += s }
  def toList: List[String] = synchronized { buf.toList }
  def clear(): Unit = synchronized { buf.clear() }
  def isEmpty: Boolean = synchronized { buf.isEmpty }
}

class ThreadSafeStringBuilder {
  private final val bldr = new StringBuilder
  def append(s: String): Unit =
    synchronized {
      bldr.append(s)
    }
  def clear(): Unit = synchronized { bldr.clear() }
  override def toString = synchronized { bldr.toString }
}

trait Builder extends AsyncSuiteMixin { this: AsyncSuite =>

  final val builder = new ThreadSafeStringBuilder

  abstract override def withAsyncFixture(test: NoArgAsyncTest) = {
    builder.append("ScalaTest is ")
    withCleanup {
      super.withAsyncFixture(test) // To be stackable, must call super.withAsyncFixture
    } {
      builder.clear()
    }
  }
}

trait Buffer extends AsyncSuiteMixin { this: AsyncSuite =>

  final val buffer = new ThreadSafeListBufferOfString

  abstract override def withAsyncFixture(test: NoArgAsyncTest) = {
    withCleanup {
      super.withAsyncFixture(test) // To be stackable, must call super.withAsyncFixture
    } {
      buffer.clear()
    }
  }
}

class ExampleSuite extends AsyncFunSuite with Builder with Buffer {

  implicit val executionContext = ExecutionContext.Implicits.global

  test("Testing should be easy") {
    Future {
      builder.append("easy!")
      assert(builder.toString === "ScalaTest is easy!")
      assert(buffer.isEmpty)
      buffer += "sweet"
      succeed
    }
  }

  test("Testing should be fun") {
    Future {
      builder.append("fun!")
      assert(builder.toString === "ScalaTest is fun!")
      assert(buffer.isEmpty)
      buffer += "clear"
      succeed
    }
  }
}
