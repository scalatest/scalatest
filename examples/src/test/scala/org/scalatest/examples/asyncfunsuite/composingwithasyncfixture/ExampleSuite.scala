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

// Defining actor messages
sealed abstract class StringOp
case object Clear extends StringOp
case class Append(value: String) extends StringOp
case object GetValue

class StringBuilderActor { // Simulating an actor
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

class StringBufferActor {
  private final val buf = ListBuffer.empty[String]
  def !(op: StringOp): Unit =
    synchronized {
      op match {
        case Append(value) => buf += value
        case Clear => buf.clear()
      }
    }
  def ?(get: GetValue.type)(implicit c: ExecutionContext): Future[List[String]] =
    Future {
      synchronized { buf.toList }
    }
}

trait Builder extends AsyncSuiteMixin { this: AsyncSuite =>

  final val builderActor = new StringBuilderActor

  abstract override def withAsyncFixture(test: NoArgAsyncTest) = {
    builderActor ! Append("ScalaTest is ")
    withCleanup {
      super.withAsyncFixture(test) // To be stackable, must call super.withAsyncFixture
    } {
      builderActor ! Clear
    }
  }
}

trait Buffer extends AsyncSuiteMixin { this: AsyncSuite =>

  final val bufferActor = new StringBufferActor

  abstract override def withAsyncFixture(test: NoArgAsyncTest) = {
    withCleanup {
      super.withAsyncFixture(test) // To be stackable, must call super.withAsyncFixture
    } {
      bufferActor ! Clear
    }
  }
}

class ExampleSuite extends AsyncFunSuite with Builder with Buffer {

  implicit val executionContext = ExecutionContext.Implicits.global

  test("Testing should be easy") {
    builderActor ! Append("easy!")
    val futureString = builderActor ? GetValue
    val futureList = bufferActor ? GetValue
    val futurePair: Future[(String, List[String])] = futureString zip futureList
    futurePair map { case (str, lst) =>
      assert(str === "ScalaTest is easy!")
      assert(lst.isEmpty)
      bufferActor ! Append("sweet")
      succeed
    }
  }

  test("Testing should be fun") {
    builderActor ! Append("fun!")
    val futureString = builderActor ? GetValue
    val futureList = bufferActor ? GetValue
    val futurePair: Future[(String, List[String])] = futureString zip futureList
    futurePair map { case (str, lst) =>
      assert(str === "ScalaTest is fun!")
      assert(lst.isEmpty)
      bufferActor ! Append("sweet")
      succeed
    }
  }
}
