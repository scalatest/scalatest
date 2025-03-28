/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.examples.asyncfreespec.composingbeforeandaftereach

import org.scalatest._
import org.scalatest.BeforeAndAfterEach
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

trait Builder extends BeforeAndAfterEach { this: Suite =>

  final val builderActor = new StringBuilderActor

  override def beforeEach() {
    builderActor ! Append("ScalaTest is ")
    super.beforeEach() // To be stackable, must call super.beforeEach
  }

  override def afterEach() {
    try super.afterEach() // To be stackable, must call super.afterEach
    finally builderActor ! Clear
  }
}

trait Buffer extends BeforeAndAfterEach { this: Suite =>

  final val bufferActor = new StringBufferActor

  override def afterEach() {
    try super.afterEach() // To be stackable, must call super.afterEach
    finally bufferActor ! Clear
  }
}

class ExampleSpec extends AsyncFreeSpec with Builder with Buffer {

  "Testing" - {

    "should be easy" in {
      builderActor ! Append("easy!")
      val futureString = builderActor ? GetValue
      val futureList = bufferActor ? GetValue
      val futurePair: Future[(String, List[String])] = futureString zip futureList
      futurePair map { case (str, lst) =>
        assert(str == "ScalaTest is easy!")
        assert(lst.isEmpty)
        bufferActor ! Append("sweet")
        succeed
      }
    }

    "should be fun" in {
      builderActor ! Append("fun!")
      val futureString = builderActor ? GetValue
      val futureList = bufferActor ? GetValue
      val futurePair: Future[(String, List[String])] = futureString zip futureList
      futurePair map { case (str, lst) =>
        assert(str == "ScalaTest is fun!")
        assert(lst.isEmpty)
        bufferActor ! Append("awesome")
        succeed
      }
    }
  }
}
