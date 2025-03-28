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
package org.scalatest.examples.asyncwordspec.oneargasynctest

import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

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

class ExampleSpec extends fixture.AsyncWordSpec {

  type FixtureParam = StringActor

  def withFixture(test: OneArgAsyncTest): FutureOutcome = {

    val actor = new StringActor
    complete {
      actor ! Append("ScalaTest is ") // set up the fixture
      withFixture(test.toNoArgAsyncTest(actor))
    } lastly {
      actor ! Clear // ensure the fixture will be cleaned up
    }
  }

  "Testing" should {
    "be easy" in { actor =>
      actor ! Append("easy!")
      val futureString = actor ? GetValue
      futureString map { s =>
        assert(s == "ScalaTest is easy!")
      }
    }

    "be fun" in { actor =>
      actor ! Append("fun!")
      val futureString = actor ? GetValue
      futureString map { s =>
        assert(s == "ScalaTest is fun!")
      }
    }
  }
}

