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
package org.scalatest.examples.asyncfreespec.beforeandafter

import org.scalatest.AsyncFreeSpec
import org.scalatest.BeforeAndAfter
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

class ExampleSpec extends AsyncFreeSpec with BeforeAndAfter {

  final val actor = new StringActor

  before {
    actor ! Append("ScalaTest is ") // set up the fixture
  }

  after {
    actor ! Clear // clean up the fixture
  }

  "Testing" - {
    "should be easy" in {
      actor ! Append("easy!")
      val futureString = actor ? GetValue
      futureString map { s =>
        assert(s == "ScalaTest is easy!")
      }
    }

    "should be fun" in {
      actor ! Append("fun!")
      val futureString = actor ? GetValue
      futureString map { s =>
        assert(s == "ScalaTest is fun!")
      }
    }
  }
}

