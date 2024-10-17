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
package org.scalatest.examples.asyncfeaturespec

import org.scalatest._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

// Defining actor messages
case object IsOn
case object PressPowerButton

class TVSetActor { // Simulating an actor
  private var on: Boolean = false
  def !(msg: PressPowerButton.type): Unit =
    synchronized {
      on = !on
    }
  def ?(msg: IsOn.type)(implicit c: ExecutionContext): Future[Boolean] =
    Future {
      synchronized { on }
    }
}

class TVSetActorSpec extends AsyncFeatureSpec with GivenWhenThen {

  implicit override def executionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  info("As a TV set owner")
  info("I want to be able to turn the TV on and off")
  info("So I can watch TV when I want")
  info("And save energy when I'm not watching TV")

  feature("TV power button") {
    scenario("User presses power button when TV is off") {

      Given("a TV set that is switched off")
      val tvSetActor = new TVSetActor

      When("the power button is pressed")
      tvSetActor ! PressPowerButton

      Then("the TV should switch on")
      val futureBoolean = tvSetActor ? IsOn
      futureBoolean map { isOn => assert(isOn) }
    }

    scenario("User presses power button when TV is on") {

      Given("a TV set that is switched on")
      val tvSetActor = new TVSetActor
      tvSetActor ! PressPowerButton

      When("the power button is pressed")
      tvSetActor ! PressPowerButton

      Then("the TV should switch off")
      val futureBoolean = tvSetActor ? IsOn
      futureBoolean map { isOn => assert(!isOn) }
    }
  }
}

/*
class TVSpec extends AsyncFeatureSpec {

  def addSoon(addends: Int*): Future[Int] = Future { addends.sum }
  def addNow(addends: Int*): Int = addends.sum

  feature("The add methods") {

    scenario("addSoon will eventually compute a sum of passed Ints") {
      val futureSum: Future[Int] = addSoon(1, 2)
      // You can map assertions onto a Future, then return
      // the resulting Future[Assertion] to ScalaTest:
      futureSum map { sum => assert(sum == 3) }
    }

    scenario("addNow will immediately compute a sum of passed Ints") {
      val sum: Int = addNow(1, 2)
      // You can also write synchronous tests. The body
      // must have result type Assertion:
      assert(sum == 3)
    }
  }
}

*/
