/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.mock

import org.scalatest._
import org.scalatest.fixture
import matchers.ShouldMatchers
import org.jmock.Expectations.{equal => thatEquals}

class JMockCycleSpec extends FlatSpec with ShouldMatchers with SharedHelpers {

  "The JMockCycle trait" should "work with multiple mocks" in {

    val a = new fixture.Suite with JMockCycleFixture {
      def testThatShouldFail(cycle: JMockCycle) {
        import cycle._
        trait OneFish {
          def eat(food: String) = ()
        }
        trait TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = mock[OneFish]
        val twoFishMock = mock[TwoFish]

        expecting { e => import e.{oneOf => OneOf}
          OneOf (oneFishMock).eat("red fish")
          OneOf (twoFishMock).eat("blue fish")
        }

        whenExecuting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("green fish")
        }
      }

      def testThatShouldSucceed(cycle: JMockCycle) {
        import cycle._
        trait OneFish {
          def eat(food: String) = ()
        }
        trait TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = mock[OneFish]
        val twoFishMock = mock[TwoFish]

        expecting { e => import e.{oneOf => OneOf}
          OneOf (oneFishMock).eat("red fish")
          OneOf (twoFishMock).eat("blue fish")
        }

        whenExecuting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }
      }

      def testThatShouldSucceedWithClass(cycle: JMockCycle) {
        import cycle._
        class OneFish {
          def eat(food: String) = ()
        }
        class TwoFish {
          def eat(food: String) = ()
        }
        val oneFishMock = mock[OneFish]
        val twoFishMock = mock[TwoFish]

        expecting { e => import e.{oneOf => OneOf}
          OneOf (oneFishMock).eat("red fish")
          OneOf (twoFishMock).eat("blue fish")
        }

        whenExecuting {
          oneFishMock.eat("red fish")
          twoFishMock.eat("blue fish")
        }
      }
    }
    val rep = new EventRecordingReporter
    a.run(None, Args(rep))
    val tf = rep.testFailedEventsReceived
    tf.size should be === 1
    val ts = rep.testSucceededEventsReceived
    ts.size should be === 2
  }

  it should "provide sugar for invoking with methods that take matchers" in {
    val a = new fixture.Suite with JMockCycleFixture {
      def testThatShouldSucceed(cycle: JMockCycle) {
        import cycle._
        trait OneFish {
          def doString(food: String) = ()
          def doInt(food: Int) = ()
          def doShort(food: Short) = ()
          def doByte(food: Byte) = ()
          def doLong(food: Long) = ()
          def doBoolean(food: Boolean) = ()
          def doFloat(food: Float) = ()
          def doDouble(food: Double) = ()
          def doChar(food: Char) = ()
        }
        val oneFishMock = mock[OneFish]

        expecting { e => import e.{oneOf => OneOf, withArg}
          OneOf (oneFishMock).doString(withArg(thatEquals("red fish")))
          OneOf (oneFishMock).doInt(withArg(thatEquals(5)))
          OneOf (oneFishMock).doShort(withArg(thatEquals(5.asInstanceOf[Short])))
          OneOf (oneFishMock).doByte(withArg(thatEquals(5.asInstanceOf[Byte])))
          OneOf (oneFishMock).doLong(withArg(thatEquals(5L)))
          OneOf (oneFishMock).doBoolean(withArg(thatEquals(true)))
          OneOf (oneFishMock).doFloat(withArg(thatEquals(5.0f)))
          OneOf (oneFishMock).doDouble(withArg(thatEquals(5.0d)))
          OneOf (oneFishMock).doChar(withArg(thatEquals('5')))
        }

        whenExecuting {
          oneFishMock.doString("red fish")
          oneFishMock.doInt(5)
          oneFishMock.doShort(5)
          oneFishMock.doByte(5)
          oneFishMock.doLong(5L)
          oneFishMock.doBoolean(true)
          oneFishMock.doFloat(5.0f)
          oneFishMock.doDouble(5.0d)
          oneFishMock.doChar('5')
        }
      }
    }
    val rep = new EventRecordingReporter
    a.run(None, Args(rep))
    val ts = rep.testSucceededEventsReceived
    ts.size should be === 1
  }

  it should "provide sugar for invoking with methods that take non-matcher values" in {
    val a = new fixture.Suite with JMockCycleFixture {
      def testThatShouldSucceed(cycle: JMockCycle) {
        import cycle._
        trait OneFish {
          def doString(food: String) = ()
          def doInt(food: Int) = ()
          def doShort(food: Short) = ()
          def doByte(food: Byte) = ()
          def doLong(food: Long) = ()
          def doBoolean(food: Boolean) = ()
          def doFloat(food: Float) = ()
          def doDouble(food: Double) = ()
          def doChar(food: Char) = ()
        }
        val oneFishMock = mock[OneFish]

        expecting { e => import e.{oneOf => OneOf, withArg}
          OneOf (oneFishMock).doString(withArg("red fish"))
          OneOf (oneFishMock).doInt(withArg(5))
          OneOf (oneFishMock).doShort(withArg(5.asInstanceOf[Short]))
          OneOf (oneFishMock).doByte(withArg(5.asInstanceOf[Byte]))
          OneOf (oneFishMock).doLong(withArg(5L))
          OneOf (oneFishMock).doBoolean(withArg(true))
          OneOf (oneFishMock).doFloat(withArg(5.0f))
          OneOf (oneFishMock).doDouble(withArg(5.0d))
          OneOf (oneFishMock).doChar(withArg('5'))
        }

        whenExecuting {
          oneFishMock.doString("red fish")
          oneFishMock.doInt(5)
          oneFishMock.doShort(5)
          oneFishMock.doByte(5)
          oneFishMock.doLong(5L)
          oneFishMock.doBoolean(true)
          oneFishMock.doFloat(5.0f)
          oneFishMock.doDouble(5.0d)
          oneFishMock.doChar('5')
        }
      }
    }
    val rep = new EventRecordingReporter
    a.run(None, Args(rep))
    val ts = rep.testSucceededEventsReceived
    ts.size should be === 1
  }
}
