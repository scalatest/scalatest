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
package org.scalatestplus.junit.helpers

import org.scalatest._
import org.scalatestplus.junit._

// This needs to be top level, not nested, because JUnit3 can't instantiate it
// to run each test in its own instance if it is nested (no no-arg construtor).
class TestWasCalledSuite extends JUnit3Suite {
  def testThis(): Unit = { TestWasCalledSuite.theTestThisCalled = true }
  def testThat(): Unit = { TestWasCalledSuite.theTestThatCalled = true }
}

object TestWasCalledSuite {

  def reinitialize(): Unit = {
    theTestThisCalled = false
    theTestThatCalled = false
  }

  // Had to pull these out of the class, because JUnit makes a separate
  // instance for each test
  var theTestThisCalled = false
  var theTestThatCalled = false
}

class TestWithNonUnitMethod extends JUnit3Suite {
  def testThis(): Unit = {}
  def testThat(): Unit = {}
  // JUnit will not discover or run this, because its return type
  // is not Unit
  def testTheOtherThing(): String = "hi"
}

class TestWithMethodNamedTest extends JUnit3Suite {
  def testThis(): Unit = {}
  def testThat(): Unit = {}
  // JUnit will discover and run this method:
  def test(): Unit = {}
}

class ASuite extends JUnit3Suite {
  def testThis(): Unit = ()
  def testThat(info: Informer): Unit = ()
}

class BSuite extends JUnit3Suite {
  @Ignore
  def testThis(): Unit = ()
  def testThat(info: Informer): Unit = ()
}

class CSuite extends JUnit3Suite {
  @FastAsLight
  def testThis(): Unit = ()
  def testThat(info: Informer): Unit = ()
}

class DSuite extends JUnit3Suite {
  @FastAsLight
  @SlowAsMolasses
  def testThis(): Unit = ()
  @SlowAsMolasses
  def testThat(info: Informer): Unit = ()
  def testTheOtherThing(info: Informer): Unit = ()
  def testOne(): Unit = ()
  def testTwo(): Unit = ()
  def test(): Unit = () // JUnit will discover and run this method simply named "test"
  def testFour(): String = "hi" // JUnit will not run these two because they don't
  def testFive(): Int = 5       // have result type Unit.
}

class ESuite extends JUnit3Suite {
  @FastAsLight
  @SlowAsMolasses
  def testThis(): Unit = ()
  @SlowAsMolasses
  def testThat(info: Informer): Unit = ()
  @Ignore
  def testTheOtherThing(info: Informer): Unit = ()
}

class ShouldFailSuite extends JUnit3Suite {
  def testThrowsAssertionError(): Unit = { throw new AssertionError }
  def testThrowsPlainOldError(): Unit = { throw new Error }
  def testThrowsThrowable(): Unit = { throw new Throwable }
}
