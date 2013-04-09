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
package org.scalatest.junit.helpers

import org.scalatest._
import org.scalatest.junit._

// This needs to be top level, not nested, because JUnit3 can't instantiate it
// to run each test in its own instance if it is nested (no no-arg construtor).
class TestWasCalledSuite extends JUnit3Suite {
  def testThis() { TestWasCalledSuite.theTestThisCalled = true }
  def testThat() { TestWasCalledSuite.theTestThatCalled = true }
}

object TestWasCalledSuite {

  def reinitialize() {
    theTestThisCalled = false
    theTestThatCalled = false
  }

  // Had to pull these out of the class, because JUnit makes a separate
  // instance for each test
  var theTestThisCalled = false
  var theTestThatCalled = false
}

class TestWithNonUnitMethod extends JUnit3Suite {
  def testThis() {}
  def testThat() {}
  // JUnit will not discover or run this, because its return type
  // is not Unit
  def testTheOtherThing(): String = "hi"
}

class TestWithMethodNamedTest extends JUnit3Suite {
  def testThis() {}
  def testThat() {}
  // JUnit will discover and run this method:
  def test() {}
}

class ASuite extends JUnit3Suite {
  def testThis() = ()
  def testThat(info: Informer) = ()
}

class BSuite extends JUnit3Suite {
  @Ignore
  def testThis() = ()
  def testThat(info: Informer) = ()
}

class CSuite extends JUnit3Suite {
  @FastAsLight
  def testThis() = ()
  def testThat(info: Informer) = ()
}

class DSuite extends JUnit3Suite {
  @FastAsLight
  @SlowAsMolasses
  def testThis() = ()
  @SlowAsMolasses
  def testThat(info: Informer) = ()
  def testTheOtherThing(info: Informer) = ()
  def testOne() = ()
  def testTwo() = ()
  def test() = () // JUnit will discover and run this method simply named "test"
  def testFour(): String = "hi" // JUnit will not run these two because they don't
  def testFive(): Int = 5       // have result type Unit.
}

class ESuite extends JUnit3Suite {
  @FastAsLight
  @SlowAsMolasses
  def testThis() = ()
  @SlowAsMolasses
  def testThat(info: Informer) = ()
  @Ignore
  def testTheOtherThing(info: Informer) = ()
}

class ShouldFailSuite extends JUnit3Suite {
  def testThrowsAssertionError() { throw new AssertionError }
  def testThrowsPlainOldError() { throw new Error }
  def testThrowsThrowable() { throw new Throwable }
}
