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
package org.scalatest.junit.junit4helpers

import org.scalatest._
import org.scalatest.junit.JUnitSuite
import org.junit.Test
import org.junit.Ignore

// This needs to be top level, not nested, because JUnit3 can't instantiate it
// to run each test in its own instance if it is nested (no no-arg construtor).
class TestWasCalledSuite extends JUnitSuite {
  @Test def doThis() { TestWasCalledSuite.theDoThisCalled = true }
  @Test def doThat() { TestWasCalledSuite.theDoThatCalled = true }
}

object TestWasCalledSuite {

  def reinitialize() {
    theDoThisCalled = false
    theDoThatCalled = false
  }

  // Had to pull these out of the class, because JUnit makes a separate
  // instance for each test
  var theDoThisCalled = false
  var theDoThatCalled = false
}

class TestWithNonUnitMethod extends JUnitSuite {
  @Test def doThis() {}
  @Test def doThat() {}
  // JUnit will not discover or run this, because its return type
  // is not Unit
  @Test def doTheOtherThing(): String = "hi"
}

class TestWithMethodNamedTest extends JUnitSuite {
  @Test def doThis() {}
  @Test def doThat() {}
  // JUnit will discover and run this method:
  @Test def doIt() {}
}

class ASuite extends JUnitSuite {
  @Test def doThis() = ()
  @Test def doThat(info: Informer) = ()
}

class BSuite extends JUnitSuite {
  @Ignore
  @Test def doThis() = ()
  @Test def doThat(info: Informer) = ()
}

class CSuite extends JUnitSuite {
  @FastAsLight
  @Test def doThis() = ()
  @Test def doThat(info: Informer) = ()
}

class DSuite extends JUnitSuite {
  @FastAsLight
  @SlowAsMolasses
  @Test def doThis() = ()
  @SlowAsMolasses
  @Test def doThat(info: Informer) = ()
  @Test def doTheOtherThing(info: Informer) = ()
  @Test def doOne() = ()
  @Test def doTwo() = ()
  @Test def doIt() = ()
  @Test def doFour(): String = "hi" // JUnit will not run these two because they don't
  @Test def doFive(): Int = 5       // have result type Unit.
}

class ESuite extends JUnitSuite {
  @FastAsLight
  @SlowAsMolasses
  @Test def doThis() = ()
  @SlowAsMolasses
  @Test def doThat(info: Informer) = ()
  @Ignore
  @Test def doTheOtherThing(info: Informer) = ()
}

class ShouldFailSuite extends JUnitSuite {
  @Test def doThrowsAssertionError() { throw new AssertionError }
  @Test def doThrowsPlainOldError() { throw new Error }
  @Test def doThrowsThrowable() { throw new Throwable }
}
