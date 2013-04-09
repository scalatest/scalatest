/*
 * Copyright 2001-2011 Artima, Inc.
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
package org.scalatest.concurrent

import org.scalatest._
import matchers.ShouldMatchers
import _root_.java.util.concurrent.{Callable, CountDownLatch}
import Thread.State._
import org.scalatest.exceptions.NotAllowedException

class ConductorMethodsSuite extends FunSuite with ConductorMethods with ShouldMatchers {

  // On Mac, got "BACDEFGHI" was not equal to "ABCDEFGHI"
  // And got: "ABDCEFGHI" was not equal to "ABCDEFGHI"
  // And "ABCFDEGHI" was not equal to "ABCDEFGHI"

  @volatile var aa = false
  @volatile var bb = false
  @volatile var cc = false
  @volatile var dd = false
  @volatile var ee = false
  @volatile var ff = false
  @volatile var gg = false
  @volatile var hh = false
  @volatile var ii = false

  // TODO: ignoring this test for now. Need to figure out why it occassionally failes, but
  // until then, may as well ignore it.
  ignore("metronome order") {

    thread("t1") {
      waitForBeat(1)
      aa should be (false)
      bb should be (false)
      cc should be (false)
      dd should be (false)
      ee should be (false)
      ff should be (false)
      gg should be (false)
      hh should be (false)
      ii should be (false)
      aa = true

      waitForBeat(3)
      aa should be (true)
      bb should be (true)
      cc should be (false)
      dd should be (false)
      ee should be (false)
      ff should be (false)
      gg should be (false)
      hh should be (false)
      ii should be (false)
      cc = true

      waitForBeat(6)
      aa should be (true)
      bb should be (true)
      cc should be (true)
      dd should be (true)
      ee should be (true)
      ff should be (false)
      gg should be (false)
      hh should be (false)
      ii should be (false)
      ff = true
    }

    thread("t2") {
      waitForBeat(2)
      aa should be (true) // failed here once
      bb should be (false)
      cc should be (false)
      dd should be (false)
      ee should be (false)
      ff should be (false)
      gg should be (false)
      hh should be (false)
      ii should be (false)
      bb = true

      waitForBeat(5)
      aa should be (true)
      bb should be (true)
      cc should be (true)
      dd should be (true)
      ee should be (false)
      ff should be (false)
      gg should be (false)
      hh should be (false)
      ii should be (false)
      ee = true

      waitForBeat(8)
      aa should be (true)
      bb should be (true)
      cc should be (true)
      dd should be (true)
      ee should be (true)
      ff should be (true)
      gg should be (true)
      hh should be (false)
      ii should be (false)
      hh = true
    }

    thread("t3") {
      waitForBeat(4)
      aa should be (true)
      bb should be (true)
      cc should be (true)
      dd should be (false)
      ee should be (false)
      ff should be (false)
      gg should be (false)
      hh should be (false)
      ii should be (false)
      dd = true

      waitForBeat(7)
      aa should be (true)
      bb should be (true)
      cc should be (true)
      dd should be (true)
      ee should be (true)
      ff should be (true)
      gg should be (false)
      hh should be (false)
      ii should be (false)
      gg = true

      waitForBeat(9)
      aa should be (true)
      bb should be (true)
      cc should be (true)
      dd should be (true)
      ee should be (true)
      ff should be (true)
      gg should be (true)
      hh should be (true)
      ii should be (false)
      ii = true
    }

    whenFinished {
      aa should be (true)
      bb should be (true)
      cc should be (true)
      dd should be (true)
      ee should be (true)
      ff should be (true)
      gg should be (true)
      hh should be (true)
      ii should be (true)
    }
  }

  test("wait for tick advances when threads are blocked") {
    var c = new CountDownLatch(3)

    thread {
      c.countDown()
      c.await()
    }

    thread {
      c.countDown()
      c.await()
    }

    thread {
      waitForBeat(1)
      c.getCount should be (1) // TODO: Got an error: 2 was not equal to 1
      waitForBeat(2) // advances quickly
      c.getCount should be (1)
      c.countDown()
    }

    whenFinished {
      c.getCount() should be (0)
    }
  }

  // TODO: t1.getState should (be(WAITING) or be(BLOCKED)) failed with:
  // RUNNABLE was not equal to WAITING, and RUNNABLE was not equal to BLOCKED
  // A thread in a wait set is allowed to show up as RUNNABLE. It could be spin waiting
  // or just wake up temporarily. So even though this fails very occasionally, it probably
  // doesn't indicate a bug. (It failed on the Azul server after I cleaned up the bugs
  // in Conductor.)
  // I got it again on the Mac. Same error message. Decided to go ahead and allow RUNNABLE
  // in the test, because it is actually possible. - bv 4/8/11
  test("wait for beat blocks thread") {

    val t1 = thread { waitForBeat(2) }

    thread {
      waitForBeat(1)
      t1.getState should (be (WAITING) or be (BLOCKED) or be (RUNNABLE))
    }
  }

  // On Mac, failed with RUNNABLE was not equal to TERMINATED
  // Same thing. Things can show up as RUNNABLE spuriously, so allow it in the test - bv 4/8/11
  test("thread terminates before finish called") {

    val t1 = thread {1 should be (1)}
    val t2 = thread {1 should be (1)}

    whenFinished {
      t1.getState should (be (TERMINATED) or be (RUNNABLE))
      t2.getState should (be (TERMINATED) or be (RUNNABLE))
    }
  }

  test("two thread calls return threads that both are in the same thread group") {

    val t1 = thread {waitForBeat(2)}
    val t2 = thread {waitForBeat(2)}

    thread {
      waitForBeat(1)
      t1.getThreadGroup should be (t2.getThreadGroup)
    }
  }

  test("if a thread call is nested inside another thread call, both threads are in the same thread group") {
    thread {
      val t2 = thread { waitForBeat(2) }
      waitForBeat(1)
      t2.getThreadGroup should be (Thread.currentThread.getThreadGroup) // Got: java.lang.ThreadGroup[name=Orchestra,maxpri=10] was not null
    }
  }

  test("whenFinished can only be called by thread that created Conductor.") {
    thread {
      intercept[NotAllowedException] {
        whenFinished {1 should be (1)}
      }.getMessage should be ("whenFinished can only be called by the thread that created Conductor.")
    }
    whenFinished {1 should be (1)}
  }

  test("nested thread calls result in a running thread that is allowed to execute immediately") (pending)

  /////////////////////////////////////////////////

  test("the test still executes (because " +
         "ConductorMethods calls conduct)") (pending)


  test("calling withConductorFrozen by two threads or twice will not screw things up. In other words, " +
          "whether or not the conductor is frozen should probably be a counting semaphor, not a flag") (pending)

  test("whenFinished throws an IllegalStateException if it is invoked during the running or" +
          "defunct phases,") (pending)

  test("conduct throws an IllegalStateException if it is invoked more than once.") (pending)
}
