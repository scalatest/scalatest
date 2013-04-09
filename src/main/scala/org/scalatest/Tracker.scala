/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest

import org.scalatest.events.Ordinal

// Note: The reason Tracker is mutable is that methods would have to pass back, and that's hard because exceptions can
// also be thrown. So this mutable object is how methods invoked "returns" updates to the current ordinal whether those
// methods return normally or complete abruptly with an exception. Also, sometimes with closures capturing free variables,
// those free variables may want to grab an ordinal in the context of a callee even after the callee has already called
// some other method. So in other words the calling method may need to know the "current ordinal" even before the method
// it calls has completed in any manner, i.e., while it is running. (The example is the info stuff in FunSuite, which sets
// up an info that's useful during a run, then calls super.run(...).

/**
 * Class that tracks the progress of a series of <code>Ordinal</code>s produced by invoking
 * <code>next</code> and <code>nextNewOldPair</code> on the current <code>Ordinal</code>.
 *
 * <p>
 * Instances of this class are thread safe. Multiple threads can invoke <code>nextOrdinal</code>
 * and <code>nextTracker</code> concurrently. This facilitates multi-threaded tests that send
 * <code>infoProvided</code> reports concurrently. When using a <code>Dispatcher</code> to execute
 * suites in parallel, the intention is that each <code>Tracker</code> will only be used by one
 * thread. For example, if the optional <code>Dispatcher</code>  passed to <code>Suite</code>'s implementation
 * of <code>runNestedSuites</code> is defined, that method will obtain a new <code>Tracker</code> by invoking
 * <code>nextTracker</code> for each nested suite it passes to the <code>Dispatcher</code>.
 * </p>
 *
 * @param firstOrdinal the first <code>Ordinal</code> in the series of <code>Ordinal</code>s
 *        tracked by this <code>Tracker</code>, which will be used to initialize this <code>Tracker</code>'s
 *        current <code>Ordinal</code>.
 *
 * @author Bill Venners
 */
final class Tracker(firstOrdinal: Ordinal = new Ordinal(0)) {

  private var currentOrdinal = firstOrdinal

  /**
   * Returns the next <code>Ordinal</code> in the series tracked by this <code>Tracker</code>.
   *
   * <p>
   * This method saves the current <code>Ordinal</code> in a local variable, reassigns the current <code>Ordinal</code>
   * with the value returned by invoking <code>nextOrdinal</code> on the saved <code>Ordinal</code>, then
   * returns the saved <code>Ordinal</code>. As a result, if this method is invoked immediately after construction,
   * this method will return the <code>Ordinal</code> passed as <code>firstOrdinal</code>.
   * </p>
   *
   * @return the next <code>Ordinal</code> in the series
   */
  def nextOrdinal(): Ordinal = {
    synchronized {
      val ordinalToReturn = currentOrdinal
      currentOrdinal = currentOrdinal.next
      ordinalToReturn
    }
  }

  /**
   * Returns a <code>Tracker</code> initialized with the first element in the tuple returned by invoking
   * <code>nextNewOldPair</code> on the current <code>Ordinal</code>, and reassigns the current <code>Ordinal</code>
   * with the second element that was returned by the <code>nextNewOldPair</code> invocation.
   *
   * <p>
   * The <code>Ordinal</code> series of the returned <code>Tracker</code> will be placed after all the
   * <code>Ordinal</code>s previously returned by invoking <code>nextOrdinal</code> on this <code>Tracker</code> and
   * before all the <code>Ordinal</code>s subsequently returned by invoking <code>nextOrdinal</code> on
   * this <code>Tracker</code> in the future. This method is intended to be used when executing nested suites
   * in parallel. Each nested suite passed to the <code>Distributor</code> will get its own <code>Tracker</code>
   * obtained by invoking <code>nextTracker</code> on the current thread's <code>Tracker</code>.
   * </p>
   *
   * @return the next <code>Tracker</code> in this series
   */
  def nextTracker(): Tracker = {
    synchronized {
      val (nextForNewThread, nextForThisThread) = currentOrdinal.nextNewOldPair
      currentOrdinal = nextForThisThread
      new Tracker(nextForNewThread)
    }
  }
}

object Tracker {
  private val defaultTracker = new Tracker()
  def default: Tracker = defaultTracker
}
