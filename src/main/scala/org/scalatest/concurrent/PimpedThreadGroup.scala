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
package org.scalatest.concurrent

import org.scalatest._
import Thread.State
import Thread.State._

/**
 * Object containing implicit def that converts a ThreadGroup into a PimpedThreadGroup.
 * Also contains a convienience method to get all the threads in the current ThreadGroup.
 *
 * @author Josh Cough
 */
private[scalatest] object PimpedThreadGroup {

  implicit def ThreadGroupToList(tg:ThreadGroup): List[Thread] = {
    new PimpedThreadGroup(tg).getThreads
  }

  /**
   * Converts a ThreadGroup into a PimpedThreadGroup
   */
  implicit def threadGroupToPimpedThreadGroup(tg: ThreadGroup) = new PimpedThreadGroup(tg)

  /**
   * Get all the Threads in the current ThreadGroup
   */
  def getThreads = Thread.currentThread.getThreadGroup.getThreads
}

/**
 * Adds several nice methods to ThreadGroup.
 */
private[concurrent] class PimpedThreadGroup(threadGroup: ThreadGroup) {

  /**
   * Return all the Threads in this ThreadGroup, recursively scanning child ThreadGroups.
   */
  def getThreads: List[Thread] = getThreads(true)

  /**
   * Return all the Threads in this ThreadGroup. If the parameter recursive is true,
   * then scan all child ThreadGroups as well. If false, just find Threads who's parent is
   * this ThreadGroup.
   *
   * This method gets around Java's mysterious Thread.enumerate call which may or may
   * not silently ignore threads if its given an array that is too small. It does so by
   * recalling enumerate with a larger array until this array is not full.
   *
   * @returns A list of Threads in this ThreadGroup (and possibly child ThreadGroups)
   *          The list will never contain null values, only Threads. 
   */
  def getThreads(recursive: Boolean): List[Thread] = {
    def getThreads(sizeEstimate: Int): Seq[Thread] = {
      val ths = new Array[Thread](sizeEstimate)
      if (threadGroup.enumerate(ths, recursive) == sizeEstimate) getThreads(sizeEstimate + 10)
      else for (t <- ths; if (t != null)) yield t
    }
    getThreads(threadGroup.activeCount() + 10).toList
  }

  /**
   * Returns a list of all the Threads in this ThreadGroup that are in the given state.
   */
  def filter(state: State): List[Thread] = getThreads.filter(_.getState == state)

  /**
   * Returns true if this ThreadGroup contains any Thread whose state matches the given State,
   * false otherwise.
   */
  def exists(state: State): Boolean = getThreads.exists(_.getState == state)

  /**
   * Returns true if any Threads in this ThreadGroup are in a State
   * other than NEW or TERMINATED, false otherwise.
   */
  def areAnyThreadsAlive = getThreads.exists(t => t.getState != NEW && t.getState != TERMINATED)

  /**
   * Returns true if any Threads in this ThreadGroup are in a State RUNNING,
   * false otherwise.
   */
  def areAnyThreadsRunning = getThreads.exists(_.getState == RUNNABLE)

  /**
   * Returns true if any Threads in this ThreadGroup are in a State TIMED_WAITING,
   * false otherwise.
   */
  def areAnyThreadsInTimedWaiting = getThreads.exists(_.getState == TIMED_WAITING)
}
