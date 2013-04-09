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

/**
 * Trait whose instances can accept a stop request and indicate whether a stop has already been requested.
 *
 * <p>
 * This is passed in
 * to the <code>run</code> method of <code>Suite</code>, so that running suites of tests can be
 * requested to stop early.
 * </p>
 *
 * @author Bill Venners
 */
trait Stopper {

  /**
   * <strong>This method has been deprecated and will be removed in a future version of ScalaTest. Please call
   * the stopRequested method instead.</strong>
   */
  @deprecated 
  def apply() = stopRequested

  /**
   * Indicates whether a stop has been requested.
   *
   * <p>
   * Call this method
   * to determine whether a running test should stop. The <code>run</code> method of any <code>Suite</code>, or
   * code invoked by <code>run</code>, should periodically check the
   * stop requested function. If <code>true</code>,
   * the <code>run</code> method should interrupt its work and simply return.
   * </p>
   *
   * @return true if a stop has been requested
   */
  def stopRequested: Boolean

  /**
   * Request that the current run stop.
   *
   * <p>
   * Invoking this method is like pulling the stop-request chord in a streetcar. It requests a stop, but in no
   * way forces a stop. The running suite of tests decides when and how (and if) to respond to a stop request.
   * ScalaTest's style traits periodically check the <code>stopRequested</code> method of the passed <code>Stopper</code>,
   * and if a stop has been requested, terminates gracefully.
   * </p>
   */
  def requestStop()
}

/**
 * Companion object to Stopper that holds a factory method that produces a new <code>Stopper</code> whose
 * <code>stopRequested</code> method returns false until after its <code>requestStop</code> has been
 * invoked.
 */
object Stopper {

  private class DefaultStopper extends Stopper {
    @volatile private var stopWasRequested = false
    def stopRequested: Boolean = stopWasRequested
    def requestStop() {
      stopWasRequested = true
    }
  }

  /**
   * Factory method that produces a new <code>Stopper</code> whose
   * <code>stopRequested</code> method returns false until after its <code>requestStop</code> has been
   * invoked.
   *
   * <p>
   * The <code>Stopper</code> returned by this method can be safely used by multiple threads concurrently.
   * </p>
   *
   * @return a new default stopper
   */
  def default: Stopper = new DefaultStopper
}
