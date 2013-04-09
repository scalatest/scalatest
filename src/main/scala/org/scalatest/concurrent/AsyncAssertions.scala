/*
 * Copyright 2001-2012 Artima, Inc.
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
import Assertions.fail
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.TestFailedException
import time.{Nanoseconds, Second, Span}

/**
 * Trait that facilitates performing assertions outside the main test thread, such as assertions in callback methods
 * that are invoked asynchronously.
 *
 * <p>
 * Trait <code>AsyncAssertions</code> provides a <code>Waiter</code> class that you can use to orchestrate the inter-thread
 * communication required to perform assertions outside the main test thread, and a means to configure it.
 * </p>
 *
 * <p>
 * To use <code>Waiter</code>, create an instance of it in the main test thread:
 * </p>
 *
 * <pre class="stHighlight">
 * val w = new Waiter // Do this in the main test thread
 * </pre>
 *
 * <p>
 * At some point later, call <code>await</code> on the waiter:
 * </p>
 *
 * <pre class="stHighlight">
 * w.await() // Call await() from the main test thread
 * </pre>
 *
 * <p>
 * The <code>await</code> call will block until it either receives a report of a failed assertion from a different thread, at which
 * point it will complete abruptly with the same exception, or until it is <em>dismissed</em> by a different thread (or threads), at
 * which point it will return normally. You can optionally specify a timeout and/or a number
 * of dismissals to wait for. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.time.SpanSugar._
 *
 * w.await(timeout(300 millis), dismissals(2))
 * </pre>
 *
 * <p>
 * The default value for <code>timeout</code>, provided via an implicit <code>PatienceConfig</code> parameter, is 150 milliseconds. The default value for
 * <code>dismissals</code> is 1. The <code>await</code> method will block until either it is dismissed a sufficient number of times by other threads or
 * an assertion fails in another thread. Thus if you just want to perform assertions in just one other thread, only that thread will be
 * performing a dismissal, so you can use the default value of 1 for <code>dismissals</code>.
 * </p>
 *
 * <p>
 * <code>Waiter</code> contains four overloaded forms of <code>await</code>, two of which take an implicit
 * <code>PatienceConfig</code> parameter. To change the default timeout configuration, override or hide
 * (if you imported the members of <code>AsyncAssertions</code> companion object instead of mixing in the
 * trait) <code>patienceConfig</code> with a new one that returns your desired configuration.
 * </p>
 *
 * <p>
 * To dismiss a waiter, you just invoke <code>dismiss</code> on it:
 * </p>
 *
 * <pre class="stHighlight">
 * w.dismiss() // Call this from one or more other threads
 * </pre>
 *
 * <p>
 * You may want to put <code>dismiss</code> invocations in a finally clause to ensure they happen even if an exception is thrown.
 * Otherwise if a dismissal is missed because of a thrown exception, an <code>await</code> call will wait until it times out.
 * </p>
 *
 * <p>
 * Finally, to perform an assertion in a different thread, you just apply the <code>Waiter</code> to the assertion code. Here are
 * some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * w { assert(1 + 1 === 3) }    // Can use assertions
 * w { 1 + 1 should equal (3) } // Or matchers
 * w { "hi".charAt(-1) }        // Any exceptions will be forwarded to await
 * </pre>
 *
 * <p>
 * Here's a complete example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import concurrent.AsyncAssertions
 * import matchers.ShouldMatchers
 * import scala.actors.Actor
 *
 * class ExampleSuite extends FunSuite with ShouldMatchers with AsyncAssertions {
 *
 *   case class Message(text: String)
 *
 *   class Publisher extends Actor {
 *
 *     @volatile private var handle: Message => Unit = { (msg) => }
 *
 *     def registerHandler(f: Message => Unit) {
 *       handle = f
 *     }
 *
 *     def act() {
 *       var done = false
 *       while (!done) {
 *         react {
 *           case msg: Message => handle(msg)
 *           case "Exit" => done = true
 *         }
 *       }
 *     }
 *   }
 *
 *   test("example one") {
 *
 *     val publisher = new Publisher
 *     val message = new Message("hi")
 *     val w = new Waiter
 *
 *     publisher.start()
 *
 *     publisher.registerHandler { msg =>
 *       w { msg should equal (message) }
 *       w.dismiss()
 *     }
 *
 *     publisher ! message
 *     w.await()
 *     publisher ! "Exit"
 *   }
 * }
 * </pre>
 *
 * @author Bill Venners
 */
trait AsyncAssertions extends PatienceConfiguration {

  /**
   * A configuration parameter that specifies the number of dismissals to wait for before returning normally
   * from an <code>await</code> call on a <code>Waiter</code>.
   *
   * @param value the number of dismissals for which to wait
   * @throws IllegalArgumentException if specified <code>value</code> is less than or equal to zero.
   *
   * @author Bill Venners
   */
  final case class Dismissals(value: Int)  // TODO check for IAE if negative

  /**
   * Returns a <code>Dismissals</code> configuration parameter containing the passed value, which
   * specifies the number of dismissals to wait for before returning normally from an <code>await</code>
   * call on a <code>Waiter</code>.
   */
  def dismissals(value: Int) = Dismissals(value)

  /**
   * Class that facilitates performing assertions outside the main test thread, such as assertions in callback methods
   * that are invoked asynchronously.
   *
   * <p>
   * To use <code>Waiter</code>, create an instance of it in the main test thread:
   * </p>
   *
   * <pre class=stHighlight">
   * val w = new Waiter // Do this in the main test thread
   * </pre>
   *
   * <p>
   * At some point later, call <code>await</code> on the waiter:
   * </p>
   *
   * <pre class="stHighlight">
   * w.await() // Call await() from the main test thread
   * </pre>
   *
   * <p>
   * The <code>await</code> call will block until it either receives a report of a failed assertion from a different thread, at which
   * point it will complete abruptly with the same exception, or until it is <em>dismissed</em> by a different thread (or threads), at
   * which point it will return normally. You can optionally specify a timeout and/or a number
   * of dismissals to wait for. Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.time.SpanSugar._
   *
   * w.await(timeout(300 millis), dismissals(2))
   * </pre>
   *
   * <p>
   * The default value for <code>timeout</code>, provided via an implicit <code>PatienceConfig</code> parameter, is 150 milliseconds. The default value for
   * <code>dismissals</code> is 1. The <code>await</code> method will block until either it is dismissed a sufficient number of times by other threads or
   * an assertion fails in another thread. Thus if you just want to perform assertions in just one other thread, only that thread will be
   * performing a dismissal, so you can use the default value of 1 for <code>dismissals</code>.
   * </p>
   *
   * <p>
   * <code>Waiter</code> contains four overloaded forms of <code>await</code>, two of which take an implicit
   * <code>PatienceConfig</code> parameter. To change the default timeout configuration, override or hide
   * (if you imported the members of <code>AsyncAssertions</code> companion object instead of mixing in the
   * trait) <code>patienceConfig</code> with a new one that returns your desired configuration.
   * </p>
   *
   * <p>
   * To dismiss a waiter, you just invoke <code>dismiss</code> on it:
   * </p>
   *
   * <pre class="stHighlight">
   * w.dismiss() // Call this from one or more other threads
   * </pre>
   *
   * <p>
   * You may want to put <code>dismiss</code> invocations in a finally clause to ensure they happen even if an exception is thrown.
   * Otherwise if a dismissal is missed because of a thrown exception, an <code>await</code> call will wait until it times out.
   * </p>
   *
   * <p>
   * Finally, to perform an assertion in a different thread, you just apply the <code>Waiter</code> to the assertion code. Here are
   * some examples:
   * </p>
   *
   * <pre class="stHighlight">
   * w { assert(1 + 1 === 3) }    // Can use assertions
   * w { 1 + 1 should equal (3) } // Or matchers
   * w { "hi".charAt(-1) }        // Any exceptions will be forwarded to await
   * </pre>
   *
   * <p>
   * Here's a complete example:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest._
   * import concurrent.AsyncAssertions
   * import matchers.ShouldMatchers
   * import scala.actors.Actor
   *
   * class ExampleSuite extends FunSuite with ShouldMatchers with AsyncAssertions {
   *
   *   case class Message(text: String)
   *
   *   class Publisher extends Actor {
   *
   *     @volatile private var handle: Message => Unit = { (msg) => }
   *
   *     def registerHandler(f: Message => Unit) {
   *       handle = f
   *     }
   *
   *     def act() {
   *       var done = false
   *       while (!done) {
   *         react {
   *           case msg: Message => handle(msg)
   *           case "Exit" => done = true
   *         }
   *       }
   *     }
   *   }
   *
   *   test("example one") {
   *
   *     val publisher = new Publisher
   *     val message = new Message("hi")
   *     val w = new Waiter
   *
   *     publisher.start()
   *
   *     publisher.registerHandler { msg =>
   *       w { msg should equal (message) }
   *       w.dismiss()
   *     }
   *
   *     publisher ! message
   *     w.await()
   *     publisher ! "Exit"
   *   }
   * }
   * </pre>
   *
   * @author Bill Venners
   */
  class Waiter {

    private final val creatingThread = Thread.currentThread

    @volatile private var dismissedCount = 0
    @volatile private var thrown: Option[Throwable] = None

    private def setThrownIfEmpty(t: Throwable) {
      synchronized {      // Why is this synchronized?
        if (thrown.isEmpty) thrown = Some(t)
      }
    }

    /**
     * Executes the passed by-name, and if it throws an exception, forwards it to the thread that calls <code>await</code>, unless
     * a by-name passed during a previous invocation of this method threw an exception.
     *
     * <p>
     * This method returns normally whether or not the passed function completes abruptly. If called multiple times, only the
     * first invocation that yields an exception will "win" and have its exception forwarded to the thread that calls <code>await</code>.
     * Any subsequent exceptions will be "swallowed." This method may be invoked by multiple threads concurrently, in which case it is a race
     * to see who wins and has their exception forwarded to <code>await</code>. The <code>await</code> call will eventually complete
     * abruptly with the winning exception, or return normally if that instance of <code>Waiter</code> is dismissed. Any exception thrown by
     * a by-name passed to <code>apply</code> after the <code>Waiter</code> has been dismissed will also be "swallowed."
     * </p>
     *
     * @param fun the by-name function to execute
     */
    def apply(fun: => Unit) {
      try {
        fun
      }
      catch { // Exceptions after the first are swallowed (need to get to dismissals later)
        case t: Throwable => setThrownIfEmpty(t)
        synchronized {
          notifyAll()
        }
      }
    }

    /**
     * Wait for an exception to be produced by the by-name passed to <code>apply</code> or the specified number of dismissals.
     *
     * <p>
     * This method may only be invoked by the thread that created the <code>Waiter</code>.
     * Each time this method completes, its internal dismissal count is reset to zero, so it can be invoked multiple times. However,
     * once <code>await</code> has completed abruptly with an exception produced during a call to <code>apply</code>, it will continue
     * to complete abruptly with that exception. The default value for the <code>dismissals</code> parameter is 1.
     * </p>
     *
     * <p>
     * The <code>timeout</code> parameter allows you to specify a timeout after which a <code>TestFailedException</code> will be thrown with
     * a detail message indicating the <code>await</code> call timed out. The default value for <code>timeout</code> is -1, which indicates
     * no timeout at all. Any positive value (or zero) will be interpreted as a timeout expressed in milliseconds. If no calls to <code>apply</code>
     * have produced an exception and an insufficient number of dismissals has been received by the time the <code>timeout</code> number
     * of milliseconds has passed, <code>await</code> will complete abruptly with <code>TestFailedException</code>.
     * </p>
     *
     * @param timeout the number of milliseconds timeout, or -1 to indicate no timeout (default is -1)
     * @param dismissals the number of dismissals to wait for (default is 1)
     */
    private def awaitImpl(timeout: Span, dismissals: Int = 1) {
      if (Thread.currentThread != creatingThread)
        throw new NotAllowedException(Resources("awaitMustBeCalledOnCreatingThread"), 2)

      val startTime: Long = System.nanoTime
      val endTime: Long = startTime + timeout.totalNanos
      def timedOut: Boolean = endTime < System.nanoTime
      while (dismissedCount < dismissals && !timedOut && thrown.isEmpty) {
        val timeLeft: Span = {
          val diff = endTime - System.nanoTime
          if (diff > 0) Span(diff, Nanoseconds) else Span.ZeroLength
        }
        synchronized {
          wait(timeLeft.millisPart, timeLeft.nanosPart)
        }
      }
      dismissedCount = 0 // reset the dismissed count to support multiple await calls
      if (thrown.isDefined)
        throw thrown.get
      else if (timedOut)
        throw new TestFailedException(Resources("awaitTimedOut"), 2)
    }

    /**
     * Wait for an exception to be produced by the by-name passed to <code>apply</code>, or one dismissal,
     * sleeping an interval between checks and timing out after a timeout, both configured
     * by an implicit <code>PatienceConfig</code>.
     *
     * <p>
     * This method may only be invoked by the thread that created the <code>Waiter</code>.
     * Each time this method completes, its internal dismissal count is reset to zero, so it can be invoked multiple times. However,
     * once <code>await</code> has completed abruptly with an exception produced during a call to <code>apply</code>, it will continue
     * to complete abruptly with that exception.
     * </p>
     *
     * <p>
     * The <code>timeout</code> parameter allows you to specify a timeout after which a
     * <code>TestFailedException</code> will be thrown with a detail message indicating the <code>await</code> call
     * timed out. If no calls to <code>apply</code> have produced an exception and an insufficient number of
     * dismissals has been received by the time the <code>timeout</code> has expired, <code>await</code> will
     * complete abruptly with <code>TestFailedException</code>.
     * </p>
     *
     * <p>
     * As used here, a "check" is checking to see whether an exception has been thrown by a by-name passed
     * to <code>apply</code> or a dismissal has occurred. The "interval" is the amount
     * of time the thread that calls <code>await</code> will sleep between "checks."
     * </p>
     *
     * @param config the <code>PatienceConfig</code> object containing the <code>timeout</code> parameter
     */
    def await()(implicit config: PatienceConfig) {
      awaitImpl(config.timeout)
    }

    /**
     * Wait for an exception to be produced by the by-name passed to <code>apply</code>, or one dismissal,
     * timing out after the specified timeout and sleeping an interval between checks configured
     * by an implicit <code>PatienceConfig</code>.
     *
     * <p>
     * This method may only be invoked by the thread that created the <code>Waiter</code>.
     * Each time this method completes, its internal dismissal count is reset to zero, so it can be invoked multiple times. However,
     * once <code>await</code> has completed abruptly with an exception produced during a call to <code>apply</code>, it will continue
     * to complete abruptly with that exception.
     * </p>
     *
     * <p>
     * The <code>timeout</code> parameter allows you to specify a timeout after which a
     * <code>TestFailedException</code> will be thrown with a detail message indicating the <code>await</code> call
     * timed out. If no calls to <code>apply</code> have produced an exception and an insufficient number of
     * dismissals has been received by the time the <code>timeout</code> has expired, <code>await</code> will
     * complete abruptly with <code>TestFailedException</code>.
     * </p>
     *
     * <p>
     * As used here, a "check" is checking to see whether an exception has been thrown by a by-name passed
     * to <code>apply</code> or a dismissal has occurred. The "interval" is the amount
     * of time the thread that calls <code>await</code> will sleep between "checks."
     * </p>
     *
     * @param timeout:  the <code>Timeout</code> configuration parameter containing the specified timeout
     */
    def await(timeout: Timeout) {
      awaitImpl(timeout.value)
    }

    /**
     * Wait for an exception to be produced by the by-name passed to <code>apply</code>, or the specified
     * number of dismissals, sleeping an interval between checks and timing out after a timeout, both configured
     * by an implicit <code>PatienceConfig</code>.
     *
     * <p>
     * This method may only be invoked by the thread that created the <code>Waiter</code>.
     * Each time this method completes, its internal dismissal count is reset to zero, so it can be invoked multiple times. However,
     * once <code>await</code> has completed abruptly with an exception produced during a call to <code>apply</code>, it will continue
     * to complete abruptly with that exception.
     * </p>
     *
     * <p>
     * The <code>timeout</code> parameter allows you to specify a timeout after which a
     * <code>TestFailedException</code> will be thrown with a detail message indicating the <code>await</code> call
     * timed out. If no calls to <code>apply</code> have produced an exception and an insufficient number of
     * dismissals has been received by the time the <code>timeout</code> has expired, <code>await</code> will
     * complete abruptly with <code>TestFailedException</code>.
     * </p>
     *
     * <p>
     * As used here, a "check" is checking to see whether an exception has been thrown by a by-name passed
     * to <code>apply</code> or the specified number of dismissals has occurred. The "interval" is the amount
     * of time the thread that calls <code>await</code> will sleep between "checks."
     * </p>
     *
     * @param dismissals:  the <code>Dismissals</code> configuration parameter containing the number of
     *    dismissals for which to wait
     * @param config the <code>PatienceConfig</code> object containing the <code>timeout</code> parameter
     */
    def await(dismissals: Dismissals)(implicit config: PatienceConfig) {
      awaitImpl(config.timeout, dismissals.value)
    }

    /**
     * Wait for an exception to be produced by the by-name passed to <code>apply</code>, or the specified
     * number of dismissals, timing out after the specified timeout and sleeping an interval between checks configured
     * by an implicit <code>PatienceConfig</code>.
     *
     * <p>
     * This method may only be invoked by the thread that created the <code>Waiter</code>.
     * Each time this method completes, its internal dismissal count is reset to zero, so it can be invoked multiple times. However,
     * once <code>await</code> has completed abruptly with an exception produced during a call to <code>apply</code>, it will continue
     * to complete abruptly with that exception.
     * </p>
     *
     * <p>
     * The <code>timeout</code> parameter allows you to specify a timeout after which a
     * <code>TestFailedException</code> will be thrown with a detail message indicating the <code>await</code> call
     * timed out. If no calls to <code>apply</code> have produced an exception and an insufficient number of
     * dismissals has been received by the time the <code>timeout</code> has expired, <code>await</code> will
     * complete abruptly with <code>TestFailedException</code>.
     * </p>
     *
     * <p>
     * As used here, a "check" is checking to see whether an exception has been thrown by a by-name passed
     * to <code>apply</code> or the specified number of dismissals has occurred. The "interval" is the amount
     * of time the thread that calls <code>await</code> will sleep between "checks."
     * </p>
     *
     * @param timeout:  the <code>Timeout</code> configuration parameter containing the specified timeout
     * @param dismissals:  the <code>Dismissals</code> configuration parameter containing the number of
     *    dismissals for which to wait
     */
    def await(timeout: Timeout, dismissals: Dismissals) {
      awaitImpl(timeout.value, dismissals.value)
    }

    /**
     * Increases the dismissal count by one.
     *
     * <p>
     * Once the dismissal count has reached the value passed to <code>await</code> (and no prior invocations of <code>apply</code>
     * produced an exception), <code>await</code> will return normally.
     * </p>
     */
    def dismiss() {
      dismissedCount += 1
      synchronized {
        notifyAll()
      }
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>AsyncAssertions</code> members as
 * an alternative to mixing in the trait. One use case is to import <code>AsyncAssertions</code>'s members so you can use
 * them in the Scala interpreter.
 */
object AsyncAssertions extends AsyncAssertions

