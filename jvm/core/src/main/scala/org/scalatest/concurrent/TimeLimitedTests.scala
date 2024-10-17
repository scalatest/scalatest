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
package org.scalatest.concurrent

import org.scalatest.TestSuiteMixin
import org.scalatest.TestSuite
import TimeLimits._
import org.scalatest.Resources
import org.scalatest.time.Span
import org.scalatest.exceptions.TimeoutField
import org.scalatest.Outcome
import org.scalatest.Exceptional
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun

/**
 * Trait that when mixed into a suite class establishes a time limit for its tests.
 *
 * <strong>
 * Unfortunately this trait experienced a potentially breaking change in 3.0: previously
 * this trait declared a <code>defaultTestInterruptor</code> <code>val</code> of type
 * <code>Interruptor</code>, in 3.0 that was renamed to <code>defaultTestSignaler</code>
 * and given type <code>Signaler</code>. The reason is that the default <code>Interruptor</code>, <code>ThreadInterruptor</code>,
 * did not make sense on Scala.js&#8212;in fact, the entire notion of interruption did not make
 * sense on Scala.js. <code>Signaler</code>'s default is <code>DoNotSignal</code>, which is a better
 * default on Scala.js, and works fine as a default on the JVM.
 * <code>Timeouts</code> was left the same in 3.0, so existing code using it would
 * continue to work as before, but after a deprecation period <code>Timeouts</code> will be
 * supplanted by <code>TimeLimits</code>, which uses <code>Signaler</code>. <code>TimeLimitedTests</code>
 * now uses <code>TimeLimits</code> instead of <code>Timeouts</code>, so if you overrode the default
 * <code>Interruptor</code> before, you'll need to change it to the equivalent <code>Signaler</code>.
 * And if you were depending on the default being a <code>ThreadInterruptor</code>, you'll need to
 * override <code>defaultTestSignaler</code> and set it to <code>ThreadSignaler</code>.
 * </strong>
 *
 * <p>
 * This trait overrides <code>withFixture</code>, wrapping a <code>super.withFixture(test)</code> call
 * in a <code>failAfter</code> invocation, specifying a time limit obtained by invoking <code>timeLimit</code>
 * and a <a href="Signaler.html"><code>Signaler</code></a> by invoking <code>defaultTestSignaler</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * failAfter(timeLimit) {
 *   super.withFixture(test)
 * } (defaultTestSignaler)
 * </pre>
 *
 * <p>
 * Note that the <code>failAfter</code> method executes the body of the by-name passed to it using the same
 * thread that invoked <code>failAfter</code>. This means that the same thread will run the <code>withFixture</code> method
 * as well as each test, so no extra synchronization is required. A second thread is used to run a timer, and if the timeout
 * expires, that second thread will attempt to signal the main test thread via the <code>defaultTestSignaler</code>.
 * </p>
 * 
 * <p>
 * The <code>timeLimit</code> field is abstract in this trait. Thus you must specify a time limit when you use it.
 * For example, the following code specifies that each test must complete within 200 milliseconds:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.FunSpec
 * import org.scalatest.concurrent.TimeLimitedTests
 * import org.scalatest.time.SpanSugar._
 * 
 * class ExampleSpec extends FunSpec with TimeLimitedTests {
 *
 *   // Note: You may need to either write 200.millis or (200 millis), or
 *   // place a semicolon or blank line after plain old 200 millis, to
 *   // avoid the semicolon inference problems of postfix operator notation.
 *   val timeLimit = 200 millis
 *
 *   describe("A time-limited test") {
 *     it("should succeed if it completes within the time limit") {
 *       Thread.sleep(100)
 *     }
 *     it("should fail if it is taking too darn long") {
 *       Thread.sleep(300)
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run the above <code>ExampleSpec</code>, the second test will fail with the error message: 
 * </p>
 * 
 * <p>
 * <code>The test did not complete within the specified 200 millisecond time limit.</code>
 * </p>
 * 
 * <p>
 * The <code>failAfter</code> method uses an <code>Signaler</code> to attempt to signal the main test thread if the timeout
 * expires. The default <code>Signaler</code> returned by the <code>defaultTestSignaler</code> method is a
 * <a href="DoNotSignal$.html"><code>DoNotSignal</code></a>, which does not signal the main test thread to stop. If you wish to change this
 * signaling strategy, override <code>defaultTestSignaler</code> to return a different <code>Signaler</code>. For example,
 * here's how you'd change the default to <a href="ThreadSignaler$.html"><code>ThreadSignaler</code></a>, which will
 * interrupt the main test thread when time is up:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.FunSpec
 * import org.scalatest.concurrent.{ThreadSignaler, TimeLimitedTests}
 * import org.scalatest.time.SpanSugar._
 *
 * class ExampleSignalerSpec extends FunSpec with TimeLimitedTests {
 *
 * val timeLimit = 200 millis
 *
 * override val defaultTestSignaler = ThreadSignaler
 *
 * describe("A time-limited test") {
 *     it("should succeed if it completes within the time limit") {
 *       Thread.sleep(100)
 *     }
 *     it("should fail if it is taking too darn long") {
 *       Thread.sleep(300)
 *     }
 *   }
 * }
 * </pre>
 * 
 * <p>
 * Like the previous incarnation of <code>ExampleSuite</code>, the second test will fail with an error message that indicates
 * a timeout expired. But whereas in the previous case, the <code>Thread.sleep</code> would be interrupted after 200 milliseconds,
 * in this case it is never interrupted. In the previous case, the failed test requires a little over 200 milliseconds to run.
 * In this case, because the <code>sleep(300)</code> is never interrupted, the failed test requires a little over 300 milliseconds
 * to run.
 * </p>
 */
trait TimeLimitedTests extends TestSuiteMixin { this: TestSuite =>

  /**
   * A stackable implementation of <code>withFixture</code> that wraps a call to <code>super.withFixture</code> in a 
   * <code>failAfter</code> invocation.
   * 
   * @param test the test on which to enforce a time limit
   */
  abstract override def withFixture(test: NoArgTest): Outcome = {
    try {
      // TODO: should pass in the prettifier also
      failAfterImpl(timeLimit, defaultTestSignaler, org.scalactic.Prettifier.default, test.pos, test.pos.map(getStackDepthFun(_)).getOrElse(getStackDepthFun("TimeLimits.scala", "failAfter", 2))) {
        super.withFixture(test)
      }
    }
    catch {
      case e: org.scalatest.exceptions.ModifiableMessage[_] with TimeoutField => 
        Exceptional(e.modifyMessage(opts => Some(Resources.testTimeLimitExceeded(e.timeout.prettyString))))
      case t: Throwable => 
        Exceptional(t)
    }
  }

  /**
   * The time limit, in milliseconds, in which each test in a <code>Suite</code> that mixes in
   * <code>TimeLimitedTests</code> must complete.
   */
  def timeLimit: Span
  
  /**
   * The default <a href="Signaler.html"><code>Signaler</code></a> strategy used to interrupt tests that exceed their time limit.
   * 
   * <p>
   * This trait's implementation of this method returns <a href="DoNotSignal$.html"><code>DoNotSignal</code></a>, which does not signal/interrupt
   * the main test and future thread. Override this method to change the test signaling strategy.
   * </p>
   * 
   * @return a <code>ThreadInterruptor</code>
   */
  val defaultTestSignaler: Signaler = DoNotSignal
}

/*
Will need to add cancelAfter to the doc comment in 2.0.
*/

