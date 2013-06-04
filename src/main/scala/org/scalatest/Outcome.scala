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
package org.scalatest

/**
 * Superclass for the possible outcomes of running a test.
 *
 * <p>
 * <code>Outcome</code> is the result type of the <code>withFixture</code> methods of traits
 * <a href="Suite.html#withFixture"><code>Suite</code></a> and <a href="fixture/Suite.html#withFixture"><code>fixture.Suite</code></a>, as well as their
 * <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a> and <a href="fixture/Suite$OneArgTest.html"><code>OneArgTest</code></a> function types.
 * The five possible outcomes are:
 * </p>
 *
 * <ul>
 * <li><a href="Succeeded$.html"><code>Succeeded</code></a> - indicates a test succeeded</li>
 * <li><a href="Failed.html"><code>Failed</code></a> - indicates a test failed and contains an exception describing the failure</li>
 * <li><a href="Canceled.html"><code>Canceled</code></a> - indicates a test was canceled and contains an exception describing the cancelation</li>
 * <li><a href="Pending.html"><code>Pending</code></a> - indicates a test was pending</li>
 * <li><a href="Omitted$.html"><code>Omitted</code></a> - indicates a test was omitted</li>
 * </ul>
 *
 * <p>
 * Note that "ignored" does not appear as a type of <code>Outcome</code>, because tests are
 * marked as ignored on the outside and skipped over as the suite executes. So an ignored test never runs, and therefore
 * never has an outcome. By contrast, a test is determined to be pending by running the test
 * and observing the actual outcome. If the test body completes abruptly with a <code>TestPendingException</code>,
 * then the outcome was that the test was pending.
 * </p>
 *
 * <p>
 * Note: <code>Omitted</code> is currently not used in ScalaTest, but will eventually be used
 * to indicate everthing except specification-text provided by mechanisms such
 * as <code>GivenWhenThen</code> has been omitted or elided from the test body. This will enable
 * full specification output to be obtained without waiting for the actual test code
 * to execute.
 * </p>
 */
sealed abstract class Outcome {

  /**
   * Indicates whether this <code>Outcome</code> represents a test that succeeded.
   *
   * <p>
   * This class's implementation of this method always returns <code>false</code>.
   * </p>
   *
   * @return true if this <code>Outcome</code> is an instance of <code>Succeeded</code>.
   */
  val isSucceeded: Boolean = false

  /**
   * Indicates whether this <code>Outcome</code> represents a test that failed.
   *
   * <p>
   * This class's implementation of this method always returns <code>false</code>.
   * </p>
   *
   * @return true if this <code>Outcome</code> is an instance of <code>Failed</code>.
   */
  val isFailed: Boolean = false

  /**
   * Indicates whether this <code>Outcome</code> represents a test that was canceled.
   *
   * <p>
   * This class's implementation of this method always returns <code>false</code>.
   * </p>
   *
   * @return true if this <code>Outcome</code> is an instance of <code>Canceled</code>.
   */
  val isCanceled: Boolean = false

  /**
   * Indicates whether this <code>Outcome</code> represents a test that was pending.
   *
   * <p>
   * This class's implementation of this method always returns <code>false</code>.
   * </p>
   *
   * @return true if this <code>Outcome</code> is an instance of <code>Pending</code>.
   */
  val isPending: Boolean = false

  /**
   * Indicates whether this <code>Outcome</code> represents a test that was omitted.
   *
   * <p>
   * This class's implementation of this method always returns <code>false</code>.
   * </p>
   *
   * @return true if this <code>Outcome</code> is an instance of <code>Omitted</code>.
   */
  val isOmitted: Boolean = false

  /**
   * Indicates whether this <code>Outcome</code> represents a test that either failed or was canceled, in which case this <code>Outcome</code> will contain an exception.
   *
   * @return true if this <code>Outcome</code> is an instance of either <code>Failed</code> or <code>Canceled</code>.
   */
  val isExceptional: Boolean = false

  /**
   * Converts this <code>Outcome</code> to an <code>Option[Throwable]</code>.
   *
   * <p>
   * This class's implementation of this method always returns <code>None</code>.
   * </p>
   *
   * @return a <code>Some</code> wrapping the contained exception if this <code>Outcome</code> is an instance of either <code>Failed</code> or <code>Canceled</code>.
   */
  def toOption: Option[Throwable] = None

  // Used internally to resuse the old code that was catching these exceptions when running tests. Eventually I would
  // like to rewrite that old code to use the result type, but it will still needs to catch and handle these exceptions
  // in the same way in case they come back from a user's withFixture implementation.
  private[scalatest] def toUnit {
    this match {
      case Succeeded =>
      case Exceptional(e) => throw e
      case Pending(_) => throw new exceptions.TestPendingException
      case Omitted => throw new exceptions.TestOmittedException
    }
  }
}

/**
 * Superclass for the two outcomes of running a test that contain an exception: <code>Failed</code> and <code>Canceled</code>.
 *
 * <p>
 * This class provides a <code>toOption</code> method that returns a <code>Some</code> wrapping the contained exception, and
 * an <code>isExceptional</code> field with the value <code>true</code>. It's companion object provides an extractor that
 * enables patterns that match a test that either failed or canceled, as in:
 * </p>
 *
 * <pre>
 * outcome match {
 *   case Exceptional(ex) =&gt; // handle failed or canceled case
 *   case _ =&gt; // handle succeeded, pending, or omitted case
 * }
 * </pre>
 *
 * @param ex the <code>Throwable</code> contained in this <code>Exceptional</code>.
 */
sealed abstract class Exceptional(ex: Throwable) extends Outcome {

  /**
   * Indicates that this <code>Outcome</code> represents a test that either failed or was canceled.
   *
   * @return true
   */
  override val isExceptional: Boolean = true

  /**
   * Converts this <code>Exceptional</code> to a <code>Some</code> that wraps the contained exception.
   *
   * @return A <code>Some</code> wrapping the exception contained in this <code>Exceptional</code>.
   */
  override def toOption: Option[Throwable] = Some(ex)
}

/**
 * Companion object to class <code>Exceptional</code> that provides a factory method and an extractor that enables
 * patterns that match both <code>Failed</code> and <code>Canceled</code> outcomes and 
 * extracts the contained exception and a factory method.
 */
object Exceptional {

  /**
   * Creates an <code>Exceptional</code> instance given the passed <code>Throwable</code>.
   *
   * <p>
   * If the passed <code>Throwable</code> is an instance of <code>TestCanceledException</code>, this
   * method will return <code>Canceled</code> containing that <code>TestCanceledException</code>. Otherwise,
   * it returns a <code>Failed</code> containing the <code>Throwable</code>.
   * </p>
   *
   * <p>
   * For example, trait <a href="SeveredStackTraces.html"><code>SeveredStackTraces</code></a> uses this
   * factory method to sever the stack trace of the exception contained in either a <code>Failed</code> and <code>Canceled</code> 
   * like this:
   * </p>
   * 
   * <pre>
   * abstract override def withFixture(test: NoArgTest): Outcome = {
   *   super.withFixture(test) match {
   *     case Exceptional(e: StackDepth) =&gt; Exceptional(e.severedAtStackDepth)
   *     case o =&gt; o
   *   }
   * }
   * </pre>
   *
   * @return a <code>Failed</code> or <code>Canceled</code> containing the passed exception.
   */
  def apply(e: Throwable): Exceptional = 
    e match {
      case tce: exceptions.TestCanceledException => Canceled(tce)
      case _ => Failed(e)
    }

  /**
   * Extractor enabling patterns that match both <code>Failed</code> and </code>Canceled</code> outcomes, 
   * extracting the contained exception.
   *
   * <p>
   * For example, trait <a href="SeveredStackTraces.html"><code>SeveredStackTraces</code></a> uses this
   * extractor to sever the stack trace of the exception contained in either a <code>Failed</code> and <code>Canceled</code> 
   * like this:
   * </p>
   * 
   * <pre>
   * abstract override def withFixture(test: NoArgTest): Outcome = {
   *   super.withFixture(test) match {
   *     case Exceptional(e: StackDepth) =&gt; Exceptional(e.severedAtStackDepth)
   *     case o =&gt; o
   *   }
   * }
   * </pre>
   *
   * @return a <code>Some</code> wrapping the contained throwable if <code>res</code> is an instance of
   *     either <code>Failed</code> or <code>Canceled</code>, else <code>None</code>.
   */
  def unapply(res: Outcome): Option[Throwable] = 
    res match {
      case Failed(ex) => Some(ex)
      case Canceled(ex) => Some(ex)
      case _ => None
    }
}

/**
 * Outcome for a test that succeeded.
 *
 * <p>
 * Note: the difference between this <code>Succeeded</code> object and the similarly named <a href="SucceededStatus$.html"><code>SucceededStatus</code></a>
 * object is that this object indicates one test succeeded, whereas the <code>SucceededStatus</code> object indicates the absence of any failed tests or
 * aborted suites during a run. Both are used as the result type of <code>Suite</code> lifecycle methods, but <code>Succeeded</code>
 * is a possible result of <code>withFixture</code>, whereas <code>SucceededStatus</code> is a possible result of <code>run</code>, <code>runNestedSuites</code>,
 * <code>runTests</code>, or <code>runTest</code>. In short, <code>Succeeded</code> is always just about one test, whereas <code>SucceededStatus</code> could be
 * about something larger: multiple tests or an entire suite.
 * </p>
 */
case object Succeeded extends Outcome {

  /**
   * Indicates that this <code>Outcome</code> represents a test that succeeded.
   *
   * <p>
   * This class's implementation of this method always returns <code>true</code>.
   * </p>
   *
   * @return true
   */
  override val isSucceeded: Boolean = true
}

/**
 * Outcome for a test that failed, containing an exception describing the cause of the failure.
 *
 * <p>
 * Note: the difference between this <code>Failed</code> class and the similarly named <a href="FailedStatus$.html"><code>FailedStatus</code></a>
 * object is that an instance of this class indicates one test failed, whereas the <code>FailedStatus</code> object indicates either one or more tests failed
 * and/or one or more suites aborted during a run. Both are used as the result type of <code>Suite</code> lifecycle methods, but <code>Failed</code>
 * is a possible result of <code>withFixture</code>, whereas <code>FailedStatus</code> is a possible result of <code>run</code>, <code>runNestedSuites</code>,
 * <code>runTests</code>, or <code>runTest</code>. In short, <code>Failed</code> is always just about one test, whereas <code>FailedStatus</code> could be
 * about something larger: multiple tests or an entire suite.
 * </p>
 *
 * @param ex the <code>Throwable</code> contained in this <code>Failed</code>.
 */
case class Failed(ex: Throwable) extends Exceptional(ex) {

  /**
   * Indicates that this <code>Outcome</code> represents a test that failed.
   *
   * <p>
   * This class's implementation of this method always returns <code>true</code>.
   * </p>
   *
   * @return true
   */
  override val isFailed: Boolean = true
}

/**
 * Outcome for a test that was canceled, containing an exception describing the cause of the cancelation.
 *
 * @param ex the <code>TestCanceledException</code> contained in this <code>Exceptional</code>.
 */
case class Canceled(ex: exceptions.TestCanceledException) extends Exceptional(ex) {

  /**
   * Indicates that this <code>Outcome</code> represents a test that was canceled.
   *
   * <p>
   * This class's implementation of this method always returns <code>true</code>.
   * </p>
   *
   * @return true
   */
  override val isCanceled: Boolean = true
}

/**
 * Companion object to class <code>Canceled</code> that provides, in addition to the extractor and factory method
 * provided by the compiler given its companion is a case class, a second factory method 
 * that produces a <code>Canceled</code> outcome given a string message.
 */
object Canceled {

  /**
   * Creates a <code>Canceled</code> outcome given a string message.
   *
   * <p>
   * For example, trait <code>CancelAfterFailure</code> uses this factory method to create
   * a <code>Canceled</code> status if a <code>cancelRemaining</code> flag is set, which will
   * be the case if a test failed previously while running the suite:
   * </p>
   *
   * <pre>
   * abstract override def withFixture(test: NoArgTest): Outcome = {
   *   if (cancelRemaining) 
   *     Canceled("Canceled by CancelOnFailure because a test failed previously")
   *   else
   *     super.withFixture(test) match {
   *       case failed: Failed =&gt;
   *         cancelRemaining = true
   *         failed
   *       case outcome =&gt; outcome
   *     }
   *  }
   * </pre>
   */
  def apply(message: String): Canceled = {
    if (message == null)
      throw new NullPointerException("message was null")
    val e = new exceptions.TestCanceledException(message, 0) // TODO: Verify this is hte correct stack depth.
    e.fillInStackTrace()
    Canceled(e)
  }
}

/**
 * Outcome for a test that was pending, which contains an optional string giving more information on what exactly is needed
 * for the test to become non-pending.
 *
 * @param message an optional message describing the reason the test is pending
 */
case class Pending(message: Option[String] = None) extends Outcome {

  /**
   * Indicates that this <code>Outcome</code> represents a test that was pending.
   *
   * <p>
   * This class's implementation of this method always returns <code>true</code>.
   * </p>
   *
   * @return true
   */
  override val isPending: Boolean = true
}

/**
 * Outcome for a test that was omitted.
 *
 * <p>
 * Note: This outcome is currently not used in ScalaTest, but will eventually be used
 * to indicate everthing except specification-text provided by mechanisms such
 * as <code>GivenWhenThen</code> has been omitted from the test body. This will enable
 * a the specification output to be obtained without waiting for the actual test code
 * to execute.
 * </p>
 */
case object Omitted extends Outcome {

  /**
   * Indicates that this <code>Outcome</code> represents a test that was omitted.
   *
   * <p>
   * This class's implementation of this method always returns <code>false</code>.
   * </p>
   *
   * @return true
   */
  override val isOmitted: Boolean = true
}

