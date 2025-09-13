/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalactic.{source, Requirements}
import Requirements.requireNonNull
import org.scalatest.exceptions.{TestPendingException, TestCanceledException, TestFailedException, StackDepthException}

/**
 * Superclass for the possible outcomes of running a test.
 *
 * <p>
 * <code>Outcome</code> is the result type of the <code>withFixture</code> methods of traits
 * <a href="Suite.html#withFixture"><code>Suite</code></a> and <a href="FixtureSuite.html#withFixture"><code>FixtureSuite</code></a>, as well as their
 * <a href="Suite$NoArgTest.html"><code>NoArgTest</code></a> and <a href="FixtureSuite$OneArgTest.html"><code>OneArgTest</code></a> function types.
 * The four possible outcomes are:
 * </p>
 *
 * <ul>
 * <li><a href="Succeeded$.html"><code>Succeeded</code></a> - indicates a test succeeded</li>
 * <li><a href="Failed.html"><code>Failed</code></a> - indicates a test failed and contains an exception describing the failure</li>
 * <li><a href="Canceled.html"><code>Canceled</code></a> - indicates a test was canceled and contains an exception describing the cancelation</li>
 * <li><a href="Pending$.html"><code>Pending</code></a> - indicates a test was pending</li>
 * </ul>
 *
 * <p>
 * Note that "ignored" does not appear as a type of <code>Outcome</code>, because tests are
 * marked as ignored on the outside and skipped over as the suite executes. So an ignored test never runs, and therefore
 * never has an outcome. By contrast, a test is determined to be pending by running the test
 * and observing the actual outcome. If the test body completes abruptly with a <code>TestPendingException</code>,
 * then the outcome was that the test was pending.
 * </p>
 */
sealed abstract class Outcome extends Product with Serializable {

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
  
  /**
   * Converts this <code>Outcome</code> to a <code>Succeeded</code>.
   *
   * <p>
   * When this <code>Outcome</code> instance is not Succeeded, it behaves as followed:
   * </p>
   * 
   * <ul>
   *   <li>Failed(ex) - throws ex</li> 
   *   <li>Canceled(tce) - throws tce</li>
   *   <li>Pending - throws TestPendingException</li> 
   * </ul>
   *
   * @return Succeeded if this <code>Outcome</code> instance is a Succeeded.
   */
  def toSucceeded: Succeeded.type

  // Used internally to resuse the old code that was catching these exceptions when running tests. Eventually I would
  // like to rewrite that old code to use the result type, but it will still needs to catch and handle these exceptions
  // in the same way in case they come back from a user's withFixture implementation.
  private[scalatest] def toUnit: Unit = {
    this match {
      case Succeeded =>
      case Exceptional(e) => throw e
      case Pending => throw new TestPendingException
    }
  }
}

/**
 * Companion object for trait <code>Outcome</code> that contains an implicit method that enables 
 * collections of <code>Outcome</code>s to be flattened into a collections of contained exceptions.
 */
object Outcome {

  // SKIP-DOTTY-START
  import scala.language.implicitConversions
  // SKIP-DOTTY-END

  /**
   * Enables collections of <code>Outcome</code>s to be flattened into a collections of contained exceptions.
   *
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalatest._
   * import org.scalatest._
   *
   * scala&gt; import prop.TableDrivenPropertyChecks._
   * import prop.TableDrivenPropertyChecks._
   *
   * scala&gt; val squares = // (includes errors)
   *      |   Table(
   *      |     ("x", "square"),
   *      |     ( 0 ,     0   ),
   *      |     ( 1 ,     1   ),
   *      |     ( 2 ,     4   ),
   *      |     ( 3 ,     8   ),
   *      |     ( 4 ,    16   ),
   *      |     ( 5 ,    26   ),
   *      |     ( 6 ,    36   )
   *      |   )
   * squares: org.scalatest.prop.TableFor2[Int,Int] =
   *   TableFor2((x,square), (0,0), (1,1), (2,4), (3,8), (4,16), (5,26), (6,36))
   * </pre>
   *
   * <p>
   * Given the above table, which includes some errors, you can obtain an <code>IndexedSeq</code> of the <code>Outcome</code>s
   * of executing an assertion on each row of the table with <code>outcomeOf</code>, like this:
   * </p>
   * 
   * <pre class="stREPL">
   * scala&gt; import OutcomeOf._
   * import OutcomeOf._
   *
   * scala&gt; import Matchers._
   * import Matchers._
   *
   * scala&gt; val outcomes = for ((x, square) &lt;- squares) yield outcomeOf { square shouldEqual x * x }
   * outcomes: IndexedSeq[org.scalatest.Outcome] =
   *   Vector(Succeeded, Succeeded, Succeeded,
   *   Failed(org.scalatest.exceptions.TestFailedException: 8 did not equal 9), Succeeded,
   *   Failed(org.scalatest.exceptions.TestFailedException: 26 did not equal 25), Succeeded)
   * </pre>
   *
   * <p>
   * Now you have a collection of all the outcomes, including successful ones. If you just want the <code>Failed</code> and <code>Canceled</code> outcomes, which
   * contain exceptions, you can filter out anything that isn't "exceptional," like this:
   * </p>
   * 
   * <pre class="stREPL">
   * scala&gt; outcomes.filter(_.isExceptional)
   * res1: IndexedSeq[org.scalatest.Outcome] =
   *   Vector(Failed(org.scalatest.exceptions.TestFailedException: 8 did not equal 9),
   *   Failed(org.scalatest.exceptions.TestFailedException: 26 did not equal 25))
   * </pre>
   *
   * <p>
   * But if you just wanted the contained exceptions, you can (thanks to this implicit method) invoke <code>flatten</code> on your collection:
   * </p>
   * 
   * <pre class="stREPL">
   * scala&gt; outcomes.flatten
   * res2: IndexedSeq[Throwable] =
   *   Vector(org.scalatest.exceptions.TestFailedException: 8 did not equal 9,
   *   org.scalatest.exceptions.TestFailedException: 26 did not equal 25)
   * </pre>
   */
  // SKIP-DOTTY-START 
  implicit def convertOutcomeToIterator(outcome: Outcome): Iterator[Throwable] =
    outcome match {
      case Exceptional(ex) => // Return an iterator with one Throwable in it
        new Iterator[Throwable] {
          private var spent: Boolean = false
          def hasNext: Boolean = !spent
          def next(): Throwable =
            if (!spent) {
              spent = true
              ex
           } else throw new NoSuchElementException
        }
      case _ => // Return an empty iterator
        new Iterator[Throwable] {
          def hasNext: Boolean = false
          def next(): Throwable = throw new NoSuchElementException
        }
    }
  // SKIP-DOTTY-END
  //DOTTY-ONLY given Conversion[Outcome, Iterator[Throwable]] with {
  //DOTTY-ONLY   def apply(outcome: Outcome): Iterator[Throwable] = convertOutcomeToIterator(outcome)
  //DOTTY-ONLY }

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Converts an <code>Outcome</code> to an iterator over its contained exceptions, if any.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param outcome the <code>Outcome</code> to convert
  //DOTTY-ONLY  * @return an iterator over the contained exceptions, or an empty iterator if none exist
  //DOTTY-ONLY  */
  //DOTTY-ONLY def convertOutcomeToIterator(outcome: Outcome): Iterator[Throwable] =
  //DOTTY-ONLY   outcome match {
  //DOTTY-ONLY     case Exceptional(ex) => // Return an iterator with one Throwable in it
  //DOTTY-ONLY       new Iterator[Throwable] {
  //DOTTY-ONLY         private var spent: Boolean = false
  //DOTTY-ONLY         def hasNext: Boolean = !spent
  //DOTTY-ONLY         def next(): Throwable =
  //DOTTY-ONLY           if (!spent) {
  //DOTTY-ONLY             spent = true
  //DOTTY-ONLY             ex
  //DOTTY-ONLY          } else throw new NoSuchElementException
  //DOTTY-ONLY       }
  //DOTTY-ONLY     case _ => // Return an empty iterator
  //DOTTY-ONLY       new Iterator[Throwable] {
  //DOTTY-ONLY         def hasNext: Boolean = false
  //DOTTY-ONLY         def next(): Throwable = throw new NoSuchElementException
  //DOTTY-ONLY       }
  //DOTTY-ONLY   }
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
 * <pre class="stHighlight">
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
   * <pre class="stHighlight">
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
      case tce: TestCanceledException => Canceled(tce)
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
   * <pre class="stHighlight">
   * abstract override def withFixture(test: NoArgTest): Outcome = {
   *   super.withFixture(test) match {
   *     case Exceptional(e: StackDepth) =&gt; Exceptional(e.severedAtStackDepth)
   *     case o =&gt; o
   *   }
   * }
   * </pre>
   *
   * @param res the <code>Outcome</code> to extract the throwable from.
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
 * object is that this object indicates one test (or assertion) succeeded, whereas the <code>SucceededStatus</code> object indicates the absence of any failed tests or
 * aborted suites during a run. Both are used as the result type of <a href="Suite.html#lifecycle-methods"><code>Suite</code></a> lifecycle methods, but <code>Succeeded</code>
 * is a possible result of <code>withFixture</code>, whereas <code>SucceededStatus</code> is a possible result of <code>run</code>, <code>runNestedSuites</code>,
 * <code>runTests</code>, or <code>runTest</code>. In short, <code>Succeeded</code> is always just about one test (or assertion), whereas <code>SucceededStatus</code> could be
 * about something larger: multiple tests or an entire suite.
 * </p>
 */
case object Succeeded extends Outcome with compatible.Assertion {

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

  /**
   * Converts this <code>Outcome</code> to a <code>Succeeded</code>.
   *
   * @return This Succeeded instance.
   */
  def toSucceeded: Succeeded.type = this
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
case class Failed(exception: Throwable) extends Exceptional(exception) {

  require(!exception.isInstanceOf[TestCanceledException], "a TestCanceledException was passed to Failed's constructor")
  require(!exception.isInstanceOf[TestPendingException], "a TestPendingException was passed to Failed's constructor")

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
  
  /**
   * Converts this <code>Outcome</code> to a <code>Succeeded</code>.
   * 
   * <p>
   * The implmentation of this class will re-throw the passed in exception. 
   * </p>
   */
  def toSucceeded: Succeeded.type = throw exception
}

/**
 * Companion object for `Failed` offering several factory methods.
 */
object Failed {

  /**
    * Creates a <code>Failed</code> instance, with a <code>TestFailedException</code> set as its <code>exception</code> field.
    *
    * @return An instance of <code>Failed</code> with a <code>TestFailedException</code> set as its <code>exception</code> field.
    */
  def apply()(implicit pos: source.Position): Failed = new Failed(new TestFailedException((_: StackDepthException) => None, None, pos))

  /**
    * Creates a <code>Failed</code> instance with the passed in message.
    *
    * @param message the message for the <code>TestFailedException</code> set as its <code>exception</code> field
    * @return An instance of <code>Failed</code> with a <code>TestFailedException</code> created from passed in <code>message</code> set as its <code>exception</code> field.
    */
  def apply(message: String)(implicit pos: source.Position): Failed = new Failed(new TestFailedException((_: StackDepthException) => Some(message), None, pos))

  /**
    * Creates a <code>Failed</code> instance with the passed in message and cause.
    *
    * @param message the message for the <code>TestFailedException</code> set as its <code>exception</code> field
    * @param cause the cause for the <code>TestFailedException</code> set as its <code>exception</code> field
    * @return An instance of <code>Failed</code> with a <code>TestFailedException</code> created from passed in <code>message</code> and <code>cause</code> set as its <code>exception</code> field.
    */
  def apply(message: String, cause: Throwable)(implicit pos: source.Position): Failed = {
    // I always wrap this in a TFE because I need to do that to get the message in there.
    require(!cause.isInstanceOf[TestCanceledException], "a TestCanceledException was passed to a factory method in object Failed")
    require(!cause.isInstanceOf[TestPendingException], "a TestPendingException was passed to a factory method in object Failed")
    new Failed(new TestFailedException((_: StackDepthException) => Some(message), Some(cause), pos))
  }

  /**
    * Creates a <code>Failed</code> with the passed in cause.
    *
    * @param cause the passed in cause
    * @return A <code>Failed</code> with <code>exception</code> field set to a newly created <code>TestFailedException</code> using the passed in <code>cause</code>.
    */
  def here(cause: Throwable)(implicit pos: source.Position): Failed = {
    require(!cause.isInstanceOf[TestCanceledException], "a TestCanceledException was passed to the \"here\" factory method in object Failed")
    require(!cause.isInstanceOf[TestPendingException], "a TestPendingException was passed to the \"here\" factory method in object Failed")

    new Failed(
      if (cause.getMessage != null)
        new TestFailedException((_: StackDepthException) => Some(cause.getMessage), Some(cause), pos)
       else
        new TestFailedException((_: StackDepthException) => None, Some(cause), pos)
     )
  }
}

/**
 * Outcome for a test that was canceled, containing an exception describing the cause of the cancelation.
 *
 * @param ex the <code>TestCanceledException</code> contained in this <code>Exceptional</code>.
 */
case class Canceled(exception: TestCanceledException) extends Exceptional(exception) {

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
  
  /**
   * Converts this <code>Outcome</code> to a <code>Succeeded</code>.
   * 
   * <p>
   * The implmentation of this class will re-throw the passed in exception. 
   * </p>
   */
  def toSucceeded: Succeeded.type = throw exception
}

/**
 * Companion object to class <code>Canceled</code> that provides, in addition to the extractor and factory method
 * provided by the compiler given its companion is a case class, a second factory method 
 * that produces a <code>Canceled</code> outcome given a string message.
 */
object Canceled {

  /**
    * Creates a <code>Canceled</code> instance, with a <code>TestCanceledException</code> set as its <code>exception</code> field.
    *
    * @return An instance of <code>Canceled</code> with a <code>TestCanceledException</code> set as its <code>exception</code> field.
    */
  def apply()(implicit pos: source.Position): Canceled = new Canceled(new TestCanceledException((_: StackDepthException) => None, None, Left(pos), None))

  /**
    * Creates a <code>Canceled</code> instance with the passed in message and cause.
    *
    * @param message the message for the <code>TestCanceledException</code> set as its <code>exception</code> field
    * @param cause the cause for the <code>TestCanceledException</code> set as its <code>exception</code> field
    * @return An instance of <code>Canceled</code> with a <code>TestCanceledException</code> created from passed in <code>message</code> and <code>cause</code> set as its <code>exception</code> field.
    */
  def apply(message: String, cause: Throwable)(implicit pos: source.Position): Canceled = // TODO write tests for NPEs
    new Canceled(new TestCanceledException((_: StackDepthException) => Some(message), Some(cause), Left(pos), None))

  /**
    * Creates a <code>Canceled</code> instance with the passed in <code>Throwable</code>.  If the passed in <code>Throwable</code> is a <code>TestCanceledException</code>,
    * it will be set as <code>exception</code> field, in other case a new <code>TestCanceledException</code> will be created using <code>ex</code> as its <code>cause</code>
    *
    * @param ex the passed in <code>Throwable</code>
    * @return An instance of <code>Canceled</code> with <code>ex</code> set as its <code>exception</code> field if <code>ex</code> is a <code>TestCanceledException</code>, or a newly created <code>TestCanceledException</code> with <code>ex</code> set as its <code>cause</code> if <code>ex</code> is not a <code>TestCanceledException</code>.
    */
  def apply(ex: Throwable)(implicit pos: source.Position): Canceled = { // TODO write tests for NPEs
    ex match {
      case tce: TestCanceledException =>
        new Canceled(tce)
      case _ =>
        val msg = ex.getMessage
        if (msg == null)
          new Canceled(new TestCanceledException((_: StackDepthException) => None, Some(ex), Left(pos), None))
        else 
          new Canceled(new TestCanceledException((_: StackDepthException) => Some(msg), Some(ex), Left(pos), None))
    }
  }

  /**
   * Creates a <code>Canceled</code> outcome given a string message.
   *
   * <p>
   * For example, trait <code>CancelAfterFailure</code> uses this factory method to create
   * a <code>Canceled</code> status if a <code>cancelRemaining</code> flag is set, which will
   * be the case if a test failed previously while running the suite:
   * </p>
   *
   * <pre class="stHighlight">
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
  def apply(message: String)(implicit pos: source.Position): Canceled = {
    requireNonNull(message)
    val e = new TestCanceledException((_: StackDepthException) => Some(message), None, Left(pos), None)
    //e.fillInStackTrace()
    Canceled(e)
  }

  /**
    * Creates a <code>Canceled</code> with the passed in cause.
    *
    * @param cause the passed in cause
    * @return A <code>Canceled</code> with <code>exception</code> field set to a newly created <code>TestCanceledException</code> using the passed in <code>cause</code>.
    */
  def here(cause: Throwable)(implicit pos: source.Position): Canceled = {
    new Canceled(
      if (cause.getMessage != null)
        new TestCanceledException((_: StackDepthException) => Some(cause.getMessage), Some(cause), Left(pos), None)
       else
        new TestCanceledException((_: StackDepthException) => None, Some(cause), Left(pos), None)
     )
  }
}

/**
 * Outcome for a test that was pending, which contains an optional string giving more information on what exactly is needed
 * for the test to become non-pending.
 *
 * @param message an optional message describing the reason the test is pending
 */
case object Pending extends Outcome {

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
  
  /**
   * Converts this <code>Outcome</code> to a <code>Succeeded</code>.
   * 
   * <p>
   * The implmentation of this class will throw <code>TestPendingException</code> with the passed in message. 
   * </p>
   */
  def toSucceeded: Succeeded.type = throw new TestPendingException
}

