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
 * Stackable trait that can be mixed into suites that need code executed before and/or after running each test.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use trait <code>BeforeAndAfterEach</code> when you want to stack traits that perform side-effects before and/or after tests, rather
 * than at the beginning or end of tests. 
 * <em>Note: For more insight into where <code>BeforeAndAfterEach</code> fits into the big picture, see the </em>
 * <a href="FlatSpec.html#sharedFixtures">Shared fixtures</a></em> section in the documentation for your chosen style trait.
 * </td></tr></table>
 * 
 * <p>
 * A test <em>fixture</em> is composed of the objects and other artifacts (files, sockets, database
 * connections, <em>etc.</em>) tests use to do their work.
 * When multiple tests need to work with the same fixtures, it is important to try and avoid
 * duplicating the fixture code across those tests. The more code duplication you have in your
 * tests, the greater drag the tests will have on refactoring the actual production code, and 
 * the slower your compile will likely be.
 * Trait <code>BeforeAndAfterEach</code> offers one way to eliminate such code duplication:
 * a <code>beforeEach</code> method that will be run before each test (like JUnit's <code>setUp</code>),
 * and an <code>afterEach</code> method that will be run after (like JUnit's <code>tearDown</code>).
 * </p>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.flatspec.composingbeforeandaftereach
 * 
 * import org.scalatest._
 * import collection.mutable.ListBuffer
 * 
 * trait Builder extends BeforeAndAfterEach { this: Suite =&gt;
 * 
 *   val builder = new StringBuilder
 * 
 *   override def beforeEach() {
 *     builder.append("ScalaTest is ")
 *     super.beforeEach() // To be stackable, must call super.beforeEach
 *   }
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       builder.clear()
 *     }
 *   }
 * }
 * 
 * trait Buffer extends BeforeAndAfterEach { this: Suite =&gt;
 * 
 *   val buffer = new ListBuffer[String]
 * 
 *   override def afterEach() {
 *     try {
 *       super.afterEach() // To be stackable, must call super.afterEach
 *     }
 *     finally {
 *       buffer.clear()
 *     }
 *   }
 * }
 * 
 * class ExampleSpec extends FlatSpec with Builder with Buffer {
 * 
 *   "Testing" should "be easy" in {
 *     builder.append("easy!")
 *     assert(builder.toString === "ScalaTest is easy!")
 *     assert(buffer.isEmpty)
 *     buffer += "sweet"
 *   }
 * 
 *   it should "be fun" in {
 *     builder.append("fun!")
 *     assert(builder.toString === "ScalaTest is fun!")
 *     assert(buffer.isEmpty)
 *     buffer += "clear"
 *   }
 * }
 * </pre>
 *
 * <p>
 * To get the same ordering as <code>withFixture</code>, place your <code>super.beforeEach</code> call at the end of each
 * <code>beforeEach</code> method, and the <code>super.afterEach</code> call at the beginning of each <code>afterEach</code>
 * method, as shown in the previous example. It is a good idea to invoke <code>super.afterEach</code> in a <code>try</code>
 * block and perform cleanup in a <code>finally</code> clause, as shown in the previous example, because this ensures the
 * cleanup code is performed even if <code>super.afterEach</code> throws an exception.
 * </p>
 *
 * <p>
 * The main advantage of <code>BeforeAndAfterEach</code> over <code>BeforeAndAfter</code> is that <code>BeforeAndAfterEach</code>.
 * enables trait stacking.
 * The main disadvantage of <code>BeforeAndAfterEach</code> compared to <code>BeforeAndAfter</code> is that <code>BeforeAndAfterEach</code>
 * requires more boilerplate. If you don't need trait stacking, use <a href="BeforeAndAfter.html"><code>BeforeAndAfter</code></a> instead
 * of <code>BeforeAndAfterEach</code>.
 * If you want to make use of test data (the test name, config map, <em>etc.</em>) in your <code>beforeEach</code>
 * or <code>afterEach</code> method, use trait <a href="BeforeAndAfterEachTestData.html"><code>BeforeAndAfterEachTestData</code></a> instead.
 * </p>
 *
 * @author Bill Venners
 */
trait BeforeAndAfterEach extends SuiteMixin {

  this: Suite =>

  /**
   * Defines a method to be run before each of this suite's tests.
   *
   * <p>
   * This trait's implementation
   * of <code>runTest</code> invokes the overloaded form of this method that
   * takes a <code>configMap</code> before running
   * each test. This trait's implementation of that <code>beforeEach(Map[String, Any])</code> method simply invokes this
   * <code>beforeEach()</code> method. Thus this method can be used to set up a test fixture
   * needed by each test, when you don't need anything from the <code>configMap</code>.
   * This trait's implementation of this method does nothing.
   * </p>
   */
  protected def beforeEach() = ()

  /**
   * Defines a method to be run after each of this suite's tests.
   *
   * <p>
   * This trait's implementation
   * of <code>runTest</code> invokes the overloaded form of this method that
   * takes a <code>configMap</code> map after running
   * each test. This trait's implementation of that <code>afterEach(Map[String, Any])</code> method simply invokes this
   * <code>afterEach()</code> method. Thus this method can be used to tear down a test fixture
   * needed by each test, when you don't need anything from the <code>configMap</code>.
   * This trait's implementation of this method does nothing.
   * </p>
   */
  protected def afterEach() = ()

  /**
   * Run a test surrounded by calls to <code>beforeEach</code> and <code>afterEach</code>.
   *
   * <p>
   * This trait's implementation of this method ("this method") invokes
   * <code>beforeEach(configMap)</code>
   * before running each test and <code>afterEach(configMap)</code>
   * after running each test. It runs each test by invoking <code>super.runTest</code>, passing along
   * the two parameters passed to it.
   * </p>
   * 
   * <p>
   * If any invocation of <code>beforeEach</code> completes abruptly with an exception, this
   * method will complete abruptly with the same exception. If any call to
   * <code>super.runTest</code> completes abruptly with an exception, this method
   * will complete abruptly with the same exception, however, before doing so, it will
   * invoke <code>afterEach</code>. If <cod>afterEach</code> <em>also</em> completes abruptly with an exception, this
   * method will nevertheless complete abruptly with the exception previously thrown by <code>super.runTest</code>.
   * If <code>super.runTest</code> returns normally, but <code>afterEach</code> completes abruptly with an
   * exception, this method will complete abruptly with the exception thrown by <code>afterEach</code>.
   * </p>
   *
   * @param testName the name of one test to run.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
  */
/*
  abstract protected override def runTest(testName: String, args: Args): Status = {

    var thrownException: Option[Throwable] = None

    if (!args.runTestInNewInstance) beforeEach()
    try {
      super.runTest(testName, args)
    }
    catch {
      case e: Exception => 
        thrownException = Some(e)
        FailedStatus
    }
    finally {
      try {
        if (!args.runTestInNewInstance) afterEach() // Make sure that afterEach is called even if runTest completes abruptly.
        thrownException match {
          case Some(e) => throw e
          case None =>
        }
      }
      catch {
        case laterException: Exception =>
          thrownException match { // If both run and afterAll throw an exception, report the test exception
            case Some(earlierException) => throw earlierException
            case None => throw laterException
          }
      }
    }
  }
*/
  abstract protected override def runTest(testName: String, args: Args): Status = {

    var thrownException: Option[Throwable] = None

    if (!args.runTestInNewInstance) beforeEach()
    val runTestStatus: Status =
      try {
        super.runTest(testName, args)
      }
      catch {
        case e: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(e) =>
          thrownException = Some(e)
          FailedStatus
      }
    // And if the exception should cause an abort, abort the afterAll too. (TODO: Update the Scaladoc.)
    try {
      val statusToReturn: Status =
        if (!args.runTestInNewInstance) {
          println ("\n@@@@@@@@@@@@@@@@@@@ TEST NAME: " + testName)
          println ("@@@@@@@@@@@@@@@@@@@ ABOUT TO ADD THE AFTER EFFECT")
          runTestStatus withAfterEffect {
            println("@@@@@@@@@@@@@@@@@@@ RUNNING THE AFTER EFFECT")
            try {
              afterEach()
              println("@@@@@@@@@@@@@@@@@@@ afterEach RETURNED NORMALLY")
              None
            }
            catch { 
              case e: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(e) =>
println("@@@@@@@@@@@@@@@@@@@ GOT HERE WITH EX: " + e.getClass.getName)
                Some(e)
            }
          } // Make sure that afterEach is called even if runTest completes abruptly.
        }
        else
          runTestStatus
      thrownException match {
        case Some(e) => throw e
        case None =>
      }
      statusToReturn
    }
    catch {
      case laterException: Exception =>
        thrownException match { // If both run and afterAll throw an exception, report the test exception
          case Some(earlierException) => throw earlierException
          case None => throw laterException
        }
    }
  }
}
