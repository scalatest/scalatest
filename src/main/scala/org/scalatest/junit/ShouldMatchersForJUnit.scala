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
package org.scalatest.junit

import org.scalatest._
import _root_.junit.framework.AssertionFailedError
import org.scalatest.matchers.ShouldMatchers

/**
 * Trait that makes ScalaTest's <code>ShouldMatchers</code> DSL syntax available for use with JUnit.
 *
 * <p>
 * The assertion methods provided in this trait look and behave exactly like the ones in
 * <a href="../matchers/ShouldMatchers.html"><code>ShouldMatchers</code></a>, except instead of throwing
 * <a href="../TestFailedException.html"><code>TestFailedException</code></a> they throw
 * <a href="JUnitTestFailedError.html"><code>JUnitTestFailedError</code></a>,
 * which extends <code>junit.framework.AssertionFailedError</code>.
 *
 * <p>
 * JUnit 3 (release 3.8 and earlier) distinguishes between <em>failures</em> and <em>errors</em>.
 * If a test fails because of a failed assertion, that is considered a <em>failure</em>. If a test
 * fails for any other reason, either the test code or the application being tested threw an unexpected
 * exception, that is considered an <em>error</em>. The way JUnit 3 decides whether an exception represents
 * a failure or error is that only thrown <code>junit.framework.AssertionFailedError</code>s are considered
 * failures. Any other exception type is considered an error. The exception type thrown by the JUnit 3
 * assertion methods declared in <code>junit.framework.Assert</code> (such as <code>assertEquals</code>,
 * <code>assertTrue</code>, and <code>fail</code>) is, therefore, <code>AssertionFailedError</code>.
 * </p>
 *
 * <p>
 * In JUnit 4, <code>AssertionFailedError</code> was made to extend <code>java.lang.AssertionError</code>,
 * and the distinction between failures and errors was essentially dropped. However, some tools that integrate
 * with JUnit carry on this distinction, so even if you are using JUnit 4 you may want to use this
 * <code>ShouldMatchersForJUnit</code> trait instead of plain-old ScalaTest
 * <a href="../matchers/ShouldMatchers.html"><code>ShouldMatchers</code></a>.
 * </p>
 *
 * <p>
 * To use this trait in a JUnit 3 <code>TestCase</code>, you can mix it into your <code>TestCase</code> class, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import junit.framework.TestCase
 * import org.scalatest.junit.ShouldMatchersForJUnit
 *
 * class MyTestCase extends TestCase with ShouldMatchersForJUnit {
 *
 *   def testSomething() {
 *     "hello, world!" should startWith ("hello")
 *   }
 *
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * You can alternatively import the methods defined in this trait.
 * </p>
 *
 * <pre class="stHighlight">
 * import junit.framework.TestCase
 * import org.scalatest.junit.ShouldMatchersForJUnit._
 *
 * class MyTestCase extends TestCase {
 *
 *   def testSomething() {
 *     "hello, world!" should startWith ("hello")
 *   }
 *
 *   // ...
 * }
 * </pre>
 *
 * <p>
 * For details on the importing approach, see the documentation
 * for the <a href="ShouldMatchersForJUnit$.html"><code>ShouldMatchersForJUnit</code> companion object</a>.
 * For the details on the <code>ShouldMatchersForJUnit</code> syntax, see the Scaladoc documentation for
 * <a href="../matchers/ShouldMatchers.html"><code>org.scalatest.matchers.ShouldMatchers</code></a>
 * </p>
 *
 * @author Bill Venners
 */
trait ShouldMatchersForJUnit extends ShouldMatchers with AssertionsForJUnit {
  private[scalatest] override def newTestFailedException(message: String, optionalCause: Option[Throwable] = None, stackDepthAdjustment: Int = 0): Throwable = {
    val fileNames = List("Matchers.scala", "ShouldMatchers.scala", "MustMatchers.scala", "ShouldMatchersForJUnit.scala", "MustMatchersForJUnit.scala")
    val temp = new RuntimeException
    val stackDepth = temp.getStackTrace.takeWhile(stackTraceElement => fileNames.exists(_ == stackTraceElement.getFileName) || stackTraceElement.getMethodName == "newTestFailedException").length

    optionalCause match {
      case Some(cause) => new JUnitTestFailedError(message, cause, stackDepth)
      case None => new JUnitTestFailedError(message, stackDepth)
    }
  }
}

/**
 * Companion object that facilitates the importing of <code>ShouldMatchersForJUnit</code> members as 
 * an alternative to mixing it in. One use case is to import <code>ShouldMatchersForJUnit</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre>
 * Macintosh-65:delus bv$ scala -cp .:../target/jar_contents:junit3.8.2/junit.jar
 * Welcome to Scala version 2.7.5.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * 
 * scala> import org.scalatest.junit.ShouldMatchersForJUnit._
 * import org.scalatest.junit.ShouldMatchersForJUnit._
 * 
 * scala> "hi" should have length (3)
 * junit.framework.AssertionFailedError: "hi" did not have length 3
 * 	at org.scalatest.junit.ShouldMatchersForJUnit$class.newTestFailedException(ShouldMatchersForJUnit.scala:22)
 * 	at org.scalatest.junit.ShouldMatchersForJUnit$.newTestFailedException(ShouldMatchersForJUnit.scala:63)
 * 	at org.scalatest.matchers.Matchers$ResultOfHaveWordForString.length(Matchers.scala:4102)
 * 	at .<init>(<co...
 * scala> 1 should equal (2)
 * junit.framework.AssertionFailedError: 1 did not equal 2
 * 	at org.scalatest.junit.ShouldMatchersForJUnit$class.newTestFailedException(ShouldMatchersForJUnit.scala:22)
 * 	at org.scalatest.junit.ShouldMatchersForJUnit$.newTestFailedException(ShouldMatchersForJUnit.scala:63)
 * 	at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.shouldMatcher(ShouldMatchers.scala:800)
 * 	at org.scal...
 * scala> "hello, world" should startWith ("hello")
 * 
 * scala> 7 should (be >= (3) and not be <= (7))
 * junit.framework.AssertionFailedError: 7 was greater than or equal to 3, but 7 was less than or equal to 7
 * 	at org.scalatest.junit.ShouldMatchersForJUnit$class.newTestFailedException(ShouldMatchersForJUnit.scala:22)
 * 	at org.scalatest.junit.ShouldMatchersForJUnit$.newTestFailedException(ShouldMatchersForJUnit.scala:63)
 * 	at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.sh...
 * </pre>
 *
 * @author Bill Venners
 */
object ShouldMatchersForJUnit extends ShouldMatchersForJUnit
