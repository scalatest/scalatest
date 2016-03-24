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

import org.scalactic.{Prettifier, SourceInfo}
import scala.collection.mutable.ListBuffer
import collection.immutable.TreeMap
import org.scalactic.Requirements._

/**
 * Sub-trait of <code>Assertions</code> that override <code>assert</code> and <code>assume</code> methods to include
 * a diagram showing the values of expression in the error message when the assertion or assumption fails.
 *
 * Here are some examples:
 *
 * <pre class="stREPL">
 * scala&gt; import DiagrammedAssertions._
 * import DiagrammedAssertions._
 * 
 * scala&gt; assert(a == b || c &gt;= d)
 * org.scalatest.exceptions.TestFailedException:
 * 
 * assert(a == b || c &gt;= d)
 *        | |  | |  | |  |
 *        1 |  2 |  3 |  4
 *          |    |    false
 *          |    false
 *          false
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * scala&gt; assert(xs.exists(_ == 4))
 * org.scalatest.exceptions.TestFailedException:
 * 
 * assert(xs.exists(_ == 4))
 *        |  |
 *        |  false
 *        List(1, 2, 3)
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * scala&gt; assert("hello".startsWith("h") &amp;&amp; "goodbye".endsWith("y"))
 * org.scalatest.exceptions.TestFailedException:
 * 
 * assert("hello".startsWith("h") &amp;&amp; "goodbye".endsWith("y"))
 *        |       |          |    |  |         |        |
 *        "hello" true       "h"  |  "goodbye" false    "y"
 *                                false
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * scala&gt; assert(num.isInstanceOf[Int])
 * org.scalatest.exceptions.TestFailedException:
 * 
 * assert(num.isInstanceOf[Int])
 *        |   |
 *        1.0 false
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * scala&gt; assert(Some(2).isEmpty)
 * org.scalatest.exceptions.TestFailedException:
 * 
 * assert(Some(2).isEmpty)
 *        |    |  |
 *        |    2  false
 *        Some(2)
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * scala&gt; assert(None.isDefined)
 * org.scalatest.exceptions.TestFailedException:
 * 
 * assert(None.isDefined)
 *        |    |
 *        None false
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * scala&gt; assert(xs.exists(i =&gt; i &gt; 10))
 * org.scalatest.exceptions.TestFailedException:
 * 
 * assert(xs.exists(i =&gt; i &gt; 10))
 *        |  |
 *        |  false
 *        List(1, 2, 3)
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * </pre>
 *
 * <p>
 * If the expression passed to <code>assert</code> or <code>assume</code> spans more than one line, <code>DiagrammedAssertions</code> falls
 * back to the default style of error message, since drawing a diagram would be difficult. Here's an example showing how
 * <code>DiagrammedAssertions</code> will treat a multi-line assertion (<em>i.e.</em>, you don't get a diagram):
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; assert("hello".startsWith("h") &amp;&amp;
 *      |   "goodbye".endsWith("y"))
 * org.scalatest.exceptions.TestFailedException: "hello" started with "h", but "goodbye" did not end with "y"
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * </pre>
 * 
 * <p>
 * Also, since an expression diagram essentially represents multi-line ascii art, if a clue string is provided, it appears <em>above</em> the diagram, not after it. It will often also show up in the diagram:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; assert(None.isDefined, "Don't do this at home")
 * org.scalatest.exceptions.TestFailedException: Don't do this at home
 * 
 * assert(None.isDefined, "Don't do this at home")
 *        |    |
 *        None false
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * 
 * scala&gt; assert(None.isDefined,
 *      |   "Don't do this at home")
 * org.scalatest.exceptions.TestFailedException: Don't do this at home
 * 
 * assert(None.isDefined,
 *        |    |
 *        None false
 * 
 *         at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 *         ...
 * </pre>
 *
 * <p>Trait <code>DiagrammedAssertions</code> was inspired by Peter Niederwieser's work in <a href="http://code.google.com/p/spock/">Spock</a> and <a href="https://github.com/pniederw/expecty">Expecty</a>.
 */
trait DiagrammedAssertions extends Assertions {

  import language.experimental.macros

  /**
   * Helper class used by code generated by the overriden <code>assert</code> macro.
   */
  class DiagrammedAssertionsHelper {

    // this is taken from expecty
    private[this] def fits(line: StringBuilder, str: String, anchor: Int): Boolean =
      line.slice(anchor, anchor + str.length + 1).forall(_.isWhitespace)

    // this is taken from expecty
    private[this] def placeString(line: StringBuilder, str: String, anchor: Int) {
      val diff = anchor - line.length
      for (i <- 1 to diff) line.append(' ')
      if (line.length == anchor)
        line.append(str)
      else
        line.replace(anchor, anchor + str.length(), str)
    }

    // this is taken from expecty and modified
    private[this] def renderValue(value: Any): String = {
      value match {
        case aEqualizer: org.scalactic.TripleEqualsSupport#Equalizer[_] => Prettifier.default(aEqualizer.leftSide)
        case aEqualizer: org.scalactic.TripleEqualsSupport#CheckingEqualizer[_] => Prettifier.default(aEqualizer.leftSide)
        case _ => Prettifier.default(value)
      }
    }

    // this is taken from expecty
    private[this] def placeValue(lines: ListBuffer[StringBuilder], value: Any, col: Int) {
      val str = renderValue(value)

      placeString(lines(0), "|", col)

      for (line <- lines.drop(1)) {
        if (fits(line, str, col)) {
          placeString(line, str, col)
          return
        }
        placeString(line, "|", col)
      }

      val newLine = new StringBuilder()
      placeString(newLine, str, col)
      lines.append(newLine)
    }

    // this is taken from expecty
    private[this] def filterAndSortByAnchor(anchorValues: List[AnchorValue]): Traversable[AnchorValue] = {
      var map = TreeMap[Int, AnchorValue]()(Ordering.by(-_))
      // values stemming from compiler generated code often have the same anchor as regular values
      // and get recorded before them; let's filter them out
      for (value <- anchorValues) if (!map.contains(value.anchor)) map += (value.anchor -> value)
      map.values
    }

    // this is taken from expecty
    private[this] def renderDiagram(sourceText: String, anchorValues: List[AnchorValue]): String = {
      val offset = sourceText.prefixLength(_.isWhitespace)
      val intro = new StringBuilder().append(sourceText.trim())
      val lines = ListBuffer(new StringBuilder)

      val rightToLeft = filterAndSortByAnchor(anchorValues)
      for (anchorValue <- rightToLeft) placeValue(lines, anchorValue.value, anchorValue.anchor - offset)

      lines.prepend(intro)
      lines.append(new StringBuilder)
      lines.mkString(Prettifier.lineSeparator)
    }

    /**
     * Assert that the passed in <code>Bool</code> is <code>true</code>, else fail with <code>TestFailedException</code>
     * with error message that include a diagram showing expression values.
     *
     * @param bool the <code>Bool</code> to assert for
     * @param clue optional clue to be included in <code>TestFailedException</code>'s error message when assertion failed
     */
    def macroAssert(bool: DiagrammedExpr[Boolean], clue: Any, sourceText: String, sourceInfo: SourceInfo): Assertion = {
      requireNonNull(clue)
      if (!bool.value) {
        val failureMessage =
          Some(clue + Prettifier.lineSeparator + Prettifier.lineSeparator + renderDiagram(sourceText, bool.anchorValues))
        throw newAssertionFailedException(failureMessage, None, sourceInfo)
      }
      Succeeded
    }

    /**
     * Assume that the passed in <code>Bool</code> is <code>true</code>, else throw <code>TestCanceledException</code>
     * with error message that include a diagram showing expression values.
     *
     * @param bool the <code>Bool</code> to assume for
     * @param clue optional clue to be included in <code>TestCanceledException</code>'s error message when assertion failed
     */
    def macroAssume(bool: DiagrammedExpr[Boolean], clue: Any, sourceText: String, sourceInfo: SourceInfo): Assertion = {
      requireNonNull(clue)
      if (!bool.value) {
        val failureMessage =
          Some(clue + Prettifier.lineSeparator + Prettifier.lineSeparator + renderDiagram(sourceText, bool.anchorValues))
        throw newTestCanceledException(failureMessage, None, sourceInfo)
      }
      Succeeded
    }
  }

  /**
   * Helper instance used by code generated by the overriden macro assertion.
   */
  val diagrammedAssertionsHelper = new DiagrammedAssertionsHelper

  /**
   * Assert that a boolean condition is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code>.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message that includes
   * a diagram showing expression values.
   * </p>
   *
   * <p>
   * If multi-line <code>Boolean</code> is passed in, it will fallback to the macro implementation of <code>Assertions</code>
   * that does not contain diagram.
   * </p>
   *
   * @param condition the boolean condition to assert
   * @throws TestFailedException if the condition is <code>false</code>.
   */
  override def assert(condition: Boolean)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Assertion = macro DiagrammedAssertionsMacro.assert

  /**
   * Assert that a boolean condition, described in <code>String</code>
   * <code>message</code>, is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestFailedException</code> with the
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> as the exception's detail message and a
   * diagram showing expression values.
   *
   * <p>
   * If multi-line <code>Boolean</code> is passed in, it will fallback to the macro implementation of <code>Assertions</code>
   * that does not contain diagram.
   * </p>
   *
   * @param condition the boolean condition to assert
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestFailedException if the condition is <code>false</code>.
   * @throws NullArgumentException if <code>message</code> is <code>null</code>.
   */
  override def assert(condition: Boolean, clue: Any)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Assertion = macro DiagrammedAssertionsMacro.assertWithClue

  /**
   * Assume that a boolean condition is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestCanceledException</code>.
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate a more helpful error message that includes
   * a diagram showing expression values.
   * </p>
   *
   * <p>
   * If multi-line <code>Boolean</code> is passed in, it will fallback to the macro implementation of <code>Assertions</code>
   * that does not contain diagram.
   * </p>
   *
   * @param condition the boolean condition to assume
   * @throws TestCanceledException if the condition is <code>false</code>.
   */
  override def assume(condition: Boolean)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Assertion = macro DiagrammedAssertionsMacro.assume

  /**
   * Assume that a boolean condition, described in <code>String</code>
   * <code>message</code>, is true.
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>TestCanceledException</code> with the
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> as the exception's detail message and a
   * diagram showing expression values.
   *
   * <p>
   * If multi-line <code>Boolean</code> is passed in, it will fallback to the macro implementation of <code>Assertions</code>
   * that does not contain diagram.
   * </p>
   *
   * @param condition the boolean condition to assume
   * @param clue An objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws TestCanceledException if the condition is <code>false</code>.
   * @throws NullArgumentException if <code>message</code> is <code>null</code>.
   */
  override def assume(condition: Boolean, clue: Any)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Assertion = macro DiagrammedAssertionsMacro.assumeWithClue
}

/**
 * Companion object that facilitates the importing of <code>DiagrammedAssertions</code> members as
 * an alternative to mixing it in. One use case is to import <code>DiagrammedAssertions</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.10.4.final (Java HotSpot(TM) Client VM, Java 1.6.0_45).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * &nbsp;
 * scala&gt; import org.scalatest.Assertions._
 * import org.scalatest.Assertions._
 * &nbsp;
 * scala&gt; assert(1 === 2)
 * org.scalatest.exceptions.TestFailedException:
 *
 * assert(1 === 2)
 *        | |   |
 *        1 |   2
 *          false
 *
 *      at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:422)
 * 	    at org.scalatest.DiagrammedAssertions$.newAssertionFailedException(DiagrammedAssertions.scala:249)
 * 	    at org.scalatest.DiagrammedAssertions$DiagrammedAssertionsHelper.macroAssert(DiagrammedAssertions.scala:111)
 * 	    at .&lt;init&gt;(&lt;console&gt;:20)
 * 	    at .&lt;clinit&gt;(&lt;console&gt;)
 * 	    at .&lt;init&gt;(&lt;console&gt;:7)
 * 	    at .&lt;clinit&gt;(&lt;console&gt;)
 *  	  at $print(&lt;console&gt;)
 * 	    at sun.reflect.NativeMethodAccessorImpl.invoke...
 * </pre>
 */
object DiagrammedAssertions extends DiagrammedAssertions
