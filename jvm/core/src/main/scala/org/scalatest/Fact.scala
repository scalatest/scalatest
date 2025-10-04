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

import org.scalactic.{UnquotedString => _, _}
import org.scalatest.exceptions._

/**
 * Represents the result of an assertion as a value that is either "yes" (true) or "no" (false).
 *
 * <p>
 * A <code>Fact</code> is a composable alternative to traditional assertions that throw exceptions.
 * Rather than immediately throwing <code>TestFailedException</code> on failure, a <code>Fact</code>
 * carries information about whether an assertion succeeded or failed, along with detailed messages
 * explaining the result.
 * </p>
 *
 * <p>
 * <code>Fact</code>s are created by the expectation methods in trait
 * <a href="expectations/Expectations.html"><code>Expectations</code></a>, such as
 * <code>expect</code>, <code>expectResult</code>, and <code>expectThrows</code>. You can compose
 * <code>Fact</code>s using logical operators like <code>&&</code>, <code>||</code>, and <code>!</code>,
 * and convert them to traditional <code>Assertion</code>s by calling <code>toAssertion</code>.
 * </p>
 *
 * <a name="yesAndNo"></a>
 * <h2>Yes and No</h2>
 *
 * <p>
 * Every <code>Fact</code> is either a "yes" (<code>isYes</code> returns <code>true</code>) indicating
 * the assertion passed, or a "no" (<code>isNo</code> returns <code>true</code>) indicating it failed.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.expectations.Expectations._
 *
 * val fact1 = expect(2 + 2 == 4)
 * fact1.isYes  // true
 *
 * val fact2 = expect(2 + 2 == 5)
 * fact2.isNo   // true
 * </pre>
 *
 * <p>
 * The <code>isNo</code> method is defined as <code>!isYes</code>, so every <code>Fact</code> is
 * always either yes or no, never both or neither.
 * </p>
 *
 * <a name="composingFacts"></a>
 * <h2>Composing Facts</h2>
 *
 * <p>
 * You can combine <code>Fact</code>s using logical operators to build complex assertions.
 * <code>Fact</code> provides both short-circuiting and strict evaluation operators:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Operator
 *   </th>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Description
 *   </th>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Evaluation
 *   </th>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>&&</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Logical AND
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Short-circuits (right not evaluated if left is no)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>&amp;</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Logical AND
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Strict (always evaluates both sides)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>||</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Logical OR
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Short-circuits (right not evaluated if left is yes)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>|</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Logical OR
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Strict (always evaluates both sides)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>!</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Logical NOT
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Negates the fact
 *   </td>
 * </tr>
 * </table>
 *
 * <p>
 * Here's an example showing composition:
 * </p>
 *
 * <pre class="stHighlight">
 * val x = 5
 * val fact1 = expect(x &gt; 0)
 * val fact2 = expect(x &lt; 10)
 * val both = fact1 && fact2
 * both.isYes  // true, because both facts are yes
 *
 * val fact3 = expect(x &gt; 10)
 * val either = fact1 || fact3
 * either.isYes  // true, because at least one fact is yes
 *
 * val negated = !fact3
 * negated.isYes  // true, because fact3 was no
 * </pre>
 *
 * <p>
 * The short-circuiting operators (<code>&&</code> and <code>||</code>) take by-name parameters,
 * so the right-hand side is only evaluated when needed. This can prevent errors when the
 * right-hand assertion would fail if the left-hand condition doesn't hold. For example:
 * </p>
 *
 * <pre class="stHighlight">
 * val nums = List.empty[Int]
 * val fact = expect(nums.nonEmpty) && expect(nums.head &gt; 0)
 * // fact.isNo is true, but nums.head was never evaluated
 * </pre>
 *
 * <a name="implication"></a>
 * <h2>Implication</h2>
 *
 * <p>
 * The <code>implies</code> method represents logical implication (if premise then consequent).
 * It is particularly useful for conditional assertions in property-based testing, where you
 * want to test a property only when certain preconditions hold.
 * </p>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * def validateScore(score: Int): Fact = {
 *   val isPositive = expect(score &gt; 0)
 *   val isReasonable = expect(score &lt;= 100)
 *   isPositive implies isReasonable
 * }
 * </pre>
 *
 * <p>
 * The <code>implies</code> method follows the truth table of logical implication:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Premise
 *   </th>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Consequent
 *   </th>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Result
 *   </th>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes (meaningful success)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No (real failure)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     VacuousYes (vacuously true)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     VacuousYes (vacuously true)
 *   </td>
 * </tr>
 * </table>
 *
 * <p>
 * When the premise is no, <code>implies</code> returns a <code>VacuousYes</code> without
 * evaluating the consequent. This follows the principle of classical logic that "false implies
 * anything" (<em>ex falso quodlibet</em>).
 * </p>
 *
 * <a name="vacuousYes"></a>
 * <h2>Vacuous Yes</h2>
 *
 * <p>
 * A <code>VacuousYes</code> is a special kind of yes that indicates an assertion was technically
 * true, but didn't test anything meaningful. This occurs when using <code>implies</code> with a
 * false premise, as shown in the example above.
 * </p>
 *
 * <p>
 * You can check whether a <code>Fact</code> is vacuous by calling <code>isVacuousYes</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * val score = -5
 * val fact = expect(score &gt; 0) implies expect(score &lt;= 100)
 * fact.isYes         // true (vacuously)
 * fact.isVacuousYes  // true (because premise was false)
 * </pre>
 *
 * <p>
 * When converted to an <code>Assertion</code>, a <code>VacuousYes</code> throws
 * <code>TestCanceledException</code> to indicate the test was skipped because its precondition
 * wasn't met:
 * </p>
 *
 * <pre class="stHighlight">
 * fact.toAssertion  // throws TestCanceledException
 * </pre>
 *
 * <p>
 * This behavior is useful in property-based testing, where you want to distinguish between:
 * </p>
 *
 * <ul>
 * <li>Tests that passed meaningfully (Yes)</li>
 * <li>Tests that failed (No)</li>
 * <li>Tests that were skipped because preconditions weren't met (VacuousYes)</li>
 * </ul>
 *
 * <a name="equivalence"></a>
 * <h2>Equivalence</h2>
 *
 * <p>
 * The <code>isEqvTo</code> method tests whether two <code>Fact</code>s have the same truth value,
 * representing logical equivalence (if and only if).
 * </p>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val x = 0
 * val fact1 = expect(x == 0)
 * val fact2 = expect(x &gt;= 0 && x &lt;= 0)
 * val equiv = fact1 isEqvTo fact2
 * equiv.isYes  // true, because both facts have the same truth value
 * </pre>
 *
 * <p>
 * The <code>isEqvTo</code> method follows the truth table of logical equivalence:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Left
 *   </th>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Right
 *   </th>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Result
 *   </th>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes (both true)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No (different truth values)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No (different truth values)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     No
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Yes (both false)
 *   </td>
 * </tr>
 * </table>
 *
 * <p>
 * If either <code>Fact</code> is a <code>VacuousYes</code>, the result will also be a
 * <code>VacuousYes</code> (when the result would otherwise be yes).
 * </p>
 *
 * <a name="messages"></a>
 * <h2>Fact Messages</h2>
 *
 * <p>
 * Every <code>Fact</code> carries detailed messages that explain the assertion result. These
 * messages are used when converting a <code>Fact</code> to an <code>Assertion</code> or when
 * displaying a <code>Fact</code> diagram.
 * </p>
 *
 * <p>
 * A <code>Fact</code> maintains four different message templates, each serving a specific purpose:
 * </p>
 *
 * <ul>
 * <li><code>rawFactMessage</code> - The primary message, used for top-level assertions</li>
 * <li><code>rawSimplifiedFactMessage</code> - A simplified version, used when the fact is negated</li>
 * <li><code>rawMidSentenceFactMessage</code> - For use mid-sentence in composite fact diagrams</li>
 * <li><code>rawMidSentenceSimplifiedFactMessage</code> - Simplified mid-sentence version</li>
 * </ul>
 *
 * <p>
 * Each message template has corresponding argument collections (<code>factMessageArgs</code>,
 * <code>simplifiedFactMessageArgs</code>, <em>etc.</em>) that are formatted into the templates
 * using the <code>prettifier</code> to produce the final messages.
 * </p>
 *
 * <p>
 * The simplified messages are used when a <code>Fact</code> is negated because the regular
 * message might contain confusing language. For example, "Expected 3, but got 4" makes sense
 * as a failure message, but when negated it would imply we expected something <em>other</em>
 * than 3. The simplified message "3 did not equal 4" works correctly in both cases.
 * </p>
 *
 * <p>
 * You can obtain the formatted messages by calling:
 * </p>
 *
 * <ul>
 * <li><code>factMessage</code> - formats <code>rawFactMessage</code> with <code>factMessageArgs</code></li>
 * <li><code>simplifiedFactMessage</code> - formats <code>rawSimplifiedFactMessage</code> with <code>simplifiedFactMessageArgs</code></li>
 * <li><code>midSentenceFactMessage</code> - formats <code>rawMidSentenceFactMessage</code> with <code>midSentenceFactMessageArgs</code></li>
 * <li><code>midSentenceSimplifiedFactMessage</code> - formats <code>rawMidSentenceSimplifiedFactMessage</code> with <code>midSentenceSimplifiedFactMessageArgs</code></li>
 * </ul>
 *
 * <a name="factDiagrams"></a>
 * <h2>Fact Diagrams</h2>
 *
 * <p>
 * The <code>factDiagram</code> method generates a textual representation of a <code>Fact</code>
 * and its structure. For simple leaf <code>Fact</code>s, this is just the message. For composite
 * <code>Fact</code>s created with operators like <code>&&</code> or <code>||</code>, it shows
 * the tree structure of the assertion.
 * </p>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val x = 3
 * val fact1 = expect(x &gt; 0)
 * val fact2 = expect(x &lt; 2)
 * val both = fact1 && fact2
 * println(both.factDiagram(0))
 * </pre>
 *
 * <p>
 * This will print:
 * </p>
 *
 * <pre class="stHighlight">
 * No(
 *   Yes(3 was greater than 0) &&
 *   No(3 was not less than 2)
 * )
 * </pre>
 *
 * <p>
 * The diagram shows the overall result (<code>No</code>), the operator (<code>&&</code>),
 * and the results of each operand, making it easy to see which part of a complex assertion failed.
 * </p>
 *
 * <a name="conversionToAssertion"></a>
 * <h2>Converting to Assertion</h2>
 *
 * <p>
 * You can convert a <code>Fact</code> to a traditional ScalaTest <code>Assertion</code> by calling
 * <code>toAssertion</code>. The behavior of <code>toAssertion</code> depends on the <code>Fact</code>'s state:
 * </p>
 *
 * <ul>
 * <li>If <code>isYes</code> is <code>true</code> and <code>isVacuousYes</code> is <code>false</code>,
 *     returns <code>Succeeded</code></li>
 * <li>If <code>isVacuousYes</code> is <code>true</code>, throws <code>TestCanceledException</code>
 *     with the fact's message</li>
 * <li>If <code>isNo</code> is <code>true</code>, throws <code>TestFailedException</code> with
 *     the fact's message</li>
 * </ul>
 *
 * <p>
 * This conversion will be applied implicitly if you import <code>org.scalatest.expectations.Expectations._</code>,
 * so long as the expected test result type is <code>Assertion</code>. For example, style traits like
 * <a href="funsuite/AnyFunSuite.html"><code>FunSuite</code></a>,
 * <a href="funspec/AnyFunSpec.html"><code>FunSpec</code></a>, and
 * <a href="featurespec/AnyFeatureSpec.html"><code>FeatureSpec</code></a> expect test result type
 * <code>Assertion</code>. As a result, you need not explicitly call <code>toAssertion</code> when
 * using expectations in these styles. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.funsuite.FunSuite
 * import org.scalatest.expectations.Expectations._
 *
 * class ExampleSuite extends FunSuite {
 *   test("addition") {
 *     val fact = expect(2 + 2 == 4)
 *     // Implicit conversion to Assertion happens here
 *   }
 * }
 * </pre>
 *
 * <p>
 * By contrast, style traits whose names start with <code>Any</code>, such as
 * <a href="funsuite/AnyFunSuite.html"><code>AnyFunSuite</code></a> and
 * <a href="featurespec/AnyFeatureSpec.html"><code>AnyFeatureSpec</code></a>, have <code>Any</code>
 * as the expected test result type. You should use assertions or matchers with these styles, not
 * expectations.
 * </p>
 *
 * <p>
 * You can also convert a <code>Fact</code> to a <code>Boolean</code> by calling <code>toBoolean</code>,
 * which simply returns the value of <code>isYes</code>.
 * </p>
 *
 * <a name="leafAndComposite"></a>
 * <h2>Leaf and Composite Facts</h2>
 *
 * <p>
 * <code>Fact</code>s are organized in a tree structure. A <em>leaf</em> <code>Fact</code> is one
 * created directly by an expectation method like <code>expect</code> or <code>expectResult</code>.
 * A <em>composite</em> <code>Fact</code> is one created by combining other <code>Fact</code>s with
 * operators like <code>&&</code>, <code>||</code>, <code>!</code>, <code>implies</code>, or
 * <code>isEqvTo</code>.
 * </p>
 *
 * <p>
 * You can check whether a <code>Fact</code> is a leaf by calling <code>isLeaf</code>. This is
 * useful when generating fact diagrams or analyzing assertion structure.
 * </p>
 *
 * <p>
 * Leaf <code>Fact</code>s are created using the factory methods in the <a href="Fact$.html">
 * <code>Fact</code> companion object</a>:
 * </p>
 *
 * <ul>
 * <li><code>Fact.Yes</code> - creates a yes leaf fact</li>
 * <li><code>Fact.No</code> - creates a no leaf fact</li>
 * <li><code>Fact.VacuousYes</code> - creates a vacuous yes by wrapping a no fact</li>
 * </ul>
 *
 * <a name="modifyingMessages"></a>
 * <h2>Modifying Messages</h2>
 *
 * <p>
 * The <code>modifyMessage</code> method allows you to transform a <code>Fact</code>'s messages
 * using a function. This is useful for adding clues or context to assertion messages.
 * </p>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val fact = expect(x &gt; 0)
 * val withClue = fact.modifyMessage { msgOpt =&gt;
 *   msgOpt.map(msg =&gt; s"$msg (while validating user input)")
 * }
 * </pre>
 *
 * <p>
 * The function receives an <code>Option[String]</code> containing the current message (or
 * <code>None</code> if there is no message) and returns an <code>Option[String]</code> with
 * the modified message.
 * </p>
 *
 * @author Bill Venners
 */
sealed abstract class Fact {
  // As it stands, this should not extend Product with Serializable because
  // subclasses exists that anen't case classes.

  /**
   * The raw message representing the fact.
   */
  val rawFactMessage: String
  /**
   * The raw simplified message representing the fact.
   */
  val rawSimplifiedFactMessage: String
  /**
   * The raw mid-sentence message representing the fact.
   */
  val rawMidSentenceFactMessage: String
  /**
   * The raw mid-sentence simplified message representing the fact.
   */
  val rawMidSentenceSimplifiedFactMessage: String
  /**
   * Arguments used to format the rawFactMessage.
   */
  val factMessageArgs: IndexedSeq[Any]
  /**
   * Arguments used to format the rawSimplifiedFactMessage.
   */
  val simplifiedFactMessageArgs: IndexedSeq[Any]
  /**
   * Arguments used to format the rawMidSentenceFactMessage.
   */
  val midSentenceFactMessageArgs: IndexedSeq[Any]
  /**
   * Arguments used to format the rawMidSentenceSimplifiedFactMessage.
   */
  val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any]
  /**
   * Indicates whether the fact is a leaf (terminal) node in a fact tree.
   */
  val isLeaf: Boolean
  /**
   * Indicates whether the fact is a vacuous yes, which means true in a sense but without meaningful assertions.
   */
  val isVacuousYes: Boolean
  /**
   * A prettifier used to format the messages when constructing failure messages.
   */
  val prettifier: Prettifier
  /**
   * An optional cause Throwable associated with the fact.
   */
  val cause: Option[Throwable] = None
  /**
   * Indicates whether the fact is a yes.
   */
  val isYes: Boolean
  /**
   * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
   */
  def modifyMessage(fun: Option[String] => Option[String]): Fact
  /**
   * Indicates whether the fact is a no.
   */
  final def isNo: Boolean = !isYes

  /**
   * Convert the fact to <code>Boolean</code>, same as the value returned by <code>isYes</code>.
   */
  final def toBoolean: Boolean = isYes

  /**
   * Convert this fact to <code>Assertion</code>.
   *
   * @param pos the related <code>Position</code> for this fact.
   * @return <code>Succeeded</code> if this fact is a yes.
   * @throws TestFailedException if this fact is not yes or vacuous yes.
   * @throws TestCanceledException if this fact is a vacuous yes.
   */
  final def toAssertion(implicit pos: source.Position): Assertion = {
    if (isYes) {
      if (!isVacuousYes) Succeeded
      else throw new TestCanceledException((e: StackDepthException) => Some(factMessage), None, pos, None)
    }
    else throw new TestFailedException((e: StackDepthException) => Some(factMessage), None, pos)
  }

  // This is called internally by implicit conversions, which has different stack depth
  private[scalatest] final def internalToAssertion(pos: source.Position): Assertion = {
    if (isYes) {
      if (!isVacuousYes) Succeeded
      else throw new TestCanceledException((e: StackDepthException) => Some(factMessage), None, pos, None)
    }
    else throw new TestFailedException((e: StackDepthException) => Some(factMessage), None, pos)
  }

  /**
   * Negates this Fact, creating a version with the opposite value.
   *
   * @return A version of this Fact with the opposite value.
   */ 
  def unary_! : Fact = Fact.Unary_!(this)

  /**
   * Creates a new Fact that represents a logical OR between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical OR.
   * @return A new Fact representing the logical OR between this Fact and the provided Fact.
   */
  final def ||(rhs: => Fact): Fact = if (isYes) this else Fact.Binary_||(this, rhs)

  /**
   * Creates a new Fact that represents a logical AND between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical AND.
   * @return A new Fact representing the logical AND between this Fact and the provided Fact.
   */
  final def &&(rhs: => Fact): Fact = if (isNo) this else Fact.Binary_&&(this, rhs)

  /**
   * Creates a new Fact that represents a logical OR between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical OR.
   * @return A new Fact representing the logical OR between this Fact and the provided Fact.
   */
  final def |(rhs: Fact): Fact = Fact.Binary_|(this, rhs)

  /**
   * Creates a new Fact that represents a logical AND between this Fact and the provided Fact.
   *
   * @param rhs The Fact to be combined with this Fact using a logical AND.
   * @return A new Fact representing the logical AND between this Fact and the provided Fact.
   */
  final def &(rhs: Fact): Fact = Fact.Binary_&(this, rhs)

  /**
   * String prefix for this fact, return either "Yes", "VacuousYes" or "No".
   */
  final def stringPrefix: String =
    if (isYes) {
      if (isVacuousYes) "VacuousYes" else "Yes"
    }
    else "No"

  /**
   * Creates a new Fact that represents the implication (=>) between this Fact and the provided Fact.
   *
   * @param rhs The Fact representing the implication's consequent.
   * @return A new Fact representing the implication between this Fact and the provided Fact.
   */
  final def implies(rhs: => Fact): Fact = if (isNo) Fact.VacuousYes(this) else Fact.Implies(this, rhs)

  /**
   * Creates a new Fact that represents the equivalence (eqv) between this Fact and the provided Fact.
   *
   * @param rhs The Fact representing the other side of the equivalence.
   * @return A new Fact representing the equivalence between this Fact and the provided Fact.
   */
  final def isEqvTo(rhs: Fact): Fact = Fact.IsEqvTo(this, rhs)

  /**
   * Construct failure message to report if a fact fails, using <code>rawFactMessage</code>, <code>factMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message to report if a fact fails
   */
  def factMessage: String =
    if (factMessageArgs.isEmpty) rawFactMessage
    else makeString(rawFactMessage, factMessageArgs)

  /**
   * Construct simplified failure message to report if a fact fails, using <code>rawSimplifiedFactMessage</code>, <code>simplifiedFactMessageArgs</code>, and <code>prettifier</code>
   *
   * @return simplified failure message to report if a fact fails
   */
  def simplifiedFactMessage: String =
    if (simplifiedFactMessageArgs.isEmpty) rawSimplifiedFactMessage
    else makeString(rawSimplifiedFactMessage, simplifiedFactMessageArgs)

  /**
   * Construct failure message suitable for appearing mid-sentence, using <code>rawMidSentenceFactMessage</code>, <code>midSentenceFactMessageArgs</code> and <code>prettifier</code>
   *
   * @return failure message suitable for appearing mid-sentence
   */
  def midSentenceFactMessage: String =
    if (midSentenceFactMessageArgs.isEmpty) rawMidSentenceFactMessage
    else makeString(rawMidSentenceFactMessage, midSentenceFactMessageArgs)

  /**
   * Construct simplified failure message suitable for appearing mid-sentence, using <code>rawMidSentenceSimplifiedFactMessage</code>, <code>midSentenceSimplifiedFactMessageArgs</code> and <code>prettifier</code>
   *
   * @return simplified failure message suitable for appearing mid-sentence
   */
  def midSentenceSimplifiedFactMessage: String =
    if (midSentenceSimplifiedFactMessageArgs.isEmpty) rawMidSentenceSimplifiedFactMessage
    else makeString(rawMidSentenceSimplifiedFactMessage, midSentenceSimplifiedFactMessageArgs)

  private def makeString(raw: String, args: IndexedSeq[Any]): String =
    Resources.formatString(raw, args.map(prettifier.apply).toArray)

  private[scalatest] val NEWLINE = System.lineSeparator

  /**
   * Generates a fact diagram, which is a textual representation of the fact and its structure.
   *
   * @param level The indentation level for formatting the diagram.
   * @return The fact diagram as a string.
   */
  def factDiagram(level: Int): String = {
    // This one makes sense for Yes and No only. The other subclassess override it.
    val msg = midSentenceFactMessage // just compute this once
    val padding = "  " * level
    if (msg.contains("\n")) {
      val padding = "  " * (level)
      padding + stringPrefix + "(" + NEWLINE + msg.split("\n").map(line => padding + "  " + line).mkString("\n") + NEWLINE + ")"
    }
    else
      padding + stringPrefix + "(" + msg + ")"
  }

  /**
   * Converts this Fact to its string representation.
   *
   * @return The string representation of this Fact.
   */
  override def toString: String = factDiagram(0)
}

/**
 * Companion object for the <code>Fact</code> trait.
 */
object Fact {

  /**
   * Represents a leaf node in the Fact tree, which is a concrete Fact with known boolean values.
   *
   * @param rawFactMessage The raw message representing the fact.
   * @param rawSimplifiedFactMessage The raw simplified message representing the fact.
   * @param rawMidSentenceFactMessage The raw mid-sentence message representing the fact.
   * @param rawMidSentenceSimplifiedFactMessage The raw mid-sentence simplified message representing the fact.
   * @param factMessageArgs Arguments used to format the rawFactMessage.
   * @param simplifiedFactMessageArgs Arguments used to format the rawSimplifiedFactMessage.
   * @param midSentenceFactMessageArgs Arguments used to format the rawMidSentenceFactMessage.
   * @param midSentenceSimplifiedFactMessageArgs Arguments used to format the rawMidSentenceSimplifiedFactMessage.
   * @param isYes Indicates whether the fact is evaluated to true.
   * @param isVacuousYes Indicates whether the fact is a vacuous yes, which means true in a sense but without meaningful assertions.
   * @param prettifier A prettifier used to format the messages when constructing failure messages.
   * @param cause An optional cause Throwable associated with the fact.
   * @throws IllegalArgumentException if `isVacuousYes` is true but `isYes` is false.
   */
  case class Leaf(
    rawFactMessage: String,
    rawSimplifiedFactMessage: String,
    rawMidSentenceFactMessage: String,
    rawMidSentenceSimplifiedFactMessage: String,
    factMessageArgs: IndexedSeq[Any],
    simplifiedFactMessageArgs: IndexedSeq[Any],
    midSentenceFactMessageArgs: IndexedSeq[Any],
    midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
    isYes: Boolean,
    isVacuousYes: Boolean,
    prettifier: Prettifier,
    override val cause: Option[Throwable] = None
  ) extends Fact {
    require(!isVacuousYes || isYes)

    /**
     * Indicates whether this Fact is a leaf node in the Fact tree, return <code>true</code>.
     */
    val isLeaf: Boolean = true
    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = 
      Leaf(
        fun(Some(rawFactMessage)).getOrElse(rawFactMessage),
        fun(Some(rawSimplifiedFactMessage)).getOrElse(rawSimplifiedFactMessage),
        fun(Some(rawMidSentenceFactMessage)).getOrElse(rawMidSentenceFactMessage),
        fun(Some(rawMidSentenceSimplifiedFactMessage)).getOrElse(rawMidSentenceSimplifiedFactMessage),
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        isYes,
        isVacuousYes,
        prettifier,
        cause
      )
  }

  /**
   * Represents a vacuous "Yes" Fact, which is a special case of Fact where the underlying Fact is No,
   * but it is treated as Yes without meaningful assertions.
   *
   * @param underlying The underlying Fact that is treated as No but represented as Yes.
   * @throws IllegalArgumentException if the underlying Fact's <code>isNo</code> is <code>false</code>.
   */
  class VacuousYes(underlying: Fact) extends Fact {

    require(underlying.isNo)
    
    /**
     * The raw message representing the vacuous "Yes" Fact.
     */
    val rawFactMessage: String = underlying.rawFactMessage
    /**
     * The raw simplified message representing the vacuous "Yes" Fact.
     */
    val rawSimplifiedFactMessage: String = underlying.rawSimplifiedFactMessage
    /**
     * The raw mid-sentence message representing the vacuous "Yes" Fact.
     */
    val rawMidSentenceFactMessage: String = underlying.rawMidSentenceFactMessage
    /**
     * The raw mid-sentence simplified message representing the vacuous "Yes" Fact.
     */
    val rawMidSentenceSimplifiedFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage

    /**
     * Arguments used to format the rawFactMessage.
     */
    val factMessageArgs: IndexedSeq[Any] = underlying.factMessageArgs
    /**
     * Arguments used to format the rawSimplifiedFactMessage.
     */
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    /**
     * Arguments used to format the rawMidSentenceFactMessage.
     */
    val midSentenceFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceFactMessageArgs
    /**
     * Arguments used to format the rawMidSentenceSimplifiedFactMessage.
     */
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs

    /**
     * Indicates whether this Fact is a leaf node in the Fact tree.
     */
    val isLeaf: Boolean = underlying.isLeaf
    /**
     * The prettifier used to format the messages when constructing failure messages.
     */
    val prettifier: Prettifier = underlying.prettifier

    /**
     * An optional cause Throwable associated with the vacuous "Yes" Fact.
     */
    override val cause: Option[Throwable] = underlying.cause

    /**
     * Indicates whether this Fact is evaluated to Yes, return <code>true</code>.
     */
    val isYes: Boolean = true
    /**
     * Indicates whether this Fact is a vacuous "Yes", which means true in a sense but without meaningful assertions, return <code>true</code>
     */
    val isVacuousYes: Boolean = true

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new VacuousYes(underlying.modifyMessage(fun))
  }

  /**
   * A companion object for the <code>VacuousYes</code> class, providing factory methods to create instances of <code>VacuousYes</code> fact.
   */
  object VacuousYes {
    /**
     * Creates a new <code>VacuousYes</code> fact with the provided underlying <code>Fact</code>.
     *
     * @param underlying The underlying <code>Fact</code> that is treated as false (No) but represented as true (Yes).
     * @return A new <code>VacuousYes</code> fact instance.
     * @throws IllegalArgumentException if the underlying Fact is not No.
     */
    def apply(underlying: Fact): VacuousYes = new VacuousYes(underlying)
  }

  /**
   * Companion object for the <code>No</code> case class.
   *
   * @author Bill Venners
   */
  object No {

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, 
     * <code>rawMidSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, <code>simplifiedFactMessageArgs</code>, 
     * <code>midSentenceFactMessageArgs</code>, <code>midSentenceSimplifiedFactMessageArgs</code>, and <code>cause</code> fields.  
     * This is suitable to create <code>No</code> with eager error messages.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param cause the causing throwable of this No instance
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
      cause: Option[Throwable], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        false,
        false,
        prettifier,
        cause
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>factMessage</code>, 
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>, 
     * <code>midSentenceSimplifiedFailureMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFailureMessageArgs</code>.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @param prettifier the prettifier used to prettify message 
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
	      false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>factMessage</code>,
     * <code>negativeFactMessage</code>, <code>midSentenceFactMessage</code>,
     * <code>midSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFactMessageArgs</code>.
     *
     * @param rawFactMessage raw fact message to report if a match fails
     * @param rawMidSentenceFactMessage raw mid sentence fact message to report if a match fails
     * @param factMessageArgs arguments for constructing fact message to report if a match fails
     * @param midSentenceFactMessageArgs arguments for constructing mid sentence fact message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceFactMessageArgs,
        false,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its simplified and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its simplified and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code> and <code>rawMidSentenceFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code> and <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        factMessageArgs,
        simplifiedFactMessageArgs,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages, and its simplified and mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        false,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>, and
     * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFactMessage</code> will return the same
     * string as <code>rawFactMessage</code>, and the <code>rawMidSentenceSimplifiedFailureMessage</code> will return the
     * same string as <code>rawSimplifiedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create No with eager error messages that have same mid-sentence messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawSimplifiedFailureMessage raw message with a meaning opposite to that of the failure message
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>factMessageArgs</code> and <code>simplifiedFailureMessageArgs</code> fields.
     * The <code>rawMidSentenceFactMessage</code> will return the same string as <code>rawFactMessage</code>, and the
     * <code>rawMidSentenceSimplifiedFailureMessage</code> will return the same string as <code>rawSimplifiedFailureMessage</code>.
     * The <code>midSentenceFactMessageArgs</code> will return the same as <code>factMessageArgs</code>, and the
     * <code>midSentenceSimplifiedFailureMessageArgs</code> will return the same as <code>simplifiedFailureMessageArgs</code>.
     * This is suitable to create No with lazy error messages that have same mid-sentence and use different arguments for
     * simplified messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawSimplifiedFailureMessage raw message with a meaning opposite to that of the failure message
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @param simplifiedFailureMessageArgs arguments for constructing message with a meaning opposite to that of the failure message
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        false,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>No</code> with passed <code>rawFactMessage</code>, and
     * <code>cause</code> fields. The <code>rawMidSentenceFactMessage</code>, <code>rawSimplifiedFailureMessage</code> and
     * <code>rawMidSentenceSimplifiedFailureMessage</code>will return the same string as <code>rawFactMessage</code>.
     * All argument fields will have <code>Vector.empty</code> values.  This is suitable to create No with eager error messages
     * that have same mid-sentence messages.
     *
     * @param rawFactMessage raw fact message
     * @param cause the causing throwable of this No instance
     * @param prettifier the prettifier used to prettify message
     * @return a <code>No</code> instance
     */
    def apply(
      rawFactMessage: String,
      cause: Throwable, 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        false,
        false,
        prettifier,
        Some(cause)
      )
  }
  
  /**
   * Companion object for the <code>Yes</code> case class.
   *
   * @author Bill Venners
   */
  object Yes {
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code>, 
     * <code>rawMidSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, <code>simplifiedFactMessageArgs</code>, 
     * <code>midSentenceFactMessageArgs</code>, <code>midSentenceSimplifiedFactMessageArgs</code>, and <code>cause</code> fields.  
     * This is suitable to create <code>Yes</code> with eager error messages.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param cause the causing throwable of this No instance
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any],
      isVacuousYes: Boolean = false,
      cause: Option[Throwable], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        true,
        isVacuousYes,
        prettifier,
        cause
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed code>factMessage</code>, 
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>, 
     * <code>midSentenceSimplifiedFailureMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFailureMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFailureMessageArgs</code>.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed code>factMessage</code>,
     * <code>negativeFailureMessage</code>, <code>midSentenceFactMessage</code>,
     * <code>midSentenceSimplifiedFactMessage</code>, <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> fields.
     * <code>factMessageArgs</code>, and <code>simplifiedFactMessageArgs</code> will be used in place of <code>midSentenceFactMessageArgs</code>
     * and <code>midSentenceSimplifiedFactMessageArgs</code>.
     *
     * @param rawFactMessage raw fact message to report if a match fails
     * @param rawMidSentenceFactMessage raw mid-sentence fact message to report if a match fails
     * @param factMessageArgs arguments for constructing fact message to report if a match fails
     * @param midSentenceFactMessageArgs arguments for constructing mid-sentence fact message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        factMessageArgs,
        factMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceFactMessageArgs,
        true,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>rawMidSentenceFactMessage</code>, and
     * <code>rawMidSentenceSimplifiedFailureMessage</code> fields.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create Yes with eager error messages, and its mid-sentence messages need to be different.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param rawMidSentenceFactMessage raw failure message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawMidSentenceFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its simplified and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its simplified and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code> and <code>rawMidSentenceFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code> and <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        factMessageArgs,
        simplifiedFactMessageArgs,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its simplified and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        true,
        false,
        prettifier,
        None
      )

    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawFactMessage</code>, <code>rawSimplifiedFactMessage</code>, <code>rawMidSentenceFactMessage</code> and
     * <code>rawMidSentenceSimplifiedFactMessage</code> fields.  All argument fields will have <code>Vector.empty</code>
     * values.  This is suitable to create Yes with eager error messages, and its simplified and mid-sentence messages
     * need to be different.
     *
     * @param rawFactMessage raw message to report for this fact
     * @param rawSimplifiedFactMessage raw simplified message to report for this fact
     * @param rawMidSentenceFactMessage raw mid-sentence message to report for this fact
     * @param rawMidSentenceSimplifiedFactMessage raw mid-sentence simplified message to report for this fact
     * @param factMessageArgs arguments for <code>rawFactMessage</code>
     * @param simplifiedFactMessageArgs arguments for <code>rawSimplifiedFactMessage</code>
     * @param midSentenceFactMessageArgs arguments for <code>rawMidSentenceFactMessage</code>
     * @param midSentenceSimplifiedFactMessageArgs arguments for <code>rawMidSentenceSimplifiedFactMessage</code>
     * @param cause optional cause <code>Throwable</code> associated with the fact.
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      rawSimplifiedFactMessage: String,
      rawMidSentenceFactMessage: String,
      rawMidSentenceSimplifiedFactMessage: String,
      factMessageArgs: IndexedSeq[Any],
      simplifiedFactMessageArgs: IndexedSeq[Any],
      midSentenceFactMessageArgs: IndexedSeq[Any],
      midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any], 
      cause: Option[Throwable], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawSimplifiedFactMessage,
        rawMidSentenceFactMessage,
        rawMidSentenceSimplifiedFactMessage,
        factMessageArgs,
        simplifiedFactMessageArgs,
        midSentenceFactMessageArgs,
        midSentenceSimplifiedFactMessageArgs,
        true,
        false,
        prettifier,
        cause
      )  
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>, and
     * <code>rawNegativeFailureMessage</code> fields. The <code>rawMidSentenceFactMessage</code> will return the same
     * string as <code>rawFactMessage</code>, and the <code>rawMidSentenceSimplifiedFailureMessage</code> will return the
     * same string as <code>rawSimplifiedFailureMessage</code>.  All argument fields will have <code>Vector.empty</code> values.
     * This is suitable to create Yes with eager error messages that have same mid-sentence messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String, 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        true,
        false,
        prettifier,
        None
      )
  
    /**
     * Factory method that constructs a new <code>Yes</code> with passed <code>rawFactMessage</code>,
     * <code>rawNegativeFailureMessage</code>, <code>factMessageArgs</code> and <code>simplifiedFailureMessageArgs</code> fields.
     * The <code>rawMidSentenceFactMessage</code> will return the same string as <code>rawFactMessage</code>, and the
     * <code>rawMidSentenceSimplifiedFailureMessage</code> will return the same string as <code>rawSimplifiedFailureMessage</code>.
     * The <code>midSentenceFactMessageArgs</code> will return the same as <code>factMessageArgs</code>, and the
     * <code>midSentenceSimplifiedFailureMessageArgs</code> will return the same as <code>simplifiedFailureMessageArgs</code>.
     * This is suitable to create Yes with lazy error messages that have same mid-sentence and use different arguments for
     * simplified messages.
     *
     * @param rawFactMessage raw failure message to report if a match fails
     * @param factMessageArgs arguments for constructing failure message to report if a match fails
     * @param prettifier the prettifier used to prettify message
     * @return a <code>Yes</code> instance
     */
    def apply(
      rawFactMessage: String,
      factMessageArgs: IndexedSeq[Any], 
      prettifier: Prettifier
    ): Leaf =
      new Leaf(
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        rawFactMessage,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        factMessageArgs,
        true,
        false,
        prettifier,
        None
      )
  }

  // The simplified Fact message is used when a Fact is negated, because
  // sometimes factMessage can include info about what was expected, as in
  // "Expected 3, but got 4", for expectResult(3) { x } . But if this is inverted
  // to !expectResult(3) { x }, then it is confusing to say Expected 3 (because
  // now anything *but* 3 is expected. So the simplified message just says, "3 did not equal 4".
  // Of course, that means x was 4, and so the inverted form would be a Yes. But if x were 3, then
  // the regular factMessage would be "Expected 3, but got 3" and the simplified fact message would be "3 equaled 3"
  // TODO: Write a test that ensures !(!(<vacuous yes>)).isVacuousYes stays true
  case class Unary_!(underlying: Fact) extends Fact {

    val rawFactMessage: String = underlying.rawSimplifiedFactMessage
    val rawSimplifiedFactMessage: String = underlying.rawSimplifiedFactMessage
    val rawMidSentenceFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = underlying.rawMidSentenceSimplifiedFactMessage
    val factMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val simplifiedFactMessageArgs: IndexedSeq[Any] = underlying.simplifiedFactMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = underlying.midSentenceSimplifiedFactMessageArgs
    val isLeaf: Boolean = underlying.isLeaf
    val prettifier: Prettifier = underlying.prettifier

    val isYes: Boolean = !(underlying.isYes)
    val isVacuousYes: Boolean = false

    override def unary_! : org.scalatest.Fact = underlying

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      val msg = underlying.factDiagram(0)
      padding + (if (isYes) "Yes(" else "No(") + NEWLINE + {
        if (msg.contains("\n"))
          ("!" + msg).split("\n").map(line => padding + "  " + line).mkString("\n") + NEWLINE
        else
          padding + "  !" + msg + NEWLINE
      } +
      padding + ")"
    }

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = Unary_!(underlying.modifyMessage(fun))
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances.
   *
   * @param left  The left-hand side `Fact` instance of the AND operation.
   * @param right The right-hand side `Fact` instance of the AND operation.
   * @param messageFun An optional message function to modify messages.
   */
  class Binary_&(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    private[scalatest] def operatorName: String = "&"

    /**
     * The raw message representing the logical AND operation result between the `left` and `right` `Fact` instances.
     *
     * @note If both `left` and `right` are leaf nodes, the result will be a comma-separated message.
     *       Otherwise, a recursive representation of the combined facts will be used.
     */
    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          if (left.isYes && right.isNo)
            Resources.rawCommaBut
          else
            Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(msgFun => msgFun(Some(msg))).getOrElse(msg)  
    }
    /**
     * The simplified version of the raw fact message, which is the same as `rawFactMessage`.
     */
    val rawSimplifiedFactMessage: String = rawFactMessage
    /**
     * The raw message used in the middle of a sentence, which is the same as `rawFactMessage`.
     */
    val rawMidSentenceFactMessage: String = rawFactMessage
    /**
     * The simplified version of the raw message used in the middle of a sentence, which is the same as `rawFactMessage`.
     */
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    /**
     * The arguments used in the fact message. If both `left` and `right` are leaf nodes, the result will be a `Vector`
     * containing simplified fact messages for both sides. Otherwise, a `Vector` of unquoted strings representing the
     * fact diagrams of the `left` and `right` instances will be used.
     */
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          SimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    /**
     * The simplified version of the fact message arguments, which is the same as `factMessageArgs`.
     */
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    /**
     * The arguments used in the middle of a sentence fact message. If both `left` and `right` are leaf nodes, the result
     * will be a `Vector` containing middle of a sentence simplified fact messages for both sides. Otherwise, a `Vector`
     * of unquoted strings representing the fact diagrams of the `left` and `right` instances will be used.
     */
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          MidSentenceSimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    /**
     * The simplified version of the middle of a sentence fact message arguments, which is the same as `midSentenceFactMessageArgs`.
     */
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs
    /**
     * Returns whether this `Binary_&` instance is a leaf node or not. Since it represents an AND operation, it is always `false`.
     */
    val isLeaf: Boolean = false
    /**
     * Returns `true` if both the `left` and `right` `Fact` instances are `yes`, `false` otherwise.
     */
    val isYes: Boolean = left.isYes && right.isYes
    /**
     * Returns `true` if both the `left` and `right` `Fact` instances are `yes` and at least one of them is a vacuous `yes`,
     * `false` otherwise.
     */
    val isVacuousYes: Boolean = isYes && (left.isVacuousYes || right.isVacuousYes)
    /**
     * The prettifier used in this `Binary_&` instance, which is taken from the `left` `Fact` instance.
     */
    val prettifier: Prettifier = left.prettifier

    /**
     * Generates the fact diagram for this `Binary_&` instance.
     *
     * @param level The indentation level used for pretty printing the fact diagram.
     * @return The fact diagram as a string representation.
     */
    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
        left.factDiagram(level + 1) + " " + operatorName + NEWLINE +
        right.factDiagram(level + 1) + NEWLINE +
        padding + ")"
    }

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_&(left, right, Some(fun))
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances.
   * This is a factory object used to create `Binary_&` instances.
   */
  object Binary_& {
    /**
     * Creates a new `Binary_&` instance with the specified left and right `Fact` instances.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @return A new `Binary_&` instance representing the logical AND operation of the two `Fact` instances.
     */
    def apply(left: Fact, right: Fact): Fact = new Binary_&(left, right, None)
    /**
     * Creates a new `Binary_&` instance with the specified left and right `Fact` instances, and an optional clue.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @param messageFun An optional messaage function to modify the messages.
     * @return A new `Binary_&` instance representing the logical AND operation of the two `Fact` instances.
     */
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_&(left, right, messageFun)
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances, enforcing that the left-hand side `Fact` instance is `yes`.
   *
   * @param left  The left-hand side `Fact` instance of the AND operation. It must be a `yes` fact.
   * @param right The right-hand side `Fact` instance of the AND operation.
   * @param messageFun An optional message function to modify the messages.
   * @throws IllegalArgumentException If the `left` `Fact` instance is not a `yes` fact.
   */
  class Binary_&&(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Binary_&(left, right, messageFun) {
    require(left.isYes)
    override private[scalatest] def operatorName: String = "&&"
    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    override def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_&&(left, right, Some(fun))
  }

  /**
   * Represents a binary logical AND operation between two `Fact` instances, enforcing that the left-hand side `Fact` instance is `yes`.
   * This is a factory object used to create `Binary_&&` instances.
   */
  object Binary_&& {
    /**
     * Creates a new `Binary_&&` instance with the specified left and right `Fact` instances, enforcing that the left-hand side `Fact` instance is `yes`.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation. It must be a `yes` fact.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @return A new `Binary_&&` instance representing the logical AND operation of the two `Fact` instances.
     * @throws IllegalArgumentException If the `left` `Fact` instance is not a `yes` fact.
     */
    def apply(left: Fact, right: Fact): Fact = new Binary_&&(left, right, None)
    /**
     * Creates a new `Binary_&&` instance with the specified left and right `Fact` instances and an optional clue, enforcing that the left-hand side `Fact` instance is `yes`.
     *
     * @param left  The left-hand side `Fact` instance of the AND operation. It must be a `yes` fact.
     * @param right The right-hand side `Fact` instance of the AND operation.
     * @param messageFun An optional message function to modify the messages.
     * @return A new `Binary_&&` instance representing the logical AND operation of the two `Fact` instances.
     * @throws IllegalArgumentException If the `left` `Fact` instance is not a `yes` fact.
     */
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_&&(left, right, messageFun)
  }

  class Binary_|(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    private[scalatest] def operatorName: String = "|"

    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(fun => fun(Some(msg))).getOrElse(msg)
    }
    val rawSimplifiedFactMessage: String = rawFactMessage
    val rawMidSentenceFactMessage: String = rawFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(SimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(MidSentenceSimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs

    val isLeaf: Boolean = false
    val isYes: Boolean = left.isYes || right.isYes
    val isVacuousYes: Boolean = (left.isVacuousYes && (right.isVacuousYes || right.isNo)) || ((left.isVacuousYes || left.isNo) && right.isVacuousYes)
    val prettifier: Prettifier = left.prettifier

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
      left.factDiagram(level + 1) + " " + operatorName + NEWLINE +
      right.factDiagram(level + 1) + NEWLINE +
      padding + ")"
    }

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_|(left, right, Some(fun))
  }

  object Binary_| {
    def apply(left: Fact, right: Fact): Fact = new Binary_|(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_|(left, right, messageFun)
  }

  class Binary_||(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Binary_|(left, right, messageFun) {
    require(left.isNo)
    override private[scalatest] def operatorName: String = "||"
    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    override def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_||(left, right, Some(fun))
  }

  object Binary_|| {
    def apply(left: Fact, right: Fact): Fact = new Binary_||(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_||(left, right, messageFun)
  }

/*
  Yes implies No // x, but y
  Yes implies Yes // x, and y
*/
  class Implies(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    require(left.isYes)

    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          if (left.isYes && right.isNo)
            Resources.rawCommaBut
          else
            Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(fun => fun(Some(msg))).getOrElse(msg)  
    }
    val rawSimplifiedFactMessage: String = rawFactMessage
    val rawMidSentenceFactMessage: String = rawFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          SimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(
          MidSentenceSimplifiedFactMessage(left),
          MidSentenceSimplifiedFactMessage(right)
        ) // Simplify if combining
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }

    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs

    val isLeaf: Boolean = false
    val isVacuousYes: Boolean = false // TODO
    val prettifier: Prettifier = left.prettifier

    val isYes: Boolean = left.isYes && right.isYes

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
        left.factDiagram(level + 1) + " implies" + NEWLINE +
        right.factDiagram(level + 1) + NEWLINE +
        padding + ")"
    }

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new Implies(left, right, Some(fun))
  }

  object Implies {
    def apply(left: Fact, right: Fact): Fact = new Implies(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Implies(left, right, messageFun)
  }

  class IsEqvTo(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Fact {

    val rawFactMessage: String = {
      val msg = 
        if (left.isLeaf && right.isLeaf) {
          Resources.rawCommaAnd
        }
        else factDiagram(0)
      messageFun.flatMap(fun => fun(Some(msg))).getOrElse(msg)  
    }
    val rawSimplifiedFactMessage: String = rawFactMessage
    val rawMidSentenceFactMessage: String = rawFactMessage
    val rawMidSentenceSimplifiedFactMessage: String = rawFactMessage
    val factMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(SimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val simplifiedFactMessageArgs: IndexedSeq[Any] = factMessageArgs
    val midSentenceFactMessageArgs: IndexedSeq[Any] = {
      if (left.isLeaf && right.isLeaf) {
        Vector(MidSentenceSimplifiedFactMessage(left), MidSentenceSimplifiedFactMessage(right))
      }
      else {
        Vector(UnquotedString(left.factDiagram(0)), UnquotedString(right.factDiagram(0)))
      }
    }
    val midSentenceSimplifiedFactMessageArgs: IndexedSeq[Any] = midSentenceFactMessageArgs

    val isLeaf: Boolean = false
    val isYes: Boolean = (left.isYes && right.isYes) || (left.isNo && right.isNo)
    val isVacuousYes: Boolean = isYes && (left.isVacuousYes || right.isVacuousYes)
    val prettifier: Prettifier = left.prettifier

    override def factDiagram(level: Int): String = {
      val padding = "  " * level
      padding + stringPrefix + "(" + NEWLINE +
      left.factDiagram(level + 1) + " isEqvTo" + NEWLINE +
      right.factDiagram(level + 1) + NEWLINE +
      padding + ")"
    }

    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    def modifyMessage(fun: Option[String] => Option[String]): Fact = new IsEqvTo(left, right, Some(fun))
  }

  object IsEqvTo {
    def apply(left: Fact, right: Fact): Fact = new IsEqvTo(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new IsEqvTo(left, right, messageFun)
  }

  // Idea is to override toString each time it is used.
  private[scalatest] sealed abstract class LazyMessage {
    val nestedArgs: IndexedSeq[Any]
  }

  private[scalatest] case class FactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.factMessageArgs
    override def toString: String = fact.factMessage
  }

  private[scalatest] case class MidSentenceFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.midSentenceFactMessageArgs
    override def toString: String = fact.midSentenceFactMessage
  }

  private[scalatest] case class SimplifiedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.simplifiedFactMessageArgs
    override def toString: String = fact.simplifiedFactMessage
  }

  private[scalatest] case class MidSentenceSimplifiedFactMessage(fact: Fact) extends LazyMessage {
    val nestedArgs: IndexedSeq[Any] = fact.midSentenceSimplifiedFactMessageArgs
    override def toString: String = fact.midSentenceSimplifiedFactMessage
  }
}
