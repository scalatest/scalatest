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
 * anything" (<em>ex falso quodlibet</em>). See the <a href="#vacuousYes">Vacuous Yes</a> section
 * below for more details.
 * </p>
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
 * <a name="vacuousYes"></a>
 * <h2>Vacuous Yes</h2>
 *
 * <p>
 * A <code>VacuousYes</code> is a special kind of yes that indicates an assertion was technically
 * true, but didn't test anything meaningful. The most common way to create a <code>VacuousYes</code>
 * is by using <code>implies</code> with a false premise. When the premise is false, the implication
 * is vacuously true according to classical logic, following the principle of <em>ex falso quodlibet</em>
 * ("from falsehood, anything follows").
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
 * <a name="vacuousPropagation"></a>
 * <h3>Vacuous Yes Propagation</h3>
 *
 * <p>
 * When you combine <code>Fact</code>s using logical operators, the vacuous property tends to
 * propagate through the operations. The general rule is: <em>if the result is yes and either
 * operand is vacuous, the result is also vacuous</em>. This ensures that vacuous truth is
 * not hidden by subsequent operations.
 * </p>
 *
 * <p>
 * Here's how vacuous yes propagates through each operator:
 * </p>
 *
 * <table style="border-collapse: collapse; border: 1px solid black">
 * <tr>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Operator
 *   </th>
 *   <th style="background-color: #CCCCCC; border-width: 1px; padding: 3px; border: 1px solid black; text-align: left">
 *     Vacuous Propagation Rule
 *   </th>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>&&</code> or <code>&amp;</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Result is VacuousYes if result is Yes AND (left is VacuousYes OR right is VacuousYes)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>||</code> or <code>|</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Result is VacuousYes if (left is VacuousYes AND right is VacuousYes) OR<br/>
 *     (left is VacuousYes AND right is No) OR (left is No AND right is VacuousYes)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>implies</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Result is VacuousYes if premise is No (regardless of consequent), OR<br/>
 *     result is Yes AND (premise is VacuousYes OR consequent is VacuousYes)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>isEqvTo</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Result is VacuousYes if result is Yes AND (left is VacuousYes OR right is VacuousYes)
 *   </td>
 * </tr>
 * <tr>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     <code>!</code>
 *   </td>
 *   <td style="border-width: 1px; padding: 3px; border: 1px solid black">
 *     Negation always produces a non-vacuous result
 *   </td>
 * </tr>
 * </table>
 *
 * <p>
 * Here's an example showing vacuous propagation:
 * </p>
 *
 * <pre class="stHighlight">
 * val x = -5
 * val premise = expect(x &gt; 0)
 * val consequent = expect(x &lt; 100)
 * val implication = premise implies consequent  // VacuousYes
 *
 * val anotherFact = expect(true)
 * val combined = implication && anotherFact     // Still VacuousYes!
 * combined.isVacuousYes  // true - vacuousness propagated
 * </pre>
 *
 * <p>
 * This propagation ensures that if any part of a complex assertion was vacuously true, the
 * overall result will be marked as vacuous, preventing false confidence in test coverage.
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
 * @author Chua Chee Seng
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
   * Negates this Fact.
   *
   * @return A Fact with the opposite value.
   */ 
  def unary_! : Fact = Fact.Unary_!(this)

  /**
   * <p>
   * Returns a new <code>Fact</code> that represents the logical OR of this <code>Fact</code> and the given <code>Fact</code>.
   * </p>
   *
   * <p>
   * This operator short-circuits, just like the <code>||</code> operator on <code>Boolean</code>. If this
   * <code>Fact</code> is <code>Yes</code>, the right-hand side will not be evaluated and this <code>Fact</code>
   * will be returned.
   * </p>
   *
   * @param rhs the right-hand side <code>Fact</code> (evaluated by-name)
   * @return a <code>Fact</code> representing the logical OR of this and the right-hand side
   */
  final def ||(rhs: => Fact): Fact = if (isYes) this else Fact.Binary_||(this, rhs)

  /**
   * <p>
   * Returns a new <code>Fact</code> that represents the logical AND of this <code>Fact</code> and the given <code>Fact</code>.
   * </p>
   *
   * <p>
   * This operator short-circuits, just like the <code>&&</code> operator on <code>Boolean</code>. If this
   * <code>Fact</code> is <code>No</code>, the right-hand side will not be evaluated and this <code>Fact</code>
   * will be returned.
   * </p>
   *
   * @param rhs the right-hand side <code>Fact</code> (evaluated by-name)
   * @return a <code>Fact</code> representing the logical AND of this and the right-hand side
   */
  final def &&(rhs: => Fact): Fact = if (isNo) this else Fact.Binary_&&(this, rhs)

  /**
   * <p>
   * Returns a new <code>Fact</code> that represents the logical OR of this <code>Fact</code> and the given <code>Fact</code>.
   * </p>
   *
   * <p>
   * This operator does <em>not</em> short-circuit, just like the <code>|</code> operator on <code>Boolean</code>.
   * Both the left-hand and right-hand sides will be evaluated regardless of their values.
   * </p>
   *
   * @param rhs the right-hand side <code>Fact</code>
   * @return a new <code>Fact</code> representing the logical OR of this and the right-hand side
   */
  final def |(rhs: Fact): Fact = Fact.Binary_|(this, rhs)

  /**
   * <p>
   * Returns a new <code>Fact</code> that represents the logical AND of this <code>Fact</code> and the given <code>Fact</code>.
   * </p>
   *
   * <p>
   * This operator does <em>not</em> short-circuit, just like the <code>&</code> operator on <code>Boolean</code>.
   * Both the left-hand and right-hand sides will be evaluated regardless of their values.
   * </p>
   *
   * @param rhs the right-hand side <code>Fact</code>
   * @return a new <code>Fact</code> representing the logical AND of this and the right-hand side
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
  final /* DOTTY-ONLY infix */ def implies(rhs: => Fact): Fact = if (isNo) Fact.VacuousYes(this) else Fact.Implies(this, rhs)

  /**
   * Creates a new Fact that represents the equivalence (eqv) between this Fact and the provided Fact.
   *
   * @param rhs The Fact representing the other side of the equivalence.
   * @return A new Fact representing the equivalence between this Fact and the provided Fact.
   */
  final /* DOTTY-ONLY infix */ def isEqvTo(rhs: Fact): Fact = Fact.IsEqvTo(this, rhs)

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
    val msg = factMessage // just compute this once
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
   * <p>
   * Case class representing a leaf node in the <code>Fact</code> tree.
   * </p>
   *
   * <p>
   * A <code>Leaf</code> is an atomic <code>Fact</code> created directly by expectation methods like
   * <code>expect</code>, <code>expectResult</code>, or <code>expectThrows</code>. Unlike composite
   * <code>Fact</code>s (which are created by combining other <code>Fact</code>s with operators like
   * <code>&&</code>, <code>||</code>, or <code>implies</code>), a <code>Leaf</code> represents a
   * single assertion result.
   * </p>
   *
   * <p>
   * You can check whether a <code>Fact</code> is a leaf by calling <code>isLeaf</code>. The
   * <code>isLeaf</code> method will return <code>true</code> only if the <code>Fact</code> is
   * a <code>Leaf</code>.
   * </p>
   *
   * <p>
   * Leaf facts are used for message formatting decisions. When combining two leaf facts with operators,
   * ScalaTest can generate simplified comma-separated messages like "3 was not equal to 4, and 5 was
   * greater than 2". When combining composite facts, full fact diagrams are used instead to show the
   * nested logical structure.
   * </p>
   *
   * @param rawFactMessage the raw message representing the fact
   * @param rawSimplifiedFactMessage the raw simplified message representing the fact (used when negated)
   * @param rawMidSentenceFactMessage the raw message for use mid-sentence
   * @param rawMidSentenceSimplifiedFactMessage the raw simplified message for use mid-sentence
   * @param factMessageArgs arguments used to format <code>rawFactMessage</code>
   * @param simplifiedFactMessageArgs arguments used to format <code>rawSimplifiedFactMessage</code>
   * @param midSentenceFactMessageArgs arguments used to format <code>rawMidSentenceFactMessage</code>
   * @param midSentenceSimplifiedFactMessageArgs arguments used to format <code>rawMidSentenceSimplifiedFactMessage</code>
   * @param isYes <code>true</code> if this fact represents a yes (passing) assertion, <code>false</code> for no (failing)
   * @param isVacuousYes <code>true</code> if this is a vacuous yes, <code>false</code> otherwise
   * @param prettifier the <code>Prettifier</code> used to format messages
   * @param cause an optional <code>Throwable</code> cause associated with this fact
   * @throws IllegalArgumentException if <code>isVacuousYes</code> is <code>true</code> but <code>isYes</code> is <code>false</code>
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
   * <p>
   * Class representing a vacuously true <code>Fact</code>.
   * </p>
   *
   * <p>
   * A <code>VacuousYes</code> is a special kind of yes <code>Fact</code> that indicates an assertion
   * was technically true, but something wasn't meaningfully tested. A <code>VacuousYes</code> has
   * <code>isYes</code> returning <code>true</code> and <code>isVacuousYes</code> also returning
   * <code>true</code>.
   * </p>
   *
   * <a name="creation"></a>
   * <h2>How VacuousYes is Created</h2>
   *
   * <p>
   * A <code>VacuousYes</code> first comes into being when the premise of an <code>implies</code>
   * operation is <code>No</code>. In classical logic, a false premise makes an implication
   * vacuously true regardless of the consequent. This principle is known as
   * <em>ex falso quodlibet</em> ("from falsehood, anything follows").
   * </p>
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val premise = expect(score &lt; 0)      // No
   * val consequent = expect(score &lt; 100) // Would be Yes, but won't be evaluated
   * val result = premise implies consequent
   *
   * result.isYes         // true
   * result.isVacuousYes  // true
   * </pre>
   *
   * <p>
   * In this example, because the premise (<code>score &lt; 0</code>) is <code>No</code>, the
   * implication returns a <code>VacuousYes</code> without even evaluating the consequent. The
   * assertion is technically true in a logical sense, but the result does not include any
   * information from the consequent expectation.
   * </p>
   *
   * <a name="propagation"></a>
   * <h2>VacuousYes Propagation</h2>
   *
   * <p>
   * Once a <code>VacuousYes</code> is created, it propagates through subsequent logical operations.
   * If you combine a <code>VacuousYes</code> with other <code>Fact</code>s using operators like
   * <code>&&</code>, <code>&</code>, <code>implies</code>, or <code>isEqvTo</code>, and the result
   * would be <code>Yes</code>, it becomes <code>VacuousYes</code> instead.
   * </p>
   *
   * <p>
   * Here's an example showing propagation:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val premise = expect(score &lt; 0)        // No
   * val implication = premise implies expect(score &lt; 100)  // VacuousYes
   *
   * val anotherFact = expect(score &gt; 0)    // Yes
   * val combined = implication && anotherFact  // VacuousYes (propagated!)
   *
   * combined.isYes         // true
   * combined.isVacuousYes  // true - vacuousness carried through
   * </pre>
   *
   * <p>
   * This propagation is important because it prevents false confidence in test coverage. Even though
   * the combined fact is "yes," the <code>VacuousYes</code> marker indicates that something wasn't
   * meaningfully tested.
   * </p>
   *
   * <a name="conversion"></a>
   * <h2>Converting to Assertion</h2>
   *
   * <p>
   * When you convert a <code>VacuousYes</code> to an <code>Assertion</code> (either explicitly
   * via <code>toAssertion</code> or implicitly in test styles that expect <code>Assertion</code>
   * result type), it throws <code>TestCanceledException</code> rather than succeeding. This
   * signals that the test didn't meaningfully execute.
   * </p>
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val premise = expect(score &lt; 0)  // No
   * val result = premise implies expect(score &lt; 100)  // VacuousYes
   *
   * result.toAssertion  // throws TestCanceledException
   * </pre>
   *
   * <p>
   * This behavior is particularly useful in property-based testing, where you might write:
   * </p>
   *
   * <pre class="stHighlight">
   * forAll { (n: Int) =&gt;
   *   val isEven = n % 2 == 0
   *   expect(isEven) implies expect(n % 4 == 0 || n % 4 == 2)
   * }
   * </pre>
   *
   * <p>
   * For odd values of <code>n</code>, the premise is false, so the implication returns
   * <code>VacuousYes</code>. When converted to <code>Assertion</code>, this throws
   * <code>TestCanceledException</code>, which ScalaTest counts separately from test failures.
   * This lets you see how many test cases were actually verified versus vacuously true.
   * </p>
   *
   * <a name="implementation"></a>
   * <h2>Implementation Details</h2>
   *
   * <p>
   * A <code>VacuousYes</code> wraps an underlying <code>No</code> fact. The underlying fact's
   * messages are preserved, which provides context about what condition was false that led to
   * the vacuous result. You can access these messages through the standard fact message properties.
   * </p>
   *
   * @param underlying the underlying <code>No</code> fact that this <code>VacuousYes</code> wraps
   * @throws IllegalArgumentException if the underlying fact's <code>isNo</code> is <code>false</code>
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
   * Companion object for the <code>VacuousYes</code> class, which provides a factory method to create <code>VacuousYes</code> instances.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
   * Object that contains factory methods for creating <code>Leaf</code> instances
   * representing failed (no) assertions.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
   * Object that contains factory methods for creating <code>Leaf</code> instances
   * representing successful (yes) assertions.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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

  /**
   * <p>
   * Case class representing the logical negation of a <code>Fact</code>.
   * </p>
   *
   * <p>
   * A <code>Unary_!</code> inverts a <code>Fact</code>: if the underlying fact is <code>Yes</code>,
   * the negation is <code>No</code>, and vice versa. Negation always produces a non-vacuous result
   * (<code>isVacuousYes</code> is <code>false</code>), even when negating a <code>VacuousYes</code>.
   * However, the original fact is preserved as the underlying fact, so double negation restores
   * vacuousness: <code>!(!vacuousYes)</code> returns the original <code>VacuousYes</code>.
   * </p>
   *
   * <p>
   * When converted to type <code>Assertion</code>, a <code>No</code> results in a thrown
   * <code>TestFailedException</code> (indicating test failure), which is already a stronger signal
   * than the <code>TestCanceledException</code> thrown when converting <code>VacuousYes</code> to
   * type <code>Assertion</code> (indicating the
   * test didn't meaningfully execute). Since <code>Unary_!</code> preserves the underlying fact, double negation
   * on a <code>VacuousYes</code> correctly remembers the vacuous nature of the resulting yes.
   * </p>
   *
   * <p>
   * The negation operator uses simplified messages instead of the regular fact messages. This is
   * because some fact messages include information about what was expected. For example,
   * <code>expectResult(3) { 4 }</code> produces the message "Expected 3, but got 4". If this is
   * negated to <code>!expectResult(3) { 4 }</code>, saying "Expected 3" would be confusing,
   * because now anything <em>except</em> 3 is expected. Instead, the simplified message is used,
   * which would say "4 did not equal 3".
   * </p>
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val fact = expect(score &lt; 100)     // Yes
   * val negated = !fact                 // No
   *
   * negated.isYes  // false
   * negated.isNo   // true
   * </pre>
   *
   * @param underlying the <code>Fact</code> to negate
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
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
  /**
   * <p>
   * Class representing the strict (non-short-circuiting) logical AND of two <code>Fact</code>s.
   * </p>
   *
   * <p>
   * A <code>Binary_&</code> evaluates both the left and right <code>Fact</code>s regardless of their
   * values, then combines them with logical AND semantics. The result is <code>Yes</code> only if both
   * facts are <code>Yes</code>. If the result is <code>Yes</code> and either operand is
   * <code>VacuousYes</code>, the result becomes <code>VacuousYes</code>.
   * </p>
   *
   * <p>
   * This operator does <em>not</em> short-circuit, unlike <code>Binary_&&</code>. Both sides are
   * always evaluated. This is useful when you want to ensure both assertions run, perhaps for their
   * side effects or to see all failures at once.
   * </p>
   *
   * <p>
   * The <code>&</code> operator creates instances of this class:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val fact1 = expect(score &gt; 0)
   * val fact2 = expect(score &lt; 100)
   * val combined = fact1 & fact2   // Both facts evaluated
   * </pre>
   *
   * @param left the left-hand <code>Fact</code>
   * @param right the right-hand <code>Fact</code>
   * @param messageFun an optional function to transform the combined message
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
   * Companion object for the <code>Binary_&</code> class, which contains factory methods for creating
   * <code>Binary_&</code> instances.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
   * <p>
   * Class representing the short-circuiting logical AND of two <code>Fact</code>s.
   * </p>
   *
   * <p>
   * A <code>Binary_&&</code> extends <code>Binary_&</code> but adds a precondition: the left-hand
   * <code>Fact</code> must be <code>Yes</code>. This class is only instantiated when the
   * <code>&&</code> operator's short-circuit check passes (when the left side is <code>Yes</code>).
   * </p>
   *
   * <p>
   * The <code>&&</code> operator short-circuits: if the left fact is <code>No</code>, it returns
   * the left side immediately without evaluating the right side. A <code>Binary_&&</code> is created
   * to combine the two facts only when the left is <code>Yes</code>.
   * </p>
   *
   * <p>
   * By extending <code>Binary_&</code>, this class inherits all the logic for combining facts,
   * message generation, and vacuous yes propagation. The only differences are the operator name
   * (displayed as "&&" instead of "&") and the precondition that <code>left.isYes</code>.
   * </p>
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val fact1 = expect(score &gt; 0)       // Yes
   * val fact2 = expect(score &lt; 100)     // Yes
   * val combined = fact1 && fact2        // Binary_&& created, both evaluated
   *
   * val fact3 = expect(score &lt; 0)       // No
   * val combined2 = fact3 && fact2       // Returns fact3 immediately, fact2 not evaluated
   * </pre>
   *
   * @param left the left-hand <code>Fact</code> (must be <code>Yes</code>)
   * @param right the right-hand <code>Fact</code>
   * @param messageFun an optional function to transform the combined message
   * @throws IllegalArgumentException if <code>left.isYes</code> is <code>false</code>
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
   * Companion object for the <code>Binary_&&</code> class, which contains factory methods for creating
   * <code>Binary_&&</code> instances.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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

  /**
   * <p>
   * Class representing the strict (non-short-circuiting) logical OR of two <code>Fact</code>s.
   * </p>
   *
   * <p>
   * A <code>Binary_|</code> evaluates both the left and right <code>Fact</code>s regardless of their
   * values, then combines them with logical OR semantics. The result is <code>Yes</code> if either
   * fact is <code>Yes</code>. If the result is <code>Yes</code> and at least one operand is
   * <code>VacuousYes</code>, the result becomes <code>VacuousYes</code>.
   * </p>
   *
   * <p>
   * This operator does <em>not</em> short-circuit, unlike <code>Binary_||</code>. Both sides are
   * always evaluated. This is useful when you want to ensure both assertions run, perhaps for their
   * side effects or to see all results.
   * </p>
   *
   * <p>
   * The <code>|</code> operator creates instances of this class:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val fact1 = expect(score &lt; 0)
   * val fact2 = expect(score &lt; 100)
   * val combined = fact1 | fact2   // Both facts evaluated, result is Yes
   * </pre>
   *
   * @param left the left-hand <code>Fact</code>
   * @param right the right-hand <code>Fact</code>
   * @param messageFun an optional function to transform the combined message
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
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

  /**
   * Companion object for the <code>Binary_|</code> class, which contains factory methods for creating
   * <code>Binary_|</code> instances.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
  object Binary_| {
    def apply(left: Fact, right: Fact): Fact = new Binary_|(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_|(left, right, messageFun)
  }

  /**
   * <p>
   * Class representing the short-circuiting logical OR of two <code>Fact</code>s.
   * </p>
   *
   * <p>
   * A <code>Binary_||</code> extends <code>Binary_|</code> but adds a precondition: the left-hand
   * <code>Fact</code> must be <code>No</code>. This class is only instantiated when the
   * <code>||</code> operator's short-circuit check passes (when the left side is <code>No</code>).
   * </p>
   *
   * <p>
   * The <code>||</code> operator short-circuits: if the left fact is <code>Yes</code>, it returns
   * the left side immediately without evaluating the right side. A <code>Binary_||</code> is created
   * to combine the two facts only when the left is <code>No</code>.
   * </p>
   *
   * <p>
   * By extending <code>Binary_|</code>, this class inherits all the logic for combining facts,
   * message generation, and vacuous yes propagation. The only differences are the operator name
   * (displayed as "||" instead of "|") and the precondition that <code>left.isNo</code>.
   * </p>
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val fact1 = expect(score &lt; 0)       // No
   * val fact2 = expect(score &lt; 100)     // Yes
   * val combined = fact1 || fact2        // Binary_|| created, both evaluated
   *
   * val fact3 = expect(score &gt; 0)       // Yes
   * val combined2 = fact3 || fact2       // Returns fact3 immediately, fact2 not evaluated
   * </pre>
   *
   * @param left the left-hand <code>Fact</code> (must be <code>No</code>)
   * @param right the right-hand <code>Fact</code>
   * @param messageFun an optional function to transform the combined message
   * @throws IllegalArgumentException if <code>left.isNo</code> is <code>false</code>
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
  class Binary_||(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]) extends Binary_|(left, right, messageFun) {
    require(left.isNo)
    override private[scalatest] def operatorName: String = "||"
    /**
     * Get a new instance of <code>Fact</code> with the messages modified using the passed <code>fun</code> function.
     */
    override def modifyMessage(fun: Option[String] => Option[String]): Fact = new Binary_||(left, right, Some(fun))
  }

  /**
   * Companion object for the <code>Binary_||</code> class, which contains factory methods for creating
   * <code>Binary_||</code> instances.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
  object Binary_|| {
    def apply(left: Fact, right: Fact): Fact = new Binary_||(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Binary_||(left, right, messageFun)
  }

  /**
   * <p>
   * Class representing logical implication between two <code>Fact</code>s.
   * </p>
   *
   * <p>
   * An <code>Implies</code> represents the logical implication "if premise then consequent" (left ⇒ right).
   * The implication is <code>No</code> only when the premise (left side) is <code>Yes</code> and the
   * consequent (right side) is <code>No</code>. In all other cases, it is <code>Yes</code> (or
   * <code>VacuousYes</code>).
   * </p>
   *
   * <p>
   * The <code>implies</code> method creates instances of this class only when the premise is <code>Yes</code>.
   * If the premise is <code>No</code>, the method returns a <code>VacuousYes</code> without creating an
   * <code>Implies</code> instance, because a false premise makes any implication vacuously true.
   * </p>
   *
   * <p>
   * Vacuous yes propagation: if the result would be <code>Yes</code> and either the premise or consequent
   * is <code>VacuousYes</code>, the result becomes <code>VacuousYes</code>.
   * </p>
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val premise = expect(score &gt; 0)        // Yes
   * val consequent = expect(score &lt; 100)   // Yes
   * val implication = premise implies consequent  // Yes (both true)
   *
   * val premise2 = expect(score &gt; 50)      // Yes
   * val consequent2 = expect(score &lt; 50)   // No
   * val implication2 = premise2 implies consequent2  // No (premise true, consequent false)
   * </pre>
   *
   * @param left the premise (left-hand <code>Fact</code>, must be <code>Yes</code>)
   * @param right the consequent (right-hand <code>Fact</code>)
   * @param messageFun an optional function to transform the combined message
   * @throws IllegalArgumentException if <code>left.isYes</code> is <code>false</code>
   *
   * @author Bill Venners
   * @author Chua Chee Seng
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
    val prettifier: Prettifier = left.prettifier

    val isYes: Boolean = left.isYes && right.isYes
    val isVacuousYes: Boolean = isYes && (left.isVacuousYes || right.isVacuousYes)

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

  /**
   * Companion object for the <code>Implies</code> class, which contains factory methods for creating
   * <code>Implies</code> instances.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
  object Implies {
    def apply(left: Fact, right: Fact): Fact = new Implies(left, right, None)
    def apply(left: Fact, right: Fact, messageFun: Option[Option[String] => Option[String]]): Fact = new Implies(left, right, messageFun)
  }

  /**
   * <p>
   * Class representing logical equivalence (biconditional) between two <code>Fact</code>s.
   * </p>
   *
   * <p>
   * An <code>IsEqvTo</code> represents the logical equivalence "left if and only if right" (left ⇔ right).
   * The equivalence is <code>Yes</code> when both facts have the same boolean value (both <code>Yes</code>
   * or both <code>No</code>). It is <code>No</code> when the facts have different boolean values.
   * </p>
   *
   * <p>
   * Logical equivalence can be understood as: (<code>left</code> implies <code>right</code>) AND
   * (<code>right</code> implies <code>left</code>). The result is <code>Yes</code> when both
   * implications would hold.
   * </p>
   *
   * <p>
   * Vacuous yes propagation: if the result would be <code>Yes</code> and either fact is
   * <code>VacuousYes</code>, the result becomes <code>VacuousYes</code>.
   * </p>
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * val score = 85
   * val fact1 = expect(score &gt; 0)        // Yes
   * val fact2 = expect(score &lt; 100)      // Yes
   * val equiv = fact1 isEqvTo fact2       // Yes (both are Yes)
   *
   * val fact3 = expect(score &lt; 0)        // No
   * val fact4 = expect(score &gt; 200)      // No
   * val equiv2 = fact3 isEqvTo fact4      // Yes (both are No)
   *
   * val equiv3 = fact1 isEqvTo fact3      // No (one Yes, one No)
   * </pre>
   *
   * @param left the left-hand <code>Fact</code>
   * @param right the right-hand <code>Fact</code>
   * @param messageFun an optional function to transform the combined message
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
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

  /**
   * Companion object for the <code>IsEqvTo</code> class, which contains factory methods for creating
   * <code>IsEqvTo</code> instances.
   *
   * @author Bill Venners
   * @author Chua Chee Seng
   */
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
