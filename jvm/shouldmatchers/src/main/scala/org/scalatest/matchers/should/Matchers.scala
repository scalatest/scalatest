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
package org.scalatest.matchers.should

import org.scalatest.FailureMessages
import org.scalatest.Resources
import org.scalatest.Assertion
import org.scalatest.Assertions
import org.scalatest.Suite
import org.scalatest.UnquotedString
import org.scalatest.CompileMacro

import org.scalactic._
import org.scalatest.enablers._
import org.scalatest.matchers._

import org.scalatest.matchers.dsl._
import org.scalatest.verbs.CompileWord
import org.scalatest.verbs.TypeCheckWord
import org.scalatest.verbs.ShouldVerb
import org.scalatest.matchers.dsl.ResultOfNoElementsOfApplication
import org.scalatest.matchers.dsl.ResultOfOneElementOfApplication
import org.scalactic.ColCompatHelper.Iterable
import scala.reflect.{classTag, ClassTag}
import scala.util.matching.Regex
import DefaultEquality.areEqualComparingArraysStructurally
import org.scalatest.matchers.MatchersHelper
import MatchersHelper.transformOperatorChars
import TripleEqualsSupport.Spread
import TripleEqualsSupport.TripleEqualsInvocation
import TripleEqualsSupport.TripleEqualsInvocationOnSpread
import ArrayHelper.deep
// SKIP-SCALATESTJS,NATIVE-START
import MatchersHelper.accessProperty
import MatchersHelper.matchSymbolToPredicateMethod
// SKIP-SCALATESTJS,NATIVE-END
import scala.language.experimental.macros
import scala.language.higherKinds
import MatchersHelper.endWithRegexWithGroups
import MatchersHelper.fullyMatchRegexWithGroups
import MatchersHelper.includeRegexWithGroups
import MatchersHelper.indicateFailure
import MatchersHelper.indicateSuccess
import MatchersHelper.newTestFailedException
import MatchersHelper.startWithRegexWithGroups
import org.scalatest.exceptions._

// TODO: drop generic support for be as an equality comparison, in favor of specific ones.
// TODO: Put links from ShouldMatchers to wherever I reveal the matrix and algo of how properties are checked dynamically.
// TODO: double check that I wrote tests for (length (7)) and (size (8)) in parens
// TODO: document how to turn off the === implicit conversion
// TODO: Document you can use JMock, EasyMock, etc.

/**
 * Trait that provides a domain specific language (DSL) for expressing assertions in tests
 * using the word <code>should</code>.
 *
 * <p>
 * For example, if you mix <code>Matchers</code> into
 * a suite class, you can write an equality assertion in that suite like this:
 * </p>
 *
 * <pre class="stHighlight">
 * result should equal (3)
 * </pre>
 *
 * <p>
 * Here <code>result</code> is a variable, and can be of any type. If the object is an
 * <code>Int</code> with the value 3, execution will continue (<em>i.e.</em>, the expression will result
 * in the unit value, <code>()</code>). Otherwise, a <a href="../../exceptions/TestFailedException.html"><code>TestFailedException</code></a>
 * will be thrown with a detail message that explains the problem, such as <code>"7 did not equal 3"</code>.
 * This <code>TestFailedException</code> will cause the test to fail.
 * </p>
 *
 * <p>
 * Here is a table of contents for this documentation:
 * </p>
 *
 * <ul>
 * <li><a href="#matchersMigration">Matchers migration in ScalaTest 2.0</a></li>
 * <li><a href="#checkingEqualityWithMatchers">Checking equality with matchers</a></li>
 * <li><a href="#checkingSizeAndLength">Checking size and length</a></li>
 * <li><a href="#checkingStrings">Checking strings</a></li>
 * <li><a href="#greaterAndLessThan">Greater and less than</a></li>
 * <li><a href="#checkingBooleanPropertiesWithBe">Checking <code>Boolean</code> properties with <code>be</code></a></li>
 * <li><a href="#usingCustomBeMatchers">Using custom <code>BeMatchers</code></a></li>
 * <li><a href="#checkingObjectIdentity">Checking object identity</a></li>
 * <li><a href="#checkingAnObjectsClass">Checking an object's class</a></li>
 * <li><a href="#checkingNumbersAgainstARange">Checking numbers against a range</a></li>
 * <li><a href="#checkingForEmptiness">Checking for emptiness</a></li>
 * <li><a href="#workingWithContainers">Working with "containers"</a></li>
 * <li><a href="#workingWithAggregations">Working with "aggregations"</a></li>
 * <li><a href="#workingWithSequences">Working with "sequences"</a></li>
 * <li><a href="#workingWithSortables">Working with "sortables"</a></li>
 * <li><a href="#workingWithIterators">Working with iterators</a></li>
 * <li><a href="#inspectorShorthands">Inspector shorthands</a></li>
 * <li><a href="#singleElementCollections">Single-element collections</a></li>
 * <li><a href="#javaCollectionsAndMaps">Java collections and maps</a></li>
 * <li><a href="#stringsAndArraysAsCollections"><code>String</code>s and <code>Array</code>s as collections</a></li>
 * <li><a href="#beAsAnEqualityComparison">Be as an equality comparison</a></li>
 * <li><a href="#beingNegative">Being negative</a></li>
 * <li><a href="#checkingThatCodeDoesNotCompile">Checking that a snippet of code does not compile</a></li>
 * <li><a href="#logicalExpressions">Logical expressions with <code>and</code> and <code>or</code></a></li>
 * <li><a href="#workingWithOptions">Working with <code>Option</code>s</a></li>
 * <li><a href="#checkingArbitraryProperties">Checking arbitrary properties with <code>have</code></a></li>
 * <li><a href="#lengthSizeHavePropertyMatchers">Using <code>length</code> and <code>size</code> with <code>HavePropertyMatcher</code>s</a></li>
 * <li><a href="#matchingAPattern">Checking that an expression matches a pattern</a></li>
 * <li><a href="#usingCustomMatchers">Using custom matchers</a></li>
 * <li><a href="#checkingForExpectedExceptions">Checking for expected exceptions</a></li>
 * <li><a href="#thosePeskyParens">Those pesky parens</a></li>
 * </ul>
 *
 * <p>
 * Trait <a href="../must/Matchers.html"><code>must.Matchers</code></a> is an alternative to <!-- PRESERVE --><code>should.Matchers</code> that provides the exact same
 * meaning, syntax, and behavior as <!-- PRESERVE --><code>should.Matchers</code>, but uses the verb <code>must</code> instead of <!-- PRESERVE --><code>should</code>.
 * The two traits differ only in the English semantics of the verb: <!-- PRESERVE --><code>should</code>
 * is informal, making the code feel like conversation between the writer and the reader; <code>must</code> is more formal, making the code feel more like
 * a written specification.
 * </p>
 *
 * <a name="checkingEqualityWithMatchers"></a>
 * <h2>Checking equality with matchers</h2>
 *
 * <p>
 * ScalaTest matchers provides five different ways to check equality, each designed to address a different need. They are:
 * </p>
 *
 * <pre class="stHighlight">
 * result should equal (3) // can customize equality
 * result should === (3)   // can customize equality and enforce type constraints
 * result should be (3)    // cannot customize equality, so fastest to compile
 * result shouldEqual 3    // can customize equality, no parentheses required
 * result shouldBe 3       // cannot customize equality, so fastest to compile, no parentheses required
 * </pre>
 *
 * <p>
 * The &ldquo;<code>left</code> <code>should</code> <code>equal</code> <code>(right)</code>&rdquo; syntax requires an
 * <code>org.scalactic.Equality[L]</code> to be provided (either implicitly or explicitly), where
 * <code>L</code> is the left-hand type on which <code>should</code> is invoked. In the "<code>left</code> <code>should</code> <code>equal</code> <code>(right)</code>" case,
 * for example, <code>L</code> is the type of <code>left</code>. Thus if <code>left</code> is type <code>Int</code>, the "<code>left</code> <code>should</code>
 * <code>equal</code> <code>(right)</code>"
 * statement would require an <code>Equality[Int]</code>.
 * </p>
 *
 * <p>
 * By default, an implicit <code>Equality[T]</code> instance is available for any type <code>T</code>, in which equality is implemented
 * by simply invoking <code>==</code>  on the <code>left</code>
 * value, passing in the <code>right</code> value, with special treatment for arrays. If either <code>left</code> or <code>right</code> is an array, <code>deep</code>
 * will be invoked on it before comparing with <em>==</em>. Thus, the following expression
 * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) == Array(1, 2) // yields false
 * </pre>
 *
 * <p>
 * The next expression will by default <em>not</em> result in a <code>TestFailedException</code>, because default <code>Equality[Array[Int]]</code> compares
 * the two arrays structurally, taking into consideration the equality of the array's contents:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) should equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
 * <code>be theSameInstanceAs</code> syntax, <a href="#checkingObjectIdentity">described below</a>.
 * </p>
 *
 * <p>
 * You can customize the meaning of equality for a type when using "<code>should</code> <code>equal</code>," "<code>should</code> <code>===</code>,"
 * or <code>shouldEqual</code> syntax by defining implicit <code>Equality</code> instances that will be used instead of default <code>Equality</code>.
 * You might do this to normalize types before comparing them with <code>==</code>, for instance, or to avoid calling the <code>==</code> method entirely,
 * such as if you want to compare <code>Double</code>s with a tolerance.
 * For an example, see the main documentation of trait <code>org.scalactic.Equality</code>.
 * </p>
 *
 * <p>
 * You can always supply implicit parameters explicitly, but in the case of implicit parameters of type <code>Equality[T]</code>, Scalactic provides a
 * simple "explictly" DSL. For example, here's how you could explicitly supply an <code>Equality[String]</code> instance that normalizes both left and right
 * sides (which must be strings), by transforming them to lowercase:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.matchers.should.Matchers._
 * import org.scalatest.matchers.should.Matchers._
 *
 * scala&gt; import org.scalactic.Explicitly._
 * import org.scalactic.Explicitly._
 *
 * scala&gt; import org.scalactic.StringNormalizations._
 * import org.scalactic.StringNormalizations._
 *
 * scala&gt; "Hi" should equal ("hi") (after being lowerCased)
 * </pre>
 *
 * <p>
 * The <code>after</code> <code>being</code> <code>lowerCased</code> expression results in an <code>Equality[String]</code>, which is then passed
 * explicitly as the second curried parameter to <code>equal</code>. For more information on the explictly DSL, see the main documentation
 * for trait <code>org.scalactic.Explicitly</code>.
 * </p>
 *
 * <p>
 * The "<code>should</code> <code>be</code>" and <code>shouldBe</code> syntax do not take an <code>Equality[T]</code> and can therefore not be customized.
 * They always use the default approach to equality described above. As a result, "<code>should</code> <code>be</code>" and <code>shouldBe</code> will
 * likely be the fastest-compiling matcher syntax for equality comparisons, since the compiler need not search for
 * an implicit <code>Equality[T]</code> each time.
 * </p>
 *
 * <p>
 * The <code>should</code> <code>===</code> syntax (and its complement, <code>should</code> <code>!==</code>) can be used to enforce type
 * constraints at compile-time between the left and right sides of the equality comparison. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.matchers.should.Matchers._
 * import org.scalatest.matchers.should.Matchers._
 *
 * scala&gt; import org.scalactic.TypeCheckedTripleEquals._
 * import org.scalactic.TypeCheckedTripleEquals._
 *
 * scala&gt; Some(2) should === (2)
 * &lt;console&gt;:17: error: types Some[Int] and Int do not adhere to the equality constraint
 * selected for the === and !== operators; the missing implicit parameter is of
 * type org.scalactic.CanEqual[Some[Int],Int]
 *               Some(2) should === (2)
 *                       ^
 * </pre>
 *
 * <p>
 * By default, the "<code>Some(2)</code> <code>should</code> <code>===</code> <code>(2)</code>" statement would fail at runtime. By mixing in
 * the equality constraints provided by <code>TypeCheckedTripleEquals</code>, however, the statement fails to compile. For more information
 * and examples, see the main documentation for trait <code>org.scalactic.TypeCheckedTripleEquals</code>.
 * </p>
 *
 * <a name="checkingSizeAndLength"></a>
 * <h2>Checking size and length</h2>
 *
 * <p>
 * You can check the size or length of any type of object for which it
 * makes sense. Here's how checking for length looks:
 * </p>
 * <pre class="stHighlight">
 * result should have length 3
 * </pre>
 *
 * <p>
 * Size is similar:
 * </p>
 *
 * <pre class="stHighlight">
 * result should have size 10
 * </pre>
 *
 * <p>
 * The <code>length</code> syntax can be used with <code>String</code>, <code>Array</code>, any <code>scala.collection.GenSeq</code>,
 * any <code>java.util.List</code>, and any type <code>T</code> for which an implicit <code>Length[T]</code> type class is
 * available in scope.
 * Similarly, the <code>size</code> syntax can be used with <code>Array</code>, any <code>org.scalactic.ColCompatHelper.Iterable</code>,
 * any <code>java.util.Collection</code>, any <code>java.util.Map</code>, and any type <code>T</code> for which an implicit <code>Size[T]</code> type class is
 * available in scope. You can enable the <code>length</code> or <code>size</code> syntax for your own arbitrary types, therefore,
 * by defining <a href="../../enablers/Length.html"><code>Length</code></a> or <a href="../../enablers/Size.html"><code>Size</code></a> type
 * classes for those types.
 * </p>
 *
 * <p>
 * In addition, the <code>length</code> syntax can be used with any object that has a field or method named <code>length</code>
 * or a method named <code>getLength</code>.   Similarly, the <code>size</code> syntax can be used with any
 * object that has a field or method named <code>size</code> or a method named <code>getSize</code>.
 * The type of a <code>length</code> or <code>size</code> field, or return type of a method, must be either <code>Int</code>
 * or <code>Long</code>. Any such method must take no parameters. (The Scala compiler will ensure at compile time that
 * the object on which <code>should</code> is being invoked has the appropriate structure.)
 * </p>
 *
 * <a name="checkingStrings"></a>
 * <h2>Checking strings</h2>
 *
 * <p>
 * You can check for whether a string starts with, ends with, or includes a substring like this:
 * </p>
 *
 * <pre class="stHighlight">
 * string should startWith ("Hello")
 * string should endWith ("world")
 * string should include ("seven")
 * </pre>
 *
 * <p>
 * You can check for whether a string starts with, ends with, or includes a regular expression, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * string should startWith regex "Hel*o"
 * string should endWith regex "wo.ld"
 * string should include regex "wo.ld"
 * </pre>
 *
 * <p>
 * And you can check whether a string fully matches a regular expression, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * string should fullyMatch regex """(-)?(\d+)(\.\d*)?"""
 * </pre>
 *
 * <p>
 * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
 * or a <code>scala.util.matching.Regex</code>.
 * </p>
 *
 * <p>
 * With the <code>startWith</code>, <code>endWith</code>, <code>include</code>, and <code>fullyMatch</code>
 * tokens can also be used with an optional specification of required groups, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * "abbccxxx" should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
 * "xxxabbcc" should endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
 * "xxxabbccxxx" should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
 * "abbcc" should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
 * </pre>
 *
 * <p>
 * You can check whether a string is empty with <code>empty</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * s shouldBe empty
 * </pre>
 *
 * <p>
 * You can also use most of ScalaTest's matcher syntax for collections on <code>String</code> by
 * treating the <code>String</code>s as collections of characters. For examples, see the
 * <a href="#stringsAndArraysAsCollections"><code>String</code>s and <code>Array</code>s as collections</a> section below.
 * </p>
 *
 * <a name="greaterAndLessThan"></a>
 * <h2>Greater and less than</h2>
 *
 * <p>
 * You can check whether any type for which an implicit <code>Ordering[T]</code> is available
 * is greater than, less than, greater than or equal, or less
 * than or equal to a value of type <code>T</code>. The syntax is:
 * </p>
 * <pre class="stHighlight">
 * one should be &lt; 7
 * one should be &gt; 0
 * one should be &lt;= 7
 * one should be &gt;= 0
 * </pre>
 *
 * <a name="checkingBooleanPropertiesWithBe"></a>
 * <h2>Checking <code>Boolean</code> properties with <code>be</code></h2>
 *
 * <p>
 * If an object has a method that takes no parameters and returns boolean, you can check
 * it by placing a <code>Symbol</code> (after <code>be</code>) that specifies the name
 * of the method (excluding an optional prefix of "<code>is</code>"). A symbol literal
 * in Scala begins with a tick mark and ends at the first non-identifier character. Thus,
 * <code>'traversableAgain</code> results in a <code>Symbol</code> object at runtime, as does
 * <code>'completed</code> and <code>'file</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * iter shouldBe 'traversableAgain
 * </pre>
 *
 * Given this code, ScalaTest will use reflection to look on the object referenced from
 * <code>emptySet</code> for a method that takes no parameters and results in <code>Boolean</code>,
 * with either the name <code>empty</code> or <code>isEmpty</code>. If found, it will invoke
 * that method. If the method returns <code>true</code>, execution will continue. But if it returns
 * <code>false</code>, a <code>TestFailedException</code> will be thrown that will contain a detail message, such as:
 *
 * <pre class="stHighlight">
 * non-empty iterator was not traversableAgain
 * </pre>
 *
 * <p>
 * This <code>be</code> syntax can be used with any reference (<code>AnyRef</code>) type.  If the object does
 * not have an appropriately named predicate method, you'll get a <code>TestFailedException</code>
 * at runtime with a detailed message that explains the problem.
 * (For the details on how a field or method is selected during this
 * process, see the documentation for <a href="matchers.dsl/BeWord.html"><code>BeWord</code></a>.)
 * </p>
 *
 * <p>
 * If you think it reads better, you can optionally put <code>a</code> or <code>an</code> after
 * <code>be</code>. For example, <code>java.io.File</code> has two predicate methods,
 * <code>isFile</code> and <code>isDirectory</code>. Thus with a <code>File</code> object
 * named <code>temp</code>, you could write:
 * </p>
 *
 * <pre class="stHighlight">
 * temp should be a 'file
 * </pre>
 *
 * <p>
 * Or, given <code>java.awt.event.KeyEvent</code> has a method <code>isActionKey</code> that takes
 * no arguments and returns <code>Boolean</code>, you could assert that a <code>KeyEvent</code> is
 * an action key with:
 *</p>
 *
 * <pre class="stHighlight">
 * keyEvent should be an 'actionKey
 * </pre>
 *
 * <p>
 * If you prefer to check <code>Boolean</code> properties in a type-safe manner, you can use a <code>BePropertyMatcher</code>.
 * This would allow you to write expressions such as:
 * </p>
 *
 * <pre class="stHighlight">
 * xs shouldBe traversableAgain
 * temp should be a file
 * keyEvent should be an actionKey
 * </pre>
 *
 * <p>
 * These expressions would fail to compile if <code>should</code> is used on an inappropriate type, as determined
 * by the type parameter of the <code>BePropertyMatcher</code> being used. (For example, <code>file</code> in this example
 * would likely be of type <code>BePropertyMatcher[java.io.File]</code>. If used with an appropriate type, such an expression will compile
 * and at run time the <code>Boolean</code> property method or field will be accessed directly; <em>i.e.</em>, no reflection will be used.
 * See the documentation for <a href="matchers/BePropertyMatcher.html"><code>BePropertyMatcher</code></a> for more information.
 * </p>
 *
 * <a name="usingCustomBeMatchers"></a>
 * <h2>Using custom <code>BeMatchers</code></h2>
 *
 * If you want to create a new way of using <code>be</code>, which doesn't map to an actual property on the
 * type you care about, you can create a <code>BeMatcher</code>. You could use this, for example, to create <code>BeMatcher[Int]</code>
 * called <code>odd</code>, which would match any odd <code>Int</code>, and <code>even</code>, which would match
 * any even <code>Int</code>.
 * Given this pair of <code>BeMatcher</code>s, you could check whether an <code>Int</code> was odd or even with expressions like:
 * </p>
 *
 * <pre class="stHighlight">
 * num shouldBe odd
 * num should not be even
 * </pre>
 *
 * For more information, see the documentation for <a href="matchers/BeMatcher.html"><code>BeMatcher</code></a>.
 *
 * <a name="checkingObjectIdentity"></a>
 * <h2>Checking object identity</h2>
 *
 * <p>
 * If you need to check that two references refer to the exact same object, you can write:
 * </p>
 *
 * <pre class="stHighlight">
 * ref1 should be theSameInstanceAs ref2
 * </pre>
 *
 * <a name="checkingAnObjectsClass"></a>
 * <h2>Checking an object's class</h2>
 *
 * <p>
 * If you need to check that an object is an instance of a particular class or trait, you can supply the type to
 * &ldquo;<code>be</code> <code>a</code>&rdquo; or &ldquo;<code>be</code> <code>an</code>&rdquo;:
 * </p>
 *
 * <pre class="stHighlight">
 * result1 shouldBe a [Tiger]
 * result1 should not be an [Orangutan]
 * </pre>
 *
 * <p>
 * Because type parameters are erased on the JVM, we recommend you insert an underscore for any type parameters
 * when using this syntax. Both of the following test only that the result is an instance of <code>List[_]</code>, because at
 * runtime the type parameter has been erased:
 * </p>
 *
 * <pre class="stHighlight">
 * result shouldBe a [List[_]] // recommended
 * result shouldBe a [List[Fruit]] // discouraged
 * </pre>
 *
 * <a name="checkingNumbersAgainstARange"></a>
 * <h2>Checking numbers against a range</h2>
 *
 * <p>
 * Often you may want to check whether a number is within a
 * range. You can do that using the <code>+-</code> operator, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * sevenDotOh should equal (6.9 +- 0.2)
 * sevenDotOh should === (6.9 +- 0.2)
 * sevenDotOh should be (6.9 +- 0.2)
 * sevenDotOh shouldEqual 6.9 +- 0.2
 * sevenDotOh shouldBe 6.9 +- 0.2
 * </pre>
 *
 * <p>
 * Any of these expressions will cause a <code>TestFailedException</code> to be thrown if the floating point
 * value, <code>sevenDotOh</code> is outside the range <code>6.7</code> to <code>7.1</code>.
 * You can use <code>+-</code> with any type <code>T</code> for which an implicit <code>Numeric[T]</code> exists, such as integral types:
 * </p>
 *
 * <pre class="stHighlight">
 * seven should equal (6 +- 2)
 * seven should === (6 +- 2)
 * seven should be (6 +- 2)
 * seven shouldEqual 6 +- 2
 * seven shouldBe 6 +- 2
 * </pre>
 *
 * <a name="checkingForEmptiness"></a>
 * <h2>Checking for emptiness</h2>
 *
 * <p>
 * You can check whether an object is "empty", like this:
 * </p>
 *
 * <pre class="stHighlight">
 * traversable shouldBe empty
 * javaMap should not be empty
 * </pre>
 *
 * <p>
 * The <code>empty</code> token can be used with any type <code>L</code> for which an implicit <code>Emptiness[L]</code> exists.
 * The <code>Emptiness</code> companion object provides implicits for <code>Iterable[E]</code>, <code>java.util.Collection[E]</code>,
 * <code>java.util.Map[K, V]</code>, <code>String</code>, <code>Array[E]</code>, and <code>Option[E]</code>. In addition, the
 * <code>Emptiness</code> companion object provides structural implicits for types that declare an <code>isEmpty</code> method that
 * returns a <code>Boolean</code>. Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.matchers.should.Matchers._
 * import org.scalatest.matchers.should.Matchers._
 *
 * scala&gt; List.empty shouldBe empty
 *
 * scala&gt; None shouldBe empty
 *
 * scala&gt; Some(1) should not be empty
 *
 * scala&gt; "" shouldBe empty
 *
 * scala&gt; new java.util.HashMap[Int, Int] shouldBe empty
 *
 * scala&gt; new { def isEmpty = true} shouldBe empty
 *
 * scala&gt; Array(1, 2, 3) should not be empty
 * </pre>
 *
 * <a name="workingWithContainers"></a>
 * <h2>Working with "containers"</h2>
 *
 * <p>
 * You can check whether a collection contains a particular element like this:
 * </p>
 *
 * <pre class="stHighlight">
 * traversable should contain ("five")
 * </pre>
 *
 * <p>
 * The <code>contain</code> syntax shown above can be used with any type <code>C</code> that has a "containing" nature, evidenced by
 * an implicit <code>org.scalatest.enablers.Containing[L]</code>, where <code>L</code> is left-hand type on
 * which <code>should</code> is invoked. In the <code>Containing</code>
 * companion object, implicits are provided for types <code>Iterable[E]</code>, <code>java.util.Collection[E]</code>,
 * <code>java.util.Map[K, V]</code>, <code>String</code>, <code>Array[E]</code>, and <code>Option[E]</code>.
 * Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.matchers.should.Matchers._
 * import org.scalatest.matchers.should.Matchers._
 *
 * scala&gt; List(1, 2, 3) should contain (2)
 *
 * scala&gt; Map('a' -&gt; 1, 'b' -&gt; 2, 'c' -&gt; 3) should contain ('b' -&gt; 2)
 *
 * scala&gt; Set(1, 2, 3) should contain (2)
 *
 * scala&gt; Array(1, 2, 3) should contain (2)
 *
 * scala&gt; "123" should contain ('2')
 *
 * scala&gt; Some(2) should contain (2)
 * </pre>
 *
 * <p>
 * ScalaTest's implicit methods that provide the <code>Containing[L]</code> type classes require an <code>Equality[E]</code>, where
 * <code>E</code> is an element type. For example, to obtain a <code>Containing[Array[Int]]</code> you must supply an <code>Equality[Int]</code>,
 * either implicitly or explicitly. The <code>contain</code> syntax uses this <code>Equality[E]</code> to determine containership.
 * Thus if you want to change how containership is determined for an element type <code>E</code>, place an implicit <code>Equality[E]</code>
 * in scope or use the explicitly DSL. Although the implicit parameter required for the <code>contain</code> syntax is of type <code>Containing[L]</code>,
 * implicit conversions are provided in the <code>Containing</code> companion object from <code>Equality[E]</code> to the various
 * types of containers of <code>E</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.matchers.should.Matchers._
 * import org.scalatest.matchers.should.Matchers._
 *
 * scala&gt; List("Hi", "Di", "Ho") should contain ("ho")
 * org.scalatest.exceptions.TestFailedException: List(Hi, Di, Ho) did not contain element "ho"
 *         at ...
 *
 * scala&gt; import org.scalactic.Explicitly._
 * import org.scalactic.Explicitly._
 *
 * scala&gt; import org.scalactic.StringNormalizations._
 * import org.scalactic.StringNormalizations._
 *
 * scala&gt; (List("Hi", "Di", "Ho") should contain ("ho")) (after being lowerCased)
 * </pre>
 *
 * <p>
 * Note that when you use the explicitly DSL with <code>contain</code> you need to wrap the entire
 * <code>contain</code> expression in parentheses, as shown here.
 * </p>
 *
 * <pre>
 * (List("Hi", "Di", "Ho") should contain ("ho")) (after being lowerCased)
 * ^                                            ^
 * </pre>
 *
 * <p>
 * In addition to determining whether an object contains another object, you can use <code>contain</code> to
 * make other determinations.
 * For example, the <code>contain</code> <code>oneOf</code> syntax ensures that one and only one of the specified elements are
 * contained in the containing object:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain oneOf (5, 7, 9)
 * Some(7) should contain oneOf (5, 7, 9)
 * "howdy" should contain oneOf ('a', 'b', 'c', 'd')
 * </pre>
 *
 * <p>
 * Note that if multiple specified elements appear in the containing object, <code>oneOf</code> will fail:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; List(1, 2, 3) should contain oneOf (2, 3, 4)
 * org.scalatest.exceptions.TestFailedException: List(1, 2, 3) did not contain one (and only one) of (2, 3, 4)
 *         at ...
 * </pre>
 *
 * <p>
 * If you really want to ensure one or more of the specified elements are contained in the containing object,
 * use <code>atLeastOneOf</code>, described below, instead of <code>oneOf</code>. Keep in mind, <code>oneOf</code>
 * means "<em>exactly</em> one of."
 * </p>
 *
 * <p>
 * Note also that with any <code>contain</code> syntax, you can place custom implicit <code>Equality[E]</code> instances in scope
 * to customize how containership is determined, or use the explicitly DSL. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * (Array("Doe", "Ray", "Me") should contain oneOf ("X", "RAY", "BEAM")) (after being lowerCased)
 * </pre>
 *
 * <p>
 * If you have a collection of elements that you'd like to use in a "one of" comparison, you can use "oneElementOf," like this:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain oneElementOf List(5, 7, 9)
 * Some(7) should contain oneElementOf Vector(5, 7, 9)
 * "howdy" should contain oneElementOf Set('a', 'b', 'c', 'd')
 * (Array("Doe", "Ray", "Me") should contain oneElementOf List("X", "RAY", "BEAM")) (after being lowerCased)
 * </pre>
 *
 * <p>
 * The <code>contain</code> <code>noneOf</code> syntax does the opposite of <code>oneOf</code>: it ensures none of the specified elements
 * are contained in the containing object:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain noneOf (7, 8, 9)
 * Some(0) should contain noneOf (7, 8, 9)
 * "12345" should contain noneOf ('7', '8', '9')
 * </pre>
 *
 * <p>
 * If you have a collection of elements that you'd like to use in a "none of" comparison, you can use "noElementsOf," like this:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain noElementsOf List(7, 8, 9)
 * Some(0) should contain noElementsOf Vector(7, 8, 9)
 * "12345" should contain noElementsOf Set('7', '8', '9')
 * </pre>
 *
 * <a name="workingWithAggregations"></a>
 * <h2>Working with "aggregations"</h2>
 *
 * <p>
 * As mentioned, the "<code>contain</code>,"  "<code>contain</code> <code>oneOf</code>," and "<code>contain</code> <code>noneOf</code>" syntax requires a
 * <code>Containing[L]</code> be provided, where <code>L</code> is the left-hand type.  Other <code>contain</code> syntax, which
 * will be described in this section, requires an <code>Aggregating[L]</code> be provided, where again <code>L</code> is the left-hand type.
 * (An <code>Aggregating[L]</code> instance defines the "aggregating nature" of a type <code>L</code>.)
 * The reason, essentially, is that <code>contain</code> syntax that makes sense for <code>Option</code> is enabled by
 * <code>Containing[L]</code>, whereas syntax that does <em>not</em> make sense for <code>Option</code> is enabled
 * by <code>Aggregating[L]</code>. For example, it doesn't make sense to assert that an <code>Option[Int]</code> contains all of a set of integers, as it
 * could only ever contain one of them. But this does make sense for a type such as <code>List[Int]</code> that can aggregate zero to many integers.
 * </p>
 *
 * <p>
 * The <code>Aggregating</code> companion object provides implicit instances of <code>Aggregating[L]</code>
 * for types <code>Iterable[E]</code>, <code>java.util.Collection[E]</code>,
 * <code>java.util.Map[K, V]</code>, <code>String</code>, <code>Array[E]</code>. Note that these are the same types as are supported with
 * <code>Containing</code>, but with <code>Option[E]</code> missing.
 * Here are some examples:
 * </p>
 *
 * <p>
 * The <code>contain</code> <code>atLeastOneOf</code> syntax, for example, works for any type <code>L</code> for which an <code>Aggregating[L]</code> exists. It ensures
 * that at least one of (<em>i.e.</em>, one or more of) the specified objects are contained in the containing object:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3) should contain atLeastOneOf (2, 3, 4)
 * Array(1, 2, 3) should contain atLeastOneOf (3, 4, 5)
 * "abc" should contain atLeastOneOf ('c', 'a', 't')
 * </pre>
 *
 * <p>
 * Similar to <code>Containing[L]</code>, the implicit methods that provide the <code>Aggregating[L]</code> instances require an <code>Equality[E]</code>, where
 * <code>E</code> is an element type. For example, to obtain a <code>Aggregating[Vector[String]]</code> you must supply an <code>Equality[String]</code>,
 * either implicitly or explicitly. The <code>contain</code> syntax uses this <code>Equality[E]</code> to determine containership.
 * Thus if you want to change how containership is determined for an element type <code>E</code>, place an implicit <code>Equality[E]</code>
 * in scope or use the explicitly DSL. Although the implicit parameter required for the <code>contain</code> syntax is of type <code>Aggregating[L]</code>,
 * implicit conversions are provided in the <code>Aggregating</code> companion object from <code>Equality[E]</code> to the various
 * types of aggregations of <code>E</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * (Vector(" A", "B ") should contain atLeastOneOf ("a ", "b", "c")) (after being lowerCased and trimmed)
 * </pre>
 *
 * <p>
 * If you have a collection of elements that you'd like to use in an "at least one of" comparison, you can use "atLeastOneElementOf," like this:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3) should contain atLeastOneElementOf List(2, 3, 4)
 * Array(1, 2, 3) should contain atLeastOneElementOf Vector(3, 4, 5)
 * "abc" should contain atLeastOneElementOf Set('c', 'a', 't')
 * (Vector(" A", "B ") should contain atLeastOneElementOf List("a ", "b", "c")) (after being lowerCased and trimmed)
 * </pre>
 *
 * <p>
 * The "<code>contain</code> <code>atMostOneOf</code>" syntax lets you specify a set of objects at most one of which should be contained in the containing object:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain atMostOneOf (5, 6, 7)
 * </pre>
 *
 * <p>
 * If you have a collection of elements that you'd like to use in a "at most one of" comparison, you can use "atMostOneElementOf," like this:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain atMostOneElementOf Vector(5, 6, 7)
 * </pre>
 *
 * <p>
 * The "<code>contain</code> <code>allOf</code>" syntax lets you specify a set of objects that should all be contained in the containing object:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain allOf (2, 3, 5)
 * </pre>
 *
 * <p>
 * If you have a collection of elements that you'd like to use in a "all of" comparison, you can use "allElementsOf," like this:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain allElementsOf Array(2, 3, 5)
 * </pre>
 *
 * <p>
 * The "<code>contain</code> <code>only</code>" syntax lets you assert that the containing object contains <em>only</em> the specified objects, though it may
 * contain more than one of each:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 2, 1) should contain only (1, 2, 3)
 * </pre>
 *
 * <p>
 * The "<code>contain</code> <code>theSameElementsAs</code>" and "<code>contain</code> <code>theSameElementsInOrderAs</code> syntax differ from the others
 * in that the right hand side is a <code>Iterable[_]</code> rather than a varargs of <code>Any</code>. (Note: in a future 2.0 milestone release, possibly
 * 2.0.M6, these will likely be widened to accept any type <code>R</code> for which an <code>Aggregating[R]</code> exists.)
 * </p>
 *
 * <p>
 * The "<code>contain</code> <code>theSameElementsAs</code>" syntax lets you assert that two aggregations contain the same objects:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 2, 3, 3, 3) should contain theSameElementsAs Vector(3, 2, 3, 1, 2, 3)
 * </pre>
 *
 * <p>
 * The number of times any family of equal objects appears must also be the same in both the left and right aggregations.
 * The specified objects may appear multiple times, but must appear in the order they appear in the right-hand list. For example, if
 * the last 3 element is left out of the right-hand list in the previous example, the expression would fail because the left side
 * has three 3's and the right hand side has only two:
 * </p>
 *
 * <pre class="stREPL">
 * List(1, 2, 2, 3, 3, 3) should contain theSameElementsAs Vector(3, 2, 3, 1, 2)
 * org.scalatest.exceptions.TestFailedException: List(1, 2, 2, 3, 3, 3) did not contain the same elements as Vector(3, 2, 3, 1, 2)
 *         at ...
 * </pre>
 *
 * <p>
 * Note that no <code>onlyElementsOf</code> matcher is provided, because it would have the same
 * behavior as <code>theSameElementsAs</code>. (<em>I.e.</em>, if you were looking for <code>onlyElementsOf</code>, please use <code>theSameElementsAs</code>
 * instead.)
 * </p>
 *
 * </p>
 * <a name="workingWithSequences"></a>
 * <h2>Working with "sequences"</h2>
 *
 * <p>
 * The rest of the <code>contain</code> syntax, which
 * will be described in this section, requires a <code>Sequencing[L]</code> be provided, where again <code>L</code> is the left-hand type.
 * (A <code>Sequencing[L]</code> instance defines the "sequencing nature" of a type <code>L</code>.)
 * The reason, essentially, is that <code>contain</code> syntax that implies an "order" of elements makes sense only for types that place elements in a sequence.
 * For example, it doesn't make sense to assert that a <code>Map[String, Int]</code> or <code>Set[Int]</code> contains all of a set of integers in a particular
 * order, as these types don't necessarily define an order for their elements. But this does make sense for a type such as <code>Seq[Int]</code> that does define
 * an order for its elements.
 * </p>
 *
 * <p>
 * The <code>Sequencing</code> companion object provides implicit instances of <code>Sequencing[L]</code>
 * for types <code>GenSeq[E]</code>, <code>java.util.List[E]</code>,
 * <code>String</code>, and <code>Array[E]</code>.
 * Here are some examples:
 * </p>
 *
 * <p>
 * Similar to <code>Containing[L]</code>, the implicit methods that provide the <code>Aggregating[L]</code> instances require an <code>Equality[E]</code>, where
 * <code>E</code> is an element type. For example, to obtain a <code>Aggregating[Vector[String]]</code> you must supply an <code>Equality[String]</code>,
 * either implicitly or explicitly. The <code>contain</code> syntax uses this <code>Equality[E]</code> to determine containership.
 * Thus if you want to change how containership is determined for an element type <code>E</code>, place an implicit <code>Equality[E]</code>
 * in scope or use the explicitly DSL. Although the implicit parameter required for the <code>contain</code> syntax is of type <code>Aggregating[L]</code>,
 * implicit conversions are provided in the <code>Aggregating</code> companion object from <code>Equality[E]</code> to the various
 * types of aggregations of <code>E</code>. Here's an example:
 * </p>
 *
 * <p>
 * The "<code>contain</code> <code>inOrderOnly</code>" syntax lets you assert that the containing object contains <em>only</em> the specified objects, in order.
 * The specified objects may appear multiple times, but must appear in the order they appear in the right-hand list. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 2, 3, 3, 3) should contain inOrderOnly (1, 2, 3)
 * </pre>
 *
 * <p>
 * The "<code>contain</code> <code>inOrder</code>" syntax lets you assert that the containing object contains <em>only</em> the specified objects in order, like
 * <code>inOrderOnly</code>, but allows other objects to appear in the left-hand aggregation as well:
 * contain more than one of each:
 * </p>
 *
 * <pre class="stHighlight">
 * List(0, 1, 2, 2, 99, 3, 3, 3, 5) should contain inOrder (1, 2, 3)
 * </pre>
 *
 * <p>
 * If you have a collection of elements that you'd like to use in a "in order" comparison, you can use "inOrderElementsOf," like this:
 * </p>
 *
 * <pre class="stHighlight">
 * List(0, 1, 2, 2, 99, 3, 3, 3, 5) should contain inOrderElementsOf Array(1, 2, 3)
 * </pre>
 *
 * <p>
 * Note that "order" in <code>inOrder</code>, <code>inOrderOnly</code>, and <code>theSameElementsInOrderAs</code> (described below)
 * in the <code>Aggregation[L]</code> instances built-in to ScalaTest is defined as "iteration order".
 * </p>
 *
 * <p>
 * Lastly, the "<code>contain</code> <code>theSameElementsInOrderAs</code>" syntax lets you assert that two aggregations contain
 * the same exact elements in the same (iteration) order:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3) should contain theSameElementsInOrderAs collection.mutable.TreeSet(3, 2, 1)
 * </pre>
 *
 * <p>
 * The previous assertion succeeds because the iteration order of a<code>TreeSet</code> is the natural
 * ordering of its elements, which in this case is 1, 2, 3. An iterator obtained from the left-hand <code>List</code> will produce the same elements
 * in the same order.
 * </p>
 *
 * <p>
 * Note that no <code>inOrderOnlyElementsOf</code> matcher is provided, because it would have the same
 * behavior as <code>theSameElementsInOrderAs</code>. (<em>I.e.</em>, if you were looking for <code>inOrderOnlyElementsOf</code>, please use <code>theSameElementsInOrderAs</code>
 * instead.)
 * </p>
 *
 * <a name="workingWithSortables"></a>
 * <h2>Working with "sortables"</h2>
 *
 * <p>
 * You can also ask whether the elements of "sortable" objects (such as <code>Array</code>s, Java <code>List</code>s, and <code>GenSeq</code>s)
 * are in sorted order, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3) shouldBe sorted
 * </pre>
 *
 * <a name="workingWithIterators"></a>
 * <h2>Working with iterators</h2>
 *
 * <p>
 * Although it seems desireable to provide similar matcher syntax for Scala and Java iterators to that provided for sequences like
 * <code>Seq</code>s, <code>Array</code>, and <code>java.util.List</code>, the
 * ephemeral nature of iterators makes this problematic. Some syntax (such as <code>should</code> <code>contain</code>) is relatively straightforward to
 * support on iterators, but other syntax (such
 * as, for example, <code>Inspector</code> expressions on nested iterators) is not. Rather
 * than allowing inconsistencies between sequences and iterators in the API, we chose to not support any such syntax directly on iterators:
 *
 * <pre class="stHighlight">
 * scala&gt; val it = List(1, 2, 3).iterator
 * it: Iterator[Int] = non-empty iterator
 *
 * scala&gt; it should contain (2)
 * &lt;console&gt;:15: error: could not find implicit value for parameter typeClass1: org.scalatest.enablers.Containing[Iterator[Int]]
 *            it should contain (2)
 *               ^
 * </pre>
 *
 * <p>
 * Instead, you will need to convert your iterators to a sequence explicitly before using them in matcher expressions:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; it.toStream should contain (2)
 * </pre>
 *
 * <p>
 * We recommend you convert (Scala or Java) iterators to <code>Stream</code>s, as shown in the previous example, so that you can
 * continue to reap any potential benefits provided by the laziness of the underlying iterator.
 * </p>
 *
 * <a name="inspectorShorthands"></a>
 * <h2>Inspector shorthands</h2>
 *
 * <p>
 * You can use the <a href="../../Inspectors.html"><code>Inspectors</code></a> syntax with matchers as well as assertions. If you have a multi-dimensional collection, such as a
 * list of lists, using <code>Inspectors</code> is your best option:
 * </p>
 *
 * <pre class="stHighlight">
 * val yss =
 *   List(
 *     List(1, 2, 3),
 *     List(1, 2, 3),
 *     List(1, 2, 3)
 *   )
 *
 * forAll (yss) { ys =&gt;
 *   forAll (ys) { y =&gt; y should be &gt; 0 }
 * }
 * </pre>
 *
 * <p>
 * For assertions on one-dimensional collections, however, matchers provides "inspector shorthands." Instead of writing:
 * </p>
 *
 * <pre class="stHighlight">
 * val xs = List(1, 2, 3)
 * forAll (xs) { x =&gt; x should be &lt; 10 }
 * </pre>
 *
 * <p>
 * You can write:
 * </p>
 *
 * <pre class="stHighlight">
 * all (xs) should be &lt; 10
 * </pre>
 *
 * <p>
 * The previous statement asserts that all elements of the <code>xs</code> list should be less than 10.
 * All of the inspectors have shorthands in matchers. Here is the full list:
 * </p>
 *
 * <ul>
 * <li><code>all</code> - succeeds if the assertion holds true for every element</li>
 * <li><code>atLeast</code> - succeeds if the assertion holds true for at least the specified number of elements</li>
 * <li><code>atMost</code> - succeeds if the assertion holds true for at most the specified number of elements</li>
 * <li><code>between</code> - succeeds if the assertion holds true for between the specified minimum and maximum number of elements, inclusive</li>
 * <li><code>every</code> - same as <code>all</code>, but lists all failing elements if it fails (whereas <code>all</code> just reports the first failing element)</li>
 * <li><code>exactly</code> - succeeds if the assertion holds true for exactly the specified number of elements</li>
 * </ul>
 *
 * <p>
 * Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.matchers.should.Matchers._
 * import org.scalatest.matchers.should.Matchers._
 *
 * scala&gt; val xs = List(1, 2, 3, 4, 5)
 * xs: List[Int] = List(1, 2, 3, 4, 5)
 *
 * scala&gt; all (xs) should be &gt; 0
 *
 * scala&gt; atMost (2, xs) should be &gt;= 4
 *
 * scala&gt; atLeast (3, xs) should be &lt; 5
 *
 * scala&gt; between (2, 3, xs) should (be &gt; 1 and be &lt; 5)
 *
 * scala&gt; exactly (2, xs) should be &lt;= 2
 *
 * scala&gt; every (xs) should be &lt; 10
 *
 * scala&gt; // And one that fails...
 *
 * scala&gt; exactly (2, xs) shouldEqual 2
 * org.scalatest.exceptions.TestFailedException: 'exactly(2)' inspection failed, because only 1 element
 *     satisfied the assertion block at index 1:
 *   at index 0, 1 did not equal 2,
 *   at index 2, 3 did not equal 2,
 *   at index 3, 4 did not equal 2,
 *   at index 4, 5 did not equal 2
 * in List(1, 2, 3, 4, 5)
 *         at ...
 * </pre>
 *
 * <p>
 * Like <a href=""><code>Inspectors</code></a>, objects used with inspector shorthands can be any type <code>T</code> for which a <code>Collecting[T, E]</code>
 * is availabe, which by default includes <code>Iterable</code>,
 * Java <code>Collection</code>, Java <code>Map</code>, <code>Array</code>s, and <code>String</code>s.
 * Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import matchers.should.Matchers._
 * import matchers.should.Matchers._
 *
 * scala&gt; all (Array(1, 2, 3)) should be &lt; 5
 *
 * scala&gt; import collection.JavaConverters._
 * import collection.JavaConverters._
 *
 * scala&gt; val js = List(1, 2, 3).asJava
 * js: java.util.List[Int] = [1, 2, 3]
 *
 * scala&gt; all (js) should be &lt; 5
 *
 * scala&gt; val jmap = Map("a" -&gt; 1, "b" -&gt; 2).asJava
 * jmap: java.util.Map[String,Int] = {a=1, b=2}
 *
 * scala&gt; atLeast(1, jmap) shouldBe Entry("b", 2)
 *
 * scala&gt; atLeast(2, "hello, world!") shouldBe 'o'
 * </pre>
 *
 * <a name="singleElementCollections"></a>
 * <h2>Single-element collections</h2>
 *
 * <p>
 * To assert both that a collection contains just one "lone" element as well as something else about that element, you can use
 * the <code>loneElement</code> syntax provided by trait <a href="../../LoneElement.html"><code>LoneElement</code></a>. For example, if a
 * <code>Set[Int]</code> should contain just one element, an <code>Int</code>
 * less than or equal to 10, you could write:
 * </p>
 *
 * <pre class="stHighlight">
 * import LoneElement._
 * set.loneElement should be &lt;= 10
 * </pre>
 *
 * <p>
 * You can invoke <code>loneElement</code> on any type <code>T</code> for which an implicit <a href="../../enablers/Collecting.html"><code>Collecting[E, T]</code></a>
 * is available, where <code>E</code> is the element type returned by the <code>loneElement</code> invocation. By default, you can use <code>loneElement</code>
 * on <code>Iterable</code>, Java <code>Collection</code>, Java <code>Map</code>, <code>Array</code>, and <code>String</code>.
 * </p>
 *
 * <a name="javaCollectionsAndMaps"></a>
 * <h2>Java collections and maps</h2>
 *
 * <p>
 * You can use similar syntax on Java collections (<code>java.util.Collection</code>) and maps (<code>java.util.Map</code>).
 * For example, you can check whether a Java <code>Collection</code> or <code>Map</code> is <code>empty</code>,
 * like this:
 * </p>
 *
 * <pre class="stHighlight">
 * javaCollection should be ('empty)
 * javaMap should be ('empty)
 * </pre>
 *
 * <p>
 * Even though Java's <code>List</code> type doesn't actually have a <code>length</code> or <code>getLength</code> method,
 * you can nevertheless check the length of a Java <code>List</code> (<code>java.util.List</code>) like this:
 * </p>
 *
 * <pre class="stHighlight">
 * javaList should have length 9
 * </pre>
 *
 * <p>
 * You can check the size of any Java <code>Collection</code> or <code>Map</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * javaMap should have size 20
 * javaSet should have size 90
 * </pre>
 *
 * <p>
 * In addition, you can check whether a Java <code>Collection</code> contains a particular
 * element, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * javaCollection should contain ("five")
 * </pre>
 *
 * <p>
 * One difference to note between the syntax supported on Java and Scala collections is that
 * in Java, <code>Map</code> is not a subtype of <code>Collection</code>, and does not
 * actually define an element type. You can ask a Java <code>Map</code> for an "entry set"
 * via the <code>entrySet</code> method, which will return the <code>Map</code>'s key/value pairs
 * wrapped in a set of <code>java.util.Map.Entry</code>, but a <code>Map</code> is not actually
 * a collection of <code>Entry</code>. To make Java <code>Map</code>s easier to work with, however,
 * ScalaTest matchers allows you to treat a Java <code>Map</code> as a collection of <code>Entry</code>,
 * and defines a convenience implementation of <code>java.util.Map.Entry</code> in
 * <a href="../../Entry.html"><code>org.scalatest.Entry</code></a>. Here's how you use it:
 * </p>
 *
 * <pre class="stHighlight">
 * javaMap should contain (Entry(2, 3))
 * javaMap should contain oneOf (Entry(2, 3), Entry(3, 4))
 * </pre>
 *
 * You can you alse just check whether a Java <code>Map</code> contains a particular key, or value, like this:
 *
 * <pre class="stHighlight">
 * javaMap should contain key 1
 * javaMap should contain value "Howdy"
 * </pre>
 *
 * <a name="stringsAndArraysAsCollections"></a>
 * <h2><code>String</code>s and <code>Array</code>s as collections</h2>
 *
 * <p>
 * You can also use all the syntax described above for Scala and Java collections on <code>Array</code>s and
 * <code>String</code>s. Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import matchers.should.Matchers._
 * import matchers.should.Matchers._
 *
 * scala&gt; atLeast (2, Array(1, 2, 3)) should be &gt; 1
 *
 * scala&gt; atMost (2, "halloo") shouldBe 'o'
 *
 * scala&gt; Array(1, 2, 3) shouldBe sorted
 *
 * scala&gt; "abcdefg" shouldBe sorted
 *
 * scala&gt; Array(1, 2, 3) should contain atMostOneOf (3, 4, 5)
 *
 * scala&gt; "abc" should contain atMostOneOf ('c', 'd', 'e')
 * </pre>
 *
 * <a name="beAsAnEqualityComparison"></a>
 * <h2><code>be</code> as an equality comparison</h2>
 *
 * <p>
 * All uses of <code>be</code> other than those shown previously perform an equality comparison. They work
 * the same as <code>equal</code> when it is used with default equality. This redundancy between <code>be</code> and <code>equals</code> exists in part
 * because it enables syntax that sometimes sounds more natural. For example, instead of writing:
 * </p>
 *
 * <pre class="stHighlight">
 * result should equal (null)
 * </pre>
 *
 * <p>
 * You can write:
 * </p>
 *
 * <pre class="stHighlight">
 * result should be (null)
 * </pre>
 *
 * <p>
 * (Hopefully you won't write that too much given <code>null</code> is error prone, and <code>Option</code>
 * is usually a better, well, option.)
 * As mentioned <a href="#checkingEqualityWithMatchers">previously</a>, the other difference between <code>equal</code>
 * and <code>be</code> is that <code>equal</code> delegates the equality check to an <code>Equality</code> typeclass, whereas
 * <code>be</code> always uses default equality.
 * Here are some other examples of <code>be</code> used for equality comparison:
 * </p>
 *
 * <pre class="stHighlight">
 * sum should be (7.0)
 * boring should be (false)
 * fun should be (true)
 * list should be (Nil)
 * option should be (None)
 * option should be (Some(1))
 * </pre>
 *
 * <p>
 * As with <code>equal</code> used with default equality, using <code>be</code> on arrays results in <code>deep</code> being called on both arrays prior to
 * calling <code>equal</code>. As a result,
 * the following expression would <em>not</em> throw a <a href="../../exceptions/TestFailedException.html"><code>TestFailedException</code></a>:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) should be (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * Because <code>be</code> is used in several ways in ScalaTest matcher syntax, just as it is used in many ways in English, one
 * potential point of confusion in the event of a failure is determining whether <code>be</code> was being used as an equality comparison or
 * in some other way, such as a property assertion. To make it more obvious when <code>be</code> is being used for equality, the failure
 * messages generated for those equality checks will include the word <code>equal</code> in them. For example, if this expression fails with a
 * <code>TestFailedException</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * option should be (Some(1))
 * </pre>
 *
 * <p>
 * The detail message in that <code>TestFailedException</code> will include the words <code>"equal to"</code> to signify <code>be</code>
 * was in this case being used for equality comparison:
 * </p>
 *
 * <pre class="stHighlight">
 * Some(2) was not equal to Some(1)
 * </pre>
 *
 * <a name="beingNegative"></a>
 * <h2>Being negative</h2>
 *
 * <p>
 * If you wish to check the opposite of some condition, you can simply insert <code>not</code> in the expression.
 * Here are a few examples:
 * </p>
 *
 * <pre class="stHighlight">
 * result should not be (null)
 * sum should not be &lt;= (10)
 * mylist should not equal (yourList)
 * string should not startWith ("Hello")
 * </pre>
 *
 * <a name="checkingThatCodeDoesNotCompile"></a>
 * <h2>Checking that a snippet of code does not compile</h2>
 *
 * <p>
 * Often when creating libraries you may wish to ensure that certain arrangements of code that
 * represent potential &ldquo;user errors&rdquo; do not compile, so that your library is more error resistant.
 * ScalaTest <code>Matchers</code> trait includes the following syntax for that purpose:
 * </p>
 *
 * <pre class="stHighlight">
 * "val a: String = 1" shouldNot compile
 * </pre>
 *
 * <p>
 * If you want to ensure that a snippet of code does not compile because of a type error (as opposed
 * to a syntax error), use:
 * </p>
 *
 * <pre class="stHighlight">
 * "val a: String = 1" shouldNot typeCheck
 * </pre>
 *
 * <p>
 * Note that the <code>shouldNot</code> <code>typeCheck</code> syntax will only succeed if the given snippet of code does not
 * compile because of a type error. A syntax error will still result on a thrown <code>TestFailedException</code>.
 * </p>
 *
 * <p>
 * If you want to state that a snippet of code <em>does</em> compile, you can make that
 * more obvious with:
 * </p>
 *
 * <pre class="stHighlight">
 * "val a: Int = 1" should compile
 * </pre>
 *
 * <p>
 * Although the previous three constructs are implemented with macros that determine at compile time whether
 * the snippet of code represented by the string does or does not compile, errors
 * are reported as test failures at runtime.
 * </p>
 *
 * <a name="logicalExpressions"></a>
 * <h2>Logical expressions with <code>and</code> and <code>or</code></h2>
 *
 * <p>
 * You can also combine matcher expressions with <code>and</code> and/or <code>or</code>, however,
 * you must place parentheses or curly braces around the <code>and</code> or <code>or</code> expression. For example,
 * this <code>and</code>-expression would not compile, because the parentheses are missing:
 * </p>
 *
 * <pre class="stHighlight">
 * map should contain key ("two") and not contain value (7) // ERROR, parentheses missing!
 * </pre>
 *
 * <p>
 * Instead, you need to write:
 * </p>
 *
 * <pre class="stHighlight">
 * map should (contain key ("two") and not contain value (7))
 * </pre>
 *
 * <p>
 * Here are some more examples:
 * </p>
 *
 * <pre class="stHighlight">
 * number should (be &gt; (0) and be &lt;= (10))
 * option should (equal (Some(List(1, 2, 3))) or be (None))
 * string should (
 *   equal ("fee") or
 *   equal ("fie") or
 *   equal ("foe") or
 *   equal ("fum")
 * )
 * </pre>
 *
 * <p>
 * Two differences exist between expressions composed of these <code>and</code> and <code>or</code> operators and the expressions you can write
 * on regular <code>Boolean</code>s using its <code>&amp;&amp;</code> and <code>||</code> operators. First, expressions with <code>and</code>
 * and <code>or</code> do not short-circuit. The following contrived expression, for example, would print <code>"hello, world!"</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "yellow" should (equal ("blue") and equal { println("hello, world!"); "green" })
 * </pre>
 *
 * <p>
 * In other words, the entire <code>and</code> or <code>or</code> expression is always evaluated, so you'll see any side effects
 * of the right-hand side even if evaluating
 * only the left-hand side is enough to determine the ultimate result of the larger expression. Failure messages produced by these
 * expressions will "short-circuit," however,
 * mentioning only the left-hand side if that's enough to determine the result of the entire expression. This "short-circuiting" behavior
 * of failure messages is intended
 * to make it easier and quicker for you to ascertain which part of the expression caused the failure. The failure message for the previous
 * expression, for example, would be:
 * </p>
 *
 * <pre class="stHighlight">
 * "yellow" did not equal "blue"
 * </pre>
 *
 * <p>
 * Most likely this lack of short-circuiting would rarely be noticeable, because evaluating the right hand side will usually not
 * involve a side effect. One situation where it might show up, however, is if you attempt to <code>and</code> a <code>null</code> check on a variable with an expression
 * that uses the variable, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * map should (not be (null) and contain key ("ouch"))
 * </pre>
 *
 * <p>
 * If <code>map</code> is <code>null</code>, the test will indeed fail, but with a <code>NullArgumentException</code>, not a
 * <code>TestFailedException</code>. Here, the <code>NullArgumentException</code> is the visible right-hand side effect. To get a
 * <code>TestFailedException</code>, you would need to check each assertion separately:
 * </p>
 *
 * <pre class="stHighlight">
 * map should not be (null)
 * map should contain key ("ouch")
 * </pre>
 *
 * <p>
 * If <code>map</code> is <code>null</code> in this case, the <code>null</code> check in the first expression will fail with
 * a <code>TestFailedException</code>, and the second expression will never be executed.
 * </p>
 *
 * <p>
 * The other difference with <code>Boolean</code> operators is that although <code>&amp;&amp;</code> has a higher precedence than <code>||</code>,
 * <code>and</code> and <code>or</code>
 * have the same precedence. Thus although the <code>Boolean</code> expression <code>(a || b &amp;&amp; c)</code> will evaluate the <code>&amp;&amp;</code> expression
 * before the <code>||</code> expression, like <code>(a || (b &amp;&amp; c))</code>, the following expression:
 * </p>
 *
 * <pre class="stHighlight">
 * traversable should (contain (7) or contain (8) and have size (9))
 * </pre>
 *
 * <p>
 * Will evaluate left to right, as:
 * </p>
 *
 * <pre class="stHighlight">
 * traversable should ((contain (7) or contain (8)) and have size (9))
 * </pre>
 *
 * <p>
 * If you really want the <code>and</code> part to be evaluated first, you'll need to put in parentheses, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * traversable should (contain (7) or (contain (8) and have size (9)))
 * </pre>
 *
 * <a name="workingWithOptions"></a>
 * <h2>Working with <code>Option</code>s</h2>
 *
 * <p>
 * You can work with options using ScalaTest's equality, <code>empty</code>,
 * <code>defined</code>, and <code>contain</code> syntax.
 * For example, if you wish to check whether an option is <code>None</code>, you can write any of:
 * </p>
 *
 * <pre class="stHighlight">
 * option shouldEqual None
 * option shouldBe None
 * option should === (None)
 * option shouldBe empty
 * </pre>
 *
 * <p>
 * If you wish to check an option is defined, and holds a specific value, you can write any of:
 * </p>
 *
 * <pre class="stHighlight">
 * option shouldEqual Some("hi")
 * option shouldBe Some("hi")
 * option should === (Some("hi"))
 * </pre>
 *
 * <p>
 * If you only wish to check that an option is defined, but don't care what it's value is, you can write:
 * </p>
 *
 * <pre class="stHighlight">
 * option shouldBe defined
 * </pre>
 *
 * <p>
 * If you mix in (or import the members of) <a href="../../OptionValues.html"><code>OptionValues</code></a>,
 * you can write one statement that indicates you believe an option should be defined and then say something else about its value. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest.OptionValues._
 * option.value should be &lt; 7
 * </pre>
 *
 * <p>
 * As mentioned previously, you can use also use ScalaTest's <code>contain</code>, <code>contain oneOf</code>, and
 * <code>contain noneOf</code> syntax with options:
 * </p>
 *
 * <pre class="stHighlight">
 * Some(2) should contain (2)
 * Some(7) should contain oneOf (5, 7, 9)
 * Some(0) should contain noneOf (7, 8, 9)
 * </pre>
 * </p>
 *
 * <a name="checkingArbitraryProperties"></a>
 * <h2>Checking arbitrary properties with <code>have</code></h2>
 *
 * <p>
 * Using <code>have</code>, you can check properties of any type, where a <em>property</em> is an attribute of any
 * object that can be retrieved either by a public field, method, or JavaBean-style <code>get</code>
 * or <code>is</code> method, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * book should have (
 *   'title ("Programming in Scala"),
 *   'author (List("Odersky", "Spoon", "Venners")),
 *   'pubYear (2008)
 * )
 * </pre>
 *
 * <p>
 * This expression will use reflection to ensure the <code>title</code>, <code>author</code>, and <code>pubYear</code> properties of object <code>book</code>
 * are equal to the specified values. For example, it will ensure that <code>book</code> has either a public Java field or method
 * named <code>title</code>, or a public method named <code>getTitle</code>, that when invoked (or accessed in the field case) results
 * in a the string <code>"Programming in Scala"</code>. If all specified properties exist and have their expected values, respectively,
 * execution will continue. If one or more of the properties either does not exist, or exists but results in an unexpected value,
 * a <code>TestFailedException</code> will be thrown that explains the problem. (For the details on how a field or method is selected during this
 * process, see the documentation for <a href="Matchers$HavePropertyMatcherGenerator.html"><code>HavePropertyMatcherGenerator</code></a>.)
 * </p>
 *
 * <p>
 * When you use this syntax, you must place one or more property values in parentheses after <code>have</code>, seperated by commas, where a <em>property
 * value</em> is a symbol indicating the name of the property followed by the expected value in parentheses. The only exceptions to this rule is the syntax
 * for checking size and length shown previously, which does not require parentheses. If you forget and put parentheses in, however, everything will
 * still work as you'd expect. Thus instead of writing:
 * </p>
 *
 * <pre class="stHighlight">
 * array should have length (3)
 * set should have size (90)
 * </pre>
 *
 * <p>
 * You can alternatively, write:
 * </p>
 *
 * <pre class="stHighlight">
 * array should have (length (3))
 * set should have (size (90))
 * </pre>
 *
 * <p>
 * If a property has a value different from the specified expected value, a <code>TestFailedError</code> will be thrown
 * with a detailed message that explains the problem. For example, if you assert the following on
 * a <code>book</code> whose title is <code>Moby Dick</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * book should have ('title ("A Tale of Two Cities"))
 * </pre>
 *
 * <p>
 * You'll get a <code>TestFailedException</code> with this detail message:
 * </p>
 *
 * <pre>
 * The title property had value "Moby Dick", instead of its expected value "A Tale of Two Cities",
 * on object Book("Moby Dick", "Melville", 1851)
 * </pre>
 *
 * <p>
 * If you prefer to check properties in a type-safe manner, you can use a <code>HavePropertyMatcher</code>.
 * This would allow you to write expressions such as:
 * </p>
 *
 * <pre class="stHighlight">
 * book should have (
 *   title ("Programming in Scala"),
 *   author (List("Odersky", "Spoon", "Venners")),
 *   pubYear (2008)
 * )
 * </pre>
 *
 * <p>
 * These expressions would fail to compile if <code>should</code> is used on an inappropriate type, as determined
 * by the type parameter of the <code>HavePropertyMatcher</code> being used. (For example, <code>title</code> in this example
 * might be of type <code>HavePropertyMatcher[org.publiclibrary.Book]</code>. If used with an appropriate type, such an expression will compile
 * and at run time the property method or field will be accessed directly; <em>i.e.</em>, no reflection will be used.
 * See the documentation for <a href="matchers/HavePropertyMatcher.html"><code>HavePropertyMatcher</code></a> for more information.
 * </p>
 *
 * <a name="lengthSizeHavePropertyMatchers"></a>
 * <h2>Using <code>length</code> and <code>size</code> with <code>HavePropertyMatcher</code>s</h2>
 *
 * <p>
 * If you want to use <code>length</code> or <code>size</code> syntax with your own custom <code>HavePropertyMatcher</code>s, you
 * can do so, but you must write <code>(of [&ldquo;the type&rdquo;])</code> afterwords. For example, you could write:
 * </p>
 *
 * <pre class="stHighlight">
 * book should have (
 *   title ("A Tale of Two Cities"),
 *   length (220) (of [Book]),
 *   author ("Dickens")
 * )
 * </pre>
 *
 * <p>
 * Prior to ScalaTest 2.0, &ldquo;<code>length</code> <code>(22)</code>&rdquo; yielded a <code>HavePropertyMatcher[Any, Int]</code> that used reflection to dynamically look
 * for a <code>length</code> field or <code>getLength</code> method. In ScalaTest 2.0, &ldquo;<code>length</code> <code>(22)</code>&rdquo; yields a
 * <code>MatcherFactory1[Any, Length]</code>, so it is no longer a <code>HavePropertyMatcher</code>. The <code>(of [&lt;type&gt;])</code> syntax converts the
 * the <code>MatcherFactory1[Any, Length]</code> to a <code>HavePropertyMatcher[&lt;type&gt;, Int]</code>.
 * </p>
 *
 * <a name="matchingAPattern"></a>
 * <h2>Checking that an expression matches a pattern</h2>
 *
 * <p>
 * ScalaTest's <a href="../../Inside.html"><code>Inside</code></a> trait allows you to make assertions after a pattern match.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * case class Name(first: String, middle: String, last: String)
 *
 * val name = Name("Jane", "Q", "Programmer")
 *
 * inside(name) { case Name(first, _, _) =&gt;
 *   first should startWith ("S")
 * }
 * </pre>
 *
 * <p>
 * You can use <code>inside</code> to just ensure a pattern is matched, without making any further assertions, but a better
 * alternative for that kind of assertion is <code>matchPattern</code>. The <code>matchPattern</code> syntax allows you
 * to express that you expect a value to match a particular pattern, no more and no less:
 * </p>
 *
 * <pre class="stHighlight">
 * name should matchPattern { case Name("Sarah", _, _) =&gt; }
 * </pre>
 *
 * <a name="usingCustomMatchers"></a>
 * <h2>Using custom matchers</h2>
 *
 * <p>
 * If none of the built-in matcher syntax (or options shown so far for extending the syntax) satisfy a particular need you have, you can create
 * custom <code>Matcher</code>s that allow
 * you to place your own syntax directly after <code>should</code>. For example, class <code>java.io.File</code> has a method <code>isHidden</code>, which
 * indicates whether a file of a certain path and name is hidden. Because the <code>isHidden</code> method takes no parameters and returns <code>Boolean</code>,
 * you can call it using <code>be</code> with a symbol or <code>BePropertyMatcher</code>, yielding assertions like:
 * </p>
 *
 * <pre class="stHighlight">
 * file should be ('hidden)  // using a symbol
 * file should be (hidden)   // using a BePropertyMatcher
 * </pre>
 *
 * <p>
 * If it doesn't make sense to have your custom syntax follow <code>be</code>, you might want to create a custom <code>Matcher</code>
 * instead, so your syntax can follow <code>should</code> directly. For example, you might want to be able to check whether
 * a <code>java.io.File</code>'s name ends with a particular extension, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * // using a plain-old Matcher
 * file should endWithExtension ("txt")
 * </pre>
 *
 * <p>
 * ScalaTest provides several mechanism to make it easy to create custom matchers, including ways to compose new matchers
 * out of existing ones complete with new error messages.  For more information about how to create custom
 * <code>Matcher</code>s, please see the documentation for the <a href="matchers/Matcher.html"><code>Matcher</code></a> trait.
 * </p>
 *
 * <a name="checkingForExpectedExceptions"></a>
 * <h2>Checking for expected exceptions</h2>
 *
 * <p>
 * Sometimes you need to test whether a method throws an expected exception under certain circumstances, such
 * as when invalid arguments are passed to the method. With <code>Matchers</code> mixed in, you can
 * check for an expected exception like this:
 * </p>
 *
 * <pre class="stHighlight">
 * an [IndexOutOfBoundsException] should be thrownBy s.charAt(-1)
 * </pre>
 *
 * <p>
 * If <code>charAt</code> throws an instance of <code>StringIndexOutOfBoundsException</code>,
 * this expression will result in that exception. But if <code>charAt</code> completes normally, or throws a different
 * exception, this expression will complete abruptly with a <code>TestFailedException</code>.
 *
 * <p>
 * If you need to further isnpect an expected exception, you can capture it using this syntax:
 * </p>
 *
 * <pre class="stHighlight">
 * val thrown = the [IndexOutOfBoundsException] thrownBy s.charAt(-1)
 * </pre>
 *
 * <p>
 * This expression returns the caught exception so that you can inspect it further if you wish, for
 * example, to ensure that data contained inside the exception has the expected values. Here's an
 * example:
 * </p>
 *
 * <pre class="stHighlight">
 * thrown.getMessage should equal ("String index out of range: -1")
 * </pre>
 *
 * <p>
 * If you prefer you can also capture and inspect an expected exception in one statement, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * the [ArithmeticException] thrownBy 1 / 0 should have message "/ by zero"
 * the [IndexOutOfBoundsException] thrownBy {
 *   s.charAt(-1)
 * } should have message "String index out of range: -1"
 * </pre>
 *
 * <p>
 * You can also state that no exception should be thrown by some code, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * noException should be thrownBy 0 / 1
 * </pre>
 *
 * <a name="thosePeskyParens"></a>
 * <h2>Those pesky parens</h2>
 *
 * <p>
 * Perhaps the most tricky part of writing assertions using ScalaTest matchers is remembering
 * when you need or don't need parentheses, but bearing in mind a few simple rules <!-- PRESERVE -->should help.
 * It is also reassuring to know that if you ever leave off a set of parentheses when they are
 * required, your code will not compile. Thus the compiler will help you remember when you need the parens.
 * That said, the rules are:
 * </p>
 *
 * <p>
 * 1. Although you don't always need them, you may choose to always put parentheses
 * around right-hand values, such as the <code>7</code> in <code>num should equal (7)</code>:
 * </p>
 *
 * <pre>
 * result should equal <span class="stRed">(</span>4<span class="stRed">)</span>
 * array should have length <span class="stRed">(</span>3<span class="stRed">)</span>
 * book should have (
 *   'title <span class="stRed">(</span>"Programming in Scala"<span class="stRed">)</span>,
 *   'author <span class="stRed">(</span>List("Odersky", "Spoon", "Venners")<span class="stRed">)</span>,
 *   'pubYear <span class="stRed">(</span>2008<span class="stRed">)</span>
 * )
 * option should be <span class="stRed">(</span>'defined<span class="stRed">)</span>
 * catMap should (contain key <span class="stRed">(</span>9<span class="stRed">)</span> and contain value <span class="stRed">(</span>"lives"<span class="stRed">)</span>)</span>
 * keyEvent should be an <span class="stRed">(</span>'actionKey<span class="stRed">)</span>
 * javaSet should have size <span class="stRed">(</span>90<span class="stRed">)</span>
 * </pre>
 *
 * <p>
 * 2. Except for <code>length</code>, <code>size</code> and <code>message</code>, you must always put parentheses around
 * the list of one or more property values following a <code>have</code>:
 * </p>
 *
 * <pre>
 * file should (exist and have <span class="stRed">(</span>'name ("temp.txt")<span class="stRed">)</span>)
 * book should have <span class="stRed">(</span>
 *   title ("Programming in Scala"),
 *   author (List("Odersky", "Spoon", "Venners")),
 *   pubYear (2008)
 * <span class="stRed">)</span>
 * javaList should have length (9) // parens optional for length and size
 * </pre>
 *
 * <p>
 * 3. You must always put parentheses around <code>and</code> and <code>or</code> expressions, as in:
 * </p>
 *
 * <pre>
 * catMap should <span class="stRed">(</span>contain key (9) and contain value ("lives")<span class="stRed">)</span>
 * number should <span class="stRed">(</span>equal (2) or equal (4) or equal (8)<span class="stRed">)</span>
 * </pre>
 *
 * <p>
 * 4. Although you don't always need them, you may choose to always put parentheses
 * around custom <code>Matcher</code>s when they appear directly after <code>not</code>:
 * </p>
 *
 * <pre>
 * file should exist
 * file should not <span class="stRed">(</span>exist<span class="stRed">)</span>
 * file should (exist and have ('name ("temp.txt")))
 * file should (not <span class="stRed">(</span>exist<span class="stRed">)</span> and have ('name ("temp.txt"))
 * file should (have ('name ("temp.txt") or exist)
 * file should (have ('name ("temp.txt") or not <span class="stRed">(</span>exist<span class="stRed">)</span>)
 * </pre>
 *
 * <p>
 * That's it. With a bit of practice it <!-- PRESERVE -->should become natural to you, and the compiler will always be there to tell you if you
 * forget a set of needed parentheses.
 * </p>
 *
 * <p>
 * <em>Note: ScalaTest's matchers are in part inspired by the matchers of <a href="http://rspec.info" target="_blank">RSpec</a>,
 * <a href="https://github.com/hamcrest/JavaHamcrest" target="_blank">Hamcrest</a>, and
 * <a href="http://etorreborre.github.io/specs2/" target="_blank">specs2</a>, and its &ldquo;<code>shouldNot compile</code>&rdquo; syntax
 * by the <code>illTyped</code> macro of <a href="https://github.com/milessabin/shapeless" target="_blank">shapeless</a>.</em>
 * </p>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
trait Matchers extends Assertions with Tolerance with ShouldVerb with MatcherWords with Explicitly { matchers =>

  import scala.language.implicitConversions

  // SKIP-SCALATESTJS,NATIVE-START
  // This guy is generally done through an implicit conversion from a symbol. It takes that symbol, and
  // then represents an object with an apply method. So it gives an apply method to symbols.
  // book should have ('author ("Gibson"))
  //                   ^ // Basically this 'author symbol gets converted into this class, and its apply  method takes "Gibson"
  // TODO, put the documentation of the details of the algo for selecting a method or field to use here.
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used as the result of an implicit conversion from class <code>Symbol</code>, to enable symbols to be
   * used in <code>have ('author ("Dickens"))</code> syntax. The name of the implicit conversion method is
   * <code>convertSymbolToHavePropertyMatcherGenerator</code>.
   * </p>
   *
   * <p>
   * Class <code>HavePropertyMatcherGenerator</code>'s primary constructor takes a <code>Symbol</code>. The
   * <code>apply</code> method uses reflection to find and access a property that has the name specified by the
   * <code>Symbol</code> passed to the constructor, so it can determine if the property has the expected value
   * passed to <code>apply</code>.
   * If the symbol passed is <code>'title</code>, for example, the <code>apply</code> method
   * will use reflection to look for a public Java field named
   * "title", a public method named "title", or a public method named "getTitle".
   * If a method, it must take no parameters. If multiple candidates are found,
   * the <code>apply</code> method will select based on the following algorithm:
   * </p>
   *
   * <table class="stTable">
   * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"get" Method</th><th class="stHeadingCell">Result</th></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>getTitle()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>title()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>title()</code></td></tr>
   * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>title()</code></td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>title()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>title</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>getTitle()</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell"><code>title()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>title()</code></td></tr>
   * <tr><td class="stTableCell"><code>title</code></td><td class="stTableCell"><code>title()</code></td><td class="stTableCell"><code>getTitle()</code></td><td class="stTableCell">Invokes <code>title()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
   * </table>
   *
   * @author Bill Venners
   */
  final class HavePropertyMatcherGenerator(symbol: Symbol, prettifer: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should have ('title ("A Tale of Two Cities"))
     *                          ^
     * </pre>
     *
     * <p>
     * This class has an <code>apply</code> method that will produce a <code>HavePropertyMatcher[AnyRef, Any]</code>.
     * The implicit conversion method, <code>convertSymbolToHavePropertyMatcherGenerator</code>, will cause the
     * above line of code to be eventually transformed into:
     * </p>
     *
     * <pre class="stHighlight">
     * book should have (convertSymbolToHavePropertyMatcherGenerator('title).apply("A Tale of Two Cities"))
     * </pre>
     */
    def apply(expectedValue: Any): HavePropertyMatcher[AnyRef, Any] =
      new HavePropertyMatcher[AnyRef, Any] {

        /**
         * This method enables the following syntax:
         *
         * <pre class="stHighlight">
         * book should have ('title ("A Tale of Two Cities"))
         * </pre>
         *
         * <p>
         * This method uses reflection to discover a field or method with a name that indicates it represents
         * the value of the property with the name contained in the <code>Symbol</code> passed to the
         * <code>HavePropertyMatcherGenerator</code>'s constructor. The field or method must be public. To be a
         * candidate, a field must have the name <code>symbol.name</code>, so if <code>symbol</code> is <code>'title</code>,
         * the field name sought will be <code>"title"</code>. To be a candidate, a method must either have the name
         * <code>symbol.name</code>, or have a JavaBean-style <code>get</code> or <code>is</code>. If the type of the
         * passed <code>expectedValue</code> is <code>Boolean</code>, <code>"is"</code> is prepended, else <code>"get"</code>
         * is prepended. Thus if <code>'title</code> is passed as <code>symbol</code>, and the type of the <code>expectedValue</code> is
         * <code>String</code>, a method named <code>getTitle</code> will be considered a candidate (the return type
         * of <code>getTitle</code> will not be checked, so it need not be <code>String</code>. By contrast, if <code>'defined</code>
         * is passed as <code>symbol</code>, and the type of the <code>expectedValue</code> is <code>Boolean</code>, a method
         * named <code>isTitle</code> will be considered a candidate so long as its return type is <code>Boolean</code>.
         * </p>
         * TODO continue the story
         */
        def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Any] = {
        
          // If 'empty passed, propertyName would be "empty"
          val propertyName = symbol.name

          val isBooleanProperty =
            expectedValue match {
              case o: Boolean => true
              case _ => false
            }

          accessProperty(objectWithProperty, symbol, isBooleanProperty) match {

            case None =>

              // if propertyName is '>, mangledPropertyName would be "$greater"
              val mangledPropertyName = transformOperatorChars(propertyName)

              // methodNameToInvoke would also be "title"
              val methodNameToInvoke = mangledPropertyName

              // methodNameToInvokeWithGet would be "getTitle"
              val methodNameToInvokeWithGet = "get"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

              throw newTestFailedException(Resources.propertyNotFound(methodNameToInvoke, expectedValue.toString, methodNameToInvokeWithGet), None, pos)

            case Some(result) =>

              new HavePropertyMatchResult[Any](
                result == expectedValue,
                propertyName,
                expectedValue,
                result
              )
          }
        }

        /**
         * Overrides to return pretty toString.
         */
        override def toString: String = "HavePropertyMatcher[AnyRef, Any](expectedValue = " + Prettifier.default(expectedValue) + ")"
      }
  }

  /**
   * This implicit conversion method converts a <code>Symbol</code> to a
   * <code>HavePropertyMatcherGenerator</code>, to enable the symbol to be used with the <code>have ('author ("Dickens"))</code> syntax.
   */
  implicit def convertSymbolToHavePropertyMatcherGenerator(symbol: Symbol)(implicit prettifier: Prettifier, pos: source.Position): HavePropertyMatcherGenerator = new HavePropertyMatcherGenerator(symbol, prettifier, pos)
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class ResultOfBeWordForAny[T](left: T, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax (positiveNumber is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * 1 should be a positiveNumber
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def a(aMatcher: AMatcher[T]): Assertion = {
    // SKIP-DOTTY-START 
    def a(aMatcher: AMatcher[T]): Assertion = {
    // SKIP-DOTTY-END  
      val matcherResult = aMatcher(left)
      if (matcherResult.matches != shouldBeTrue) {
        indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
      } else indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
    }

    /**
     * This method enables the following syntax (positiveNumber is a <code>AnMatcher</code>):
     *
     * <pre class="stHighlight">
     * 1 should be an oddNumber
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def an(anMatcher: AnMatcher[T]): Assertion = {
    // SKIP-DOTTY-START 
    def an(anMatcher: AnMatcher[T]): Assertion = {
    // SKIP-DOTTY-END  
      val matcherResult = anMatcher(left)
      if (matcherResult.matches != shouldBeTrue) {
        indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
      } else indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *                  ^
     * </pre>
     */
    //DOTTY-ONLY infix def theSameInstanceAs(right: AnyRef)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def theSameInstanceAs(right: AnyRef)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      if ((toAnyRef(left) eq right) != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.wasNotSameInstanceAs(prettifier, left, right) else FailureMessages.wasSameInstanceAs(prettifier, left, right), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.wasSameInstanceAs(prettifier, left, right), FailureMessages.wasNotSameInstanceAs(prettifier, left, right))
    }

    /* *
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be a [String]
     *                  ^
     * </pre>
    //DOTTY-ONLY infix def a[EXPECTED : ClassManifest] {
    // SKIP-DOTTY-START 
    def a[EXPECTED : ClassManifest] {
    // SKIP-DOTTY-END  
      val clazz = implicitly[ClassManifest[EXPECTED]].erasure.asInstanceOf[Class[EXPECTED]]
      if (clazz.isAssignableFrom(left.getClass)) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages.wasNotAnInstanceOf(prettifier, left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
          else
            FailureMessages.wasAnInstanceOf
        )
      }
    }
    */

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fileMock should be a ('file)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def a(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def a(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), symbol, true, true, prettifier, pos)
      if (matcherResult.matches != shouldBeTrue) {
        indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
      } else indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    // TODO: Check the shouldBeTrues, are they sometimes always false or true?
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should be a (goodRead)
     *                   ^
     * </pre>
     */
    //DOTTY-ONLY infix def a(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try expanding this to 2.10 AnyVals
    // SKIP-DOTTY-START 
    def a(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try expanding this to 2.10 AnyVals
    // SKIP-DOTTY-END
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        indicateFailure(if (shouldBeTrue) FailureMessages.wasNotA(prettifier, left, UnquotedString(result.propertyName)) else FailureMessages.wasA(prettifier, left, UnquotedString(result.propertyName)), None, pos)
      } else indicateSuccess(shouldBeTrue, FailureMessages.wasA(prettifier, left, UnquotedString(result.propertyName)), FailureMessages.wasNotA(prettifier, left, UnquotedString(result.propertyName)))
    }

    // SKIP-SCALATESTJS,NATIVE-START
    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fruit should be an ('orange)
     *                 ^
     * </pre>
     */
    //DOTTY-ONLY infix def an(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def an(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), symbol, true, false, prettifier, pos)
      if (matcherResult.matches != shouldBeTrue) {
        indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
      } else indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should be an (excellentRead)
     *                ^
     * </pre>
     */
    //DOTTY-ONLY infix def an(beTrueMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try expanding this to 2.10 AnyVals
    // SKIP-DOTTY-START 
    def an(beTrueMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try expanding this to 2.10 AnyVals
    // SKIP-DOTTY-END
      val beTrueMatchResult = beTrueMatcher(left)
      if (beTrueMatchResult.matches != shouldBeTrue) {
        indicateFailure(if (shouldBeTrue) FailureMessages.wasNotAn(prettifier, left, UnquotedString(beTrueMatchResult.propertyName)) else FailureMessages.wasAn(prettifier, left, UnquotedString(beTrueMatchResult.propertyName)), None, pos)
      } else indicateSuccess(shouldBeTrue, FailureMessages.wasAn(prettifier, left, UnquotedString(beTrueMatchResult.propertyName)), FailureMessages.wasNotAn(prettifier, left, UnquotedString(beTrueMatchResult.propertyName)))
    }

    /**
     * This method enables the following syntax, where <code>fraction</code> is, for example, of type <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * fraction should be definedAt (6)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def definedAt[U](right: U)(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    // SKIP-DOTTY-START 
    def definedAt[U](right: U)(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    // SKIP-DOTTY-END  
      if (left.isDefinedAt(right) != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.wasNotDefinedAt(prettifier, left, right) else FailureMessages.wasDefinedAt(prettifier, left, right), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.wasDefinedAt(prettifier, left, right), FailureMessages.wasNotDefinedAt(prettifier, left, right))
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfBeWordForAny([left], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfBeWordForAny(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class RegexWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
     *                                     ^
     * </pre>
     */
    def apply(regexString: String): ResultOfRegexWordApplication = 
      new ResultOfRegexWordApplication(regexString, IndexedSeq.empty)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                                     ^
     * </pre>
     */
    def apply(regex: Regex): ResultOfRegexWordApplication = 
      new ResultOfRegexWordApplication(regex, IndexedSeq.empty)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should not fullyMatch regex ("a(b*)c" withGroup "bb")
     *                                    ^
     * </pre>
     */
    def apply(regexWithGroups: RegexWithGroups) =
      new ResultOfRegexWordApplication(regexWithGroups.regex, regexWithGroups.groups)

    /**
     * Overrides to return "regex"
     */
    override def toString: String = "regex"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfIncludeWordForString(left: String, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should include regex ("world")
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion =
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      regex(rightRegexString.r)
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should include regex ("a(b*)c" withGroup "bb")
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-END  
      val result = includeRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
      else indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should include regex ("wo.ld".r)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-END  
      if (rightRegex.findFirstIn(left).isDefined != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.didNotIncludeRegex(prettifier, left, rightRegex) else FailureMessages.includedRegex(prettifier, left, rightRegex), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.includedRegex(prettifier, left, rightRegex), FailureMessages.didNotIncludeRegex(prettifier, left, rightRegex))
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfIncludeWordForString([left], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfIncludeWordForString(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfStartWithWordForString(left: String, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o")
     *                         ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion =
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      regex(rightRegexString.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should startWith regex ("a(b*)c" withGroup "bb")
     *                         ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-END  
      val result = startWithRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
      else indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o".r)
     *                         ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-END  
      if (rightRegex.pattern.matcher(left).lookingAt != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.didNotStartWithRegex(prettifier, left, rightRegex) else FailureMessages.startedWithRegex(prettifier, left, rightRegex), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.startedWithRegex(prettifier, left, rightRegex), FailureMessages.didNotStartWithRegex(prettifier, left, rightRegex))
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfStartWithWordForString([left], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfStartWithWordForString(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfEndWithWordForString(left: String, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d")
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion =
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      regex(rightRegexString.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should endWith regex ("a(b*)c" withGroup "bb")
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-END  
      val result = endWithRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
      else indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d".r)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-END  
      val allMatches = rightRegex.findAllIn(left)
      if ((allMatches.hasNext && (allMatches.end == left.length)) != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.didNotEndWithRegex(prettifier, left, rightRegex) else FailureMessages.endedWithRegex(prettifier, left, rightRegex), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.endedWithRegex(prettifier, left, rightRegex), FailureMessages.didNotEndWithRegex(prettifier, left, rightRegex))
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfEndWithWordForString([left], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfEndWithWordForString(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfFullyMatchWordForString(left: String, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should fullMatch regex ("Hel*o world")
     *                         ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion =
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      regex(rightRegexString.r)
    

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should fullMatch regex ("a(b*)c" withGroup "bb")
     *                         ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = {
    // SKIP-DOTTY-END  
      val result = fullyMatchRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
      else indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should fullymatch regex ("Hel*o world".r)
     *                          ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = {
    // SKIP-DOTTY-END  
      if (rightRegex.pattern.matcher(left).matches != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.didNotFullyMatchRegex(prettifier, left, rightRegex) else FailureMessages.fullyMatchedRegex(prettifier, left, rightRegex), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.fullyMatchedRegex(prettifier, left, rightRegex), FailureMessages.didNotFullyMatchRegex(prettifier, left, rightRegex))
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfFullyMatchWordForString([left], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfFullyMatchWordForString(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  // Going back to original, legacy one to get to a good place to check in.
/*
  def equal(right: Any): Matcher[Any] =
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
          MatchResult(
            areEqualComparingArraysStructurally(left, right),
            FailureMessages.didNotEqual(prettifier, leftee, rightee),
            FailureMessages.equaled(prettifier, left, right)
          )
        }
      }
*/

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal (100 +- 1)
   *               ^
   * </pre>
   */
  def equal[T](spread: Spread[T]): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        MatchResult(
          spread.isWithin(left),
          Resources.rawDidNotEqualPlusOrMinus,
          Resources.rawEqualedPlusOrMinus,
          Vector(left, spread.pivot, spread.tolerance)
        )
      }
      override def toString: String = "equal (" + Prettifier.default(spread) + ")"
    }
  }

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal (null)
   *               ^
   * </pre>
   */ 
  def equal(o: Null): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left == null,
          Resources.rawDidNotEqualNull,
          Resources.rawEqualedNull,
          Resources.rawDidNotEqualNull,
          Resources.rawMidSentenceEqualedNull,
          Vector(left),
          Vector.empty
        )
      }
      override def toString: String = "equal (" + Prettifier.default(o) + ")"
    }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class KeyWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not contain key (10)
     *                            ^
     * </pre>
     */
    def apply(expectedKey: Any): ResultOfKeyWordApplication = 
      new ResultOfKeyWordApplication(expectedKey)

    /**
     * Overrides to return pretty toString.
     *
     * @return "key"
     */
    override def toString: String = "key"
  }

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should not contain key (10)
   *                        ^
   * </pre>
   */
  val key = new KeyWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ValueWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not contain key (10)
     *                            ^
     * </pre>
     */ 
    def apply(expectedValue: Any): ResultOfValueWordApplication =
      new ResultOfValueWordApplication(expectedValue)

    /**
     * Overrides to return pretty toString.
     *
     * @return "value"
     */
    override def toString: String = "value"
  }

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should not contain value (10)
   *                        ^
   * </pre>
   */
  val value = new ValueWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * badBook should not be a ('goodRead)
     *                         ^
     * </pre>
     */
    def apply(symbol: Symbol): ResultOfAWordToSymbolApplication =
      new ResultOfAWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>goodRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be a (goodRead)
     *                         ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAWordToBePropertyMatcherApplication[T] = 
      new ResultOfAWordToBePropertyMatcherApplication(beTrueMatcher)

    /**
     * This method enables the following syntax, where, <code>positiveNumber</code> is an <code>AMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * result should not be a (positiveNumber)
     *                        ^
     * </pre>
     */
    def apply[T](aMatcher: AMatcher[T]): ResultOfAWordToAMatcherApplication[T] = 
      new ResultOfAWordToAMatcherApplication(aMatcher)

    /**
     * Overrides to return pretty toString.
     *
     * @return "a"
     */
    override def toString: String = "a"
  }

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   * badBook should not be a ('goodRead)
   *                       ^
   * </pre>
   */
  val a = new AWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AnWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * badBook should not be an ('excellentRead)
     *                          ^
     * </pre>
     */
    def apply(symbol: Symbol): ResultOfAnWordToSymbolApplication = 
      new ResultOfAnWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>excellentRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be an (excellentRead)
     *                          ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAnWordToBePropertyMatcherApplication[T] = 
      new ResultOfAnWordToBePropertyMatcherApplication(beTrueMatcher)

    /**
     * This method enables the following syntax, where, <code>positiveNumber</code> is an <code>AnMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * result should not be an (positiveNumber)
     *                         ^
     * </pre>
     */
    def apply[T](anMatcher: AnMatcher[T]): ResultOfAnWordToAnMatcherApplication[T] = 
      new ResultOfAnWordToAnMatcherApplication(anMatcher)

    /**
     * Overrides to return pretty toString.
     *
     * @return "an"
     */
    override def toString: String = "an"
  }

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   * badBook should not be an (excellentRead)
   *                       ^
   * </pre>
   */
  val an = new AnWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class TheSameInstanceAsPhrase {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * oneString should not be theSameInstanceAs (anotherString)
     *                                           ^
     * </pre>
     */
    def apply(anyRef: AnyRef): ResultOfTheSameInstanceAsApplication =
      new ResultOfTheSameInstanceAsApplication(anyRef)

    /**
     * Overrides to return pretty toString.
     *
     * @return "theSameInstanceAs"
     */
    override def toString: String = "theSameInstanceAs"
  }

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   * oneString should not be theSameInstanceAs (anotherString)
   *                         ^
   * </pre>
   */
  val theSameInstanceAs: TheSameInstanceAsPhrase = new TheSameInstanceAsPhrase

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""".r)
   *                               ^
   * </pre>
   */
  val regex = new RegexWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForExtent[A](left: A, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2L)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    //DOTTY-ONLY infix def length(expectedLength: Long)(implicit len: Length[A]): Assertion = {
    // SKIP-DOTTY-START 
    def length(expectedLength: Long)(implicit len: Length[A]): Assertion = {
    // SKIP-DOTTY-END  
      val leftLength = len.lengthOf(left)
      if ((leftLength == expectedLength) != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, left, leftLength, expectedLength) else FailureMessages.hadLength(prettifier, left, expectedLength), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.hadLength(prettifier, left, expectedLength), FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, left, leftLength, expectedLength))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2L)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    //DOTTY-ONLY infix def size(expectedSize: Long)(implicit sz: Size[A]): Assertion = {
    // SKIP-DOTTY-START 
    def size(expectedSize: Long)(implicit sz: Size[A]): Assertion = {
    // SKIP-DOTTY-END  
      val leftSize = sz.sizeOf(left)
      if ((leftSize == expectedSize) != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, left, leftSize, expectedSize) else FailureMessages.hadSize(prettifier, left, expectedSize), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.hadSize(prettifier, left, expectedSize), FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, left, leftSize, expectedSize))
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * exception should have message ("file not found")
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def message(expectedMessage: String)(implicit messaging: Messaging[A]): Assertion = {
    // SKIP-DOTTY-START 
    def message(expectedMessage: String)(implicit messaging: Messaging[A]): Assertion = {
    // SKIP-DOTTY-END  
      val actualMessage = messaging.messageOf(left)
      if ((actualMessage== expectedMessage) != shouldBeTrue)
        indicateFailure(if (shouldBeTrue) FailureMessages.hadMessageInsteadOfExpectedMessage(prettifier, left, actualMessage, expectedMessage) else FailureMessages.hadExpectedMessage(prettifier, left, expectedMessage), None, pos)
      else indicateSuccess(shouldBeTrue, FailureMessages.hadExpectedMessage(prettifier, left, expectedMessage), FailureMessages.hadMessageInsteadOfExpectedMessage(prettifier, left, actualMessage, expectedMessage))
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfHaveWordForExtent([left], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfHaveWordForExtent(" + Prettifier.default(left) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should (not be &lt; (10) and not be &gt; (17))
   *                    ^
   * </pre>
   */ 
  def <[T : Ordering] (right: T): ResultOfLessThanComparison[T] =
    new ResultOfLessThanComparison(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should (not be &gt; (10) and not be &lt; (7))
   *                    ^
   * </pre>
   */ 
  def >[T : Ordering] (right: T): ResultOfGreaterThanComparison[T] =
    new ResultOfGreaterThanComparison(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should (not be &lt;= (10) and not be &gt; (17))
   *                    ^
   * </pre>
   */ 
  def <=[T : Ordering] (right: T): ResultOfLessThanOrEqualToComparison[T] =
    new ResultOfLessThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should (not be &gt;= (10) and not be < (7))
   *                    ^
   * </pre>
   */ 
  def >=[T : Ordering] (right: T): ResultOfGreaterThanOrEqualToComparison[T] =
    new ResultOfGreaterThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * list should (not be definedAt (7) and not be definedAt (9))
   *                     ^
   * </pre>
   */
  def definedAt[T](right: T): ResultOfDefinedAt[T] =
    new ResultOfDefinedAt(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (oneOf(1, 2))
   *                               ^
   * </pre>
   */
  def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit pos: source.Position) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.oneOfDuplicate, pos)
    new ResultOfOneOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (oneElementOf (List(1, 2)))
   *                               ^
   * </pre>
   */
  def oneElementOf(elements: Iterable[Any]) = {
    val xs = elements.toList
    new ResultOfOneElementOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (atLeastOneOf(1, 2))
   *                               ^
   * </pre>
   */
  def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit pos: source.Position) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.atLeastOneOfDuplicate, pos)
    new ResultOfAtLeastOneOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (atLeastOneElementOf (List(1, 2)))
   *                               ^
   * </pre>
   */
  def atLeastOneElementOf(elements: Iterable[Any]) = {
    val xs = elements.toList
    new ResultOfAtLeastOneElementOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (noneOf(1, 2))
   *                               ^
   * </pre>
   */
  def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit pos: source.Position) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.noneOfDuplicate, pos)
    new ResultOfNoneOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (noElementsOf List(1, 2))
   *                               ^
   * </pre>
   */
  def noElementsOf(elements: Iterable[Any]) = {
    val xs = elements.toList
    new ResultOfNoElementsOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (theSameElementsAs(List(1, 2, 3)))
   *                               ^
   * </pre>
   */
  def theSameElementsAs(xs: Iterable[_]) = 
    new ResultOfTheSameElementsAsApplication(xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (theSameElementsInOrderAs(List(1, 2)))
   *                               ^
   * </pre>
   */
  def theSameElementsInOrderAs(xs: Iterable[_]) = 
    new ResultOfTheSameElementsInOrderAsApplication(xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (only(1, 2))
   *                               ^
   * </pre>
   */
  def only(xs: Any*)(implicit pos: source.Position) = {
    if (xs.isEmpty)
      throw new NotAllowedException(FailureMessages.onlyEmpty, pos)
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.onlyDuplicate, pos)
    new ResultOfOnlyApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrderOnly(1, 2))
   *                               ^
   * </pre>
   */
  def inOrderOnly[T](firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit pos: source.Position) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.inOrderOnlyDuplicate, pos)
    new ResultOfInOrderOnlyApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (allOf(1, 2))
   *                               ^
   * </pre>
   */
  def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit pos: source.Position) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.allOfDuplicate, pos)
    new ResultOfAllOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (allElementsOf(1, 2))
   *                               ^
   * </pre>
   */
  def allElementsOf[R](elements: Iterable[R]) = {
    val xs = elements.toList
    new ResultOfAllElementsOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrder(1, 2))
   *                               ^
   * </pre>
   */
  def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit pos: source.Position) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.inOrderDuplicate, pos)
    new ResultOfInOrderApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrderElementsOf List(1, 2))
   *                               ^
   * </pre>
   */
  def inOrderElementsOf[R](elements: Iterable[R]) = {
    val xs = elements.toList
    new ResultOfInOrderElementsOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (atMostOneOf(1, 2))
   *                               ^
   * </pre>
   */
  def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit pos: source.Position) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages.atMostOneOfDuplicate, pos)
    new ResultOfAtMostOneOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (atMostOneElementOf (List(1, 2)))
   *                               ^
   * </pre>
   */
  def atMostOneElementOf[R](elements: Iterable[R]) = {
    val xs = elements.toList
    new ResultOfAtMostOneElementOfApplication(xs)
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy {...}
   *                                ^
   * </pre>
   */
  def thrownBy(fun: => Any) = 
    new ResultOfThrownByApplication(fun)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * exception should not have message ("file not found")
   *                           ^
   * </pre>
   */
  def message(expectedMessage: String) = 
    new ResultOfMessageWordApplication(expectedMessage)

/*
  // For safe keeping
  private implicit def nodeToCanonical(node: scala.xml.Node) = new Canonicalizer(node)

  private class Canonicalizer(node: scala.xml.Node) {

    def toCanonical: scala.xml.Node = {
      node match {
        case elem: scala.xml.Elem =>
          val canonicalizedChildren =
            for (child <- node.child if !child.toString.trim.isEmpty) yield {
              child match {
                case elem: scala.xml.Elem => elem.toCanonical
                case other => other
              }
            }
          new scala.xml.Elem(elem.prefix, elem.label, elem.attributes, elem.scope, canonicalizedChildren: _*)
        case other => other
      }
    }
  }
*/

/*
  class AType[T : ClassManifest] {

    private val clazz = implicitly[ClassManifest[T]].erasure.asInstanceOf[Class[T]]

    def isAssignableFromClassOf(o: Any): Boolean = clazz.isAssignableFrom(o.getClass)

    def className: String = clazz.getName
  }

  def a[T : ClassManifest]: AType[T] = new AType[T]
*/

  // This is where InspectorShorthands started

  protected sealed abstract class Collected(name: String) extends Serializable {
    override def toString: String = name
  }
  private object AllCollected extends Collected("AllCollected")
  private object EveryCollected extends Collected("EveryCollected")
  private case class BetweenCollected(from: Int, to: Int) extends Collected("BetweenCollected")
  private case class AtLeastCollected(num: Int) extends Collected("AtLeastCollected")
  private case class AtMostCollected(num: Int) extends Collected("AtMostCollected")
  private object NoCollected extends Collected("NoCollected")
  private case class ExactlyCollected(num: Int) extends Collected("ExactlyCollected")

  private[scalatest] def doCollected[T](collected: Collected, xs: Iterable[T], original: Any, prettifier: Prettifier, pos: source.Position)(fun: T => Assertion): Assertion = {

    val asserting = InspectorAsserting.assertingNatureOfAssertion

    collected match {
      case AllCollected =>
        asserting.forAll(xs, original, true, prettifier, pos) { e =>
          fun(e)
        }
      case AtLeastCollected(num) =>
        asserting.forAtLeast(num, xs, original, true, prettifier, pos) { e =>
          fun(e)
        }
      case EveryCollected =>
        asserting.forEvery(xs, original, true, prettifier, pos) { e =>
          fun(e)
        }
      case ExactlyCollected(num) =>
        asserting.forExactly(num, xs, original, true, prettifier, pos) { e =>
          fun(e)
        }
      case NoCollected =>
        asserting.forNo(xs, original, true, prettifier, pos) { e =>
          fun(e)
        }
      case BetweenCollected(from, to) =>
        asserting.forBetween(from, to, xs, original, true, prettifier, pos) { e =>
          fun(e)
        }
      case AtMostCollected(num) =>
        asserting.forAtMost(num, xs, original, true, prettifier, pos) { e =>
          fun(e)
        }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfNotWordForCollectedAny[T](collected: Collected, xs: Iterable[T], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {


    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (7)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def equal(right: Any)(implicit equality: Equality[T]): Assertion = {
    // SKIP-DOTTY-START 
    def equal(right: Any)(implicit equality: Equality[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((equality.areEqual(e, right)) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.didNotEqual(prettifier, e, right) else FailureMessages.equaled(prettifier, e, right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.equaled(prettifier, e, right), FailureMessages.didNotEqual(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (7)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(right: Any): Assertion = {
    // SKIP-DOTTY-START 
    def be(right: Any): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((e == right) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotEqualTo(prettifier, e, right) else FailureMessages.wasEqualTo(prettifier, e, right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.wasEqualTo(prettifier, e, right), FailureMessages.wasNotEqualTo(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be &lt;= (7)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (comparison(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotLessThanOrEqualTo(prettifier, e, comparison.right) else FailureMessages.wasLessThanOrEqualTo(prettifier, e, comparison.right), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasLessThanOrEqualTo(prettifier, e, comparison.right), FailureMessages.wasNotLessThanOrEqualTo(prettifier, e, comparison.right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be &gt;= (7)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (comparison(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotGreaterThanOrEqualTo(prettifier, e, comparison.right) else FailureMessages.wasGreaterThanOrEqualTo(prettifier, e, comparison.right), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasGreaterThanOrEqualTo(prettifier, e, comparison.right), FailureMessages.wasNotGreaterThanOrEqualTo(prettifier, e, comparison.right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be &lt; (7)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(comparison: ResultOfLessThanComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(comparison: ResultOfLessThanComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (comparison(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotLessThan(prettifier, e, comparison.right) else FailureMessages.wasLessThan(prettifier, e, comparison.right), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasLessThan(prettifier, e, comparison.right), FailureMessages.wasNotLessThan(prettifier, e, comparison.right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be &gt; (7)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (comparison(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotGreaterThan(prettifier, e, comparison.right) else FailureMessages.wasGreaterThan(prettifier, e, comparison.right), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasGreaterThan(prettifier, e, comparison.right), FailureMessages.wasNotGreaterThan(prettifier, e, comparison.right))
      }
    }

    /**
     * <strong>
     * The deprecation period for the "be ===" syntax has expired, and the syntax
     * will now throw <code>NotAllowedException</code>.  Please use should equal, should ===, shouldEqual,
     * should be, or shouldBe instead.
     * </strong>
     *
     * <p>
     * Note: usually syntax will be removed after its deprecation period. This was left in because otherwise the syntax could in some
     * cases still compile, but silently wouldn't work.
     * </p>
     */
    @deprecated("The deprecation period for the be === syntax has expired. Please use should equal, should ===, shouldEqual, should be, or shouldBe instead.", "1.9.2")
    //DOTTY-ONLY infix def be(comparison: TripleEqualsInvocation[_]): Nothing = {
    // SKIP-DOTTY-START
    def be(comparison: TripleEqualsInvocation[_]): Nothing = {
    // SKIP-DOTTY-END  
      throw new NotAllowedException(FailureMessages.beTripleEqualsNotAllowed, pos)
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to
     * a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (odd)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(beMatcher: BeMatcher[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(beMatcher: BeMatcher[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = beMatcher(e)
        if (result.matches != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) result.failureMessage(prettifier) else result.negatedFailureMessage(prettifier), None, pos)
        }
        else indicateSuccess(shouldBeTrue, result.negatedFailureMessage(prettifier), result.failureMessage(prettifier))
      }
    }

    /**
     * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
     * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (empty)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(bePropertyMatcher: BePropertyMatcher[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(bePropertyMatcher: BePropertyMatcher[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNot(prettifier, e, UnquotedString(result.propertyName)) else FailureMessages.was(prettifier, e, UnquotedString(result.propertyName)), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.was(prettifier, e, UnquotedString(result.propertyName)), FailureMessages.wasNot(prettifier, e, UnquotedString(result.propertyName)))
      }
    }

    /**
     * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should not be a (file)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Assertion = {
    // SKIP-DOTTY-START 
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = resultOfAWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotA(prettifier, e, UnquotedString(result.propertyName)) else FailureMessages.wasA(prettifier, e, UnquotedString(result.propertyName)), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasA(prettifier, e, UnquotedString(result.propertyName)), FailureMessages.wasNotA(prettifier, e, UnquotedString(result.propertyName)))
      }
    }

    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * all(keyEvents) should not be an (actionKey)
     *                           ^
     * </pre>
     */
    //DOTTY-ONLY infix def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): Assertion = {
    // SKIP-DOTTY-START 
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = resultOfAnWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotAn(prettifier, e, UnquotedString(result.propertyName)) else FailureMessages.wasAn(prettifier, e, UnquotedString(result.propertyName)), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasAn(prettifier, e, UnquotedString(result.propertyName)), FailureMessages.wasNotAn(prettifier, e, UnquotedString(result.propertyName)))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be theSameInstanceAs (string)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Assertion = {
    // SKIP-DOTTY-START 
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        e match {
          case ref: AnyRef =>
            if ((resultOfSameInstanceAsApplication.right eq ref) != shouldBeTrue) {
              indicateFailure(if (shouldBeTrue) FailureMessages.wasNotSameInstanceAs(prettifier, e, resultOfSameInstanceAsApplication.right) else FailureMessages.wasSameInstanceAs(prettifier, e, resultOfSameInstanceAsApplication.right), None, pos)
            }
            else indicateSuccess(shouldBeTrue, FailureMessages.wasSameInstanceAs(prettifier, e, resultOfSameInstanceAsApplication.right), FailureMessages.wasNotSameInstanceAs(prettifier, e, resultOfSameInstanceAsApplication.right))
          case _ =>
            throw new IllegalArgumentException("theSameInstanceAs should only be used for AnyRef")
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be definedAt ("apple")
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be[U](resultOfDefinedAt: ResultOfDefinedAt[U])(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    // SKIP-DOTTY-START 
    def be[U](resultOfDefinedAt: ResultOfDefinedAt[U])(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (e.isDefinedAt(resultOfDefinedAt.right) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotDefinedAt(prettifier, e, resultOfDefinedAt.right) else FailureMessages.wasDefinedAt(prettifier, e, resultOfDefinedAt.right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.wasDefinedAt(prettifier, e, resultOfDefinedAt.right), FailureMessages.wasNotDefinedAt(prettifier, e, resultOfDefinedAt.right))
      }
    }

    // TODO: Write tests and implement cases for:
    // have(length (9), title ("hi")) (this one we'll use this have method but add a HavePropertyMatcher* arg)
    // have(size (9), title ("hi")) (this one we'll use the next have method but add a HavePropertyMatcher* arg)
    // have(length(9), size (9), title ("hi")) (for this one we'll need a new overloaded have(ROLWA, ROSWA, HPM*))
    // have(size(9), length (9), title ("hi")) (for this one we'll need a new overloaded have(ROSWA, ROLWA, HPM*))
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not have length (0)
     *                    ^
     * </pre>
     *
     */
    //DOTTY-ONLY infix def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit len: Length[T]): Assertion = {
    // SKIP-DOTTY-START 
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit len: Length[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        val leftLength = len.lengthOf(e)
        if ((leftLength == right) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, e, leftLength, right) else FailureMessages.hadLength(prettifier, e, right), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.hadLength(prettifier, e, right), FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, e, leftLength, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not have size (0)
     *                    ^
     * </pre>
     *
     */
    //DOTTY-ONLY infix def have(resultOfSizeWordApplication: ResultOfSizeWordApplication)(implicit sz: Size[T]): Assertion = {
    // SKIP-DOTTY-START 
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication)(implicit sz: Size[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        val leftSize = sz.sizeOf(e)
        if ((leftSize == right) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, e, leftSize, right) else FailureMessages.hadSize(prettifier, e, right), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.hadSize(prettifier, e, right), FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, e, leftSize, right))
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should not have (title ("One Hundred Years of Solitude"))
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Assertion = {
    // SKIP-DOTTY-START 
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>

        val results =
          for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
            propertyVerifier(e)

        val firstFailureOption = results.find(pv => !pv.matches)

        val justOneProperty = propertyMatchers.isEmpty

        // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
        // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
        if (firstFailureOption.isDefined == shouldBeTrue) {
          firstFailureOption match {
            case Some(firstFailure) =>
              // This is one of these cases, thus will only get here if shouldBeTrue is true
              // 0 0 | 0 | 1
              // 0 1 | 0 | 1
              // 1 0 | 0 | 1
              indicateFailure(
                FailureMessages.propertyDidNotHaveExpectedValue(prettifier,
                  UnquotedString(firstFailure.propertyName),
                  firstFailure.expectedValue,
                  firstFailure.actualValue,
                  e
                ),
                None,
                pos
              )
            case None =>
              // This is this cases, thus will only get here if shouldBeTrue is false
              // 1 1 | 1 | 0
              val failureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages.propertyHadExpectedValue(prettifier,
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    e
                  )
                }
                else FailureMessages.allPropertiesHadExpectedValues(prettifier, e)

              indicateFailure(failureMessage, None, pos)
          }
        }
        else {
          if (shouldBeTrue)
            indicateSuccess(FailureMessages.allPropertiesHadExpectedValues(prettifier, e))
          else {
            firstFailureOption match {
              case Some(firstFailure) =>
                indicateSuccess(
                  FailureMessages.propertyDidNotHaveExpectedValue(prettifier,
                    UnquotedString(firstFailure.propertyName),
                    firstFailure.expectedValue,
                    firstFailure.actualValue,
                    e
                  )
                )
              case None =>
                val message =
                  if (justOneProperty) {
                    val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                    FailureMessages.propertyHadExpectedValue(prettifier,
                      UnquotedString(firstPropertyResult.propertyName),
                      firstPropertyResult.expectedValue,
                      e
                    )
                  }
                  else FailureMessages.allPropertiesHadExpectedValues(prettifier, e)

                indicateSuccess(message)
            }
          }
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (null)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(o: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def be(o: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((e == null) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotNull(prettifier, e) else FailureMessages.wasNull, None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasNull, FailureMessages.wasNotNull(prettifier, e))
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be ('empty)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def be(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, false, false, prettifier, pos)
        if (matcherResult.matches != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
        }
        else indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be a ('file)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAWordApplication.symbol, true, true, prettifier, pos)
        if (matcherResult.matches != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
        }
        else indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be an ('actionKey)
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAnWordApplication.symbol, true, false, prettifier, pos)
        if (matcherResult.matches != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) matcherResult.failureMessage(prettifier) else matcherResult.negatedFailureMessage(prettifier), None, pos)
        }
        else indicateSuccess(shouldBeTrue, matcherResult.negatedFailureMessage(prettifier), matcherResult.failureMessage(prettifier))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be sorted
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(sortedWord: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(sortedWord: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sortable.isSorted(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotSorted(prettifier, e) else FailureMessages.wasSorted(prettifier, e), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasSorted(prettifier, e), FailureMessages.wasNotSorted(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be readable
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(readableWord: ReadableWord)(implicit readability: Readability[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(readableWord: ReadableWord)(implicit readability: Readability[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (readability.isReadable(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotReadable(prettifier, e) else FailureMessages.wasReadable(prettifier, e), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasReadable(prettifier, e), FailureMessages.wasNotReadable(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be writable
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(writableWord: WritableWord)(implicit writability: Writability[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(writableWord: WritableWord)(implicit writability: Writability[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (writability.isWritable(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotWritable(prettifier, e) else FailureMessages.wasWritable(prettifier, e), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasWritable(prettifier, e), FailureMessages.wasNotWritable(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be empty
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (emptiness.isEmpty(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotEmpty(prettifier, e) else FailureMessages.wasEmpty(prettifier, e), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasEmpty(prettifier, e), FailureMessages.wasNotEmpty(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be defined
     *                    ^
     * </pre>
     */
    //DOTTY-ONLY infix def be(definedWord: DefinedWord)(implicit definition: Definition[T]): Assertion = {
    // SKIP-DOTTY-START 
    def be(definedWord: DefinedWord)(implicit definition: Definition[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (definition.isDefined(e) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.wasNotDefined(prettifier, e) else FailureMessages.wasDefined(prettifier, e), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.wasDefined(prettifier, e), FailureMessages.wasNotDefined(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain (null)
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(nullValue: Null)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(nullValue: Null)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((containing.contains(e, null)) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainNull(prettifier, e) else FailureMessages.containedNull(prettifier, e), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.containedNull(prettifier, e), FailureMessages.didNotContainNull(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(expectedElement: Any)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(expectedElement: Any)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val right = expectedElement
        if ((containing.contains(e, right)) != shouldBeTrue) {
          indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainExpectedElement(prettifier, e, right) else FailureMessages.containedExpectedElement(prettifier, e, right), None, pos)
        }
        else indicateSuccess(shouldBeTrue, FailureMessages.containedExpectedElement(prettifier, e, right), FailureMessages.didNotContainExpectedElement(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain oneOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(oneOf: ResultOfOneOfApplication)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(oneOf: ResultOfOneOfApplication)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END
      val right = oneOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (containing.containsOneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos)
        else indicateSuccess(
          shouldBeTrue,
          FailureMessages.containedOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
          FailureMessages.didNotContainOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain oneElementOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(oneElementOf: ResultOfOneElementOfApplication)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(oneElementOf: ResultOfOneElementOfApplication)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END

      val right = oneElementOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (containing.containsOneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainOneElementOf(prettifier, e, right) else FailureMessages.containedOneElementOf(prettifier, e, right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.containedOneElementOf(prettifier, e, right), FailureMessages.didNotContainOneElementOf(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain atLeastOneOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(atLeastOneOf: ResultOfAtLeastOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(atLeastOneOf: ResultOfAtLeastOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = atLeastOneOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAtLeastOneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos)
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain atLeastOneElementOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(atLeastOneElementOf: ResultOfAtLeastOneElementOfApplication)(implicit evidence: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(atLeastOneElementOf: ResultOfAtLeastOneElementOfApplication)(implicit evidence: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = atLeastOneElementOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (evidence.containsAtLeastOneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right) else FailureMessages.containedAtLeastOneElementOf(prettifier, e, right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.containedAtLeastOneElementOf(prettifier, e, right), FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain noneOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(noneOf: ResultOfNoneOfApplication)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(noneOf: ResultOfNoneOfApplication)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = noneOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (containing.containsNoneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else indicateSuccess(
          shouldBeTrue,
          FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
          FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain noElementsOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(noElementsOf: ResultOfNoElementsOfApplication)(implicit evidence: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(noElementsOf: ResultOfNoElementsOfApplication)(implicit evidence: Containing[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = noElementsOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (evidence.containsNoneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.containedAtLeastOneElementOf(prettifier, e, right) else FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right), FailureMessages.containedAtLeastOneElementOf(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain theSameElementsAs ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(theSameElementsAs: ResultOfTheSameElementsAsApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(theSameElementsAs: ResultOfTheSameElementsAsApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = theSameElementsAs.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsTheSameElementsAs(e, right) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainSameElements(prettifier, e, right) else FailureMessages.containedSameElements(prettifier, e, right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.containedSameElements(prettifier, e, right), FailureMessages.didNotContainSameElements(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain theSameElementsInOrderAs ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(theSameElementsInOrderAs: ResultOfTheSameElementsInOrderAsApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(theSameElementsInOrderAs: ResultOfTheSameElementsInOrderAsApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END

      val right = theSameElementsInOrderAs.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sequencing.containsTheSameElementsInOrderAs(e, right) != shouldBeTrue)
          indicateFailure(if (shouldBeTrue) FailureMessages.didNotContainSameElementsInOrder(prettifier, e, right) else FailureMessages.containedSameElementsInOrder(prettifier, e, right), None, pos)
        else indicateSuccess(shouldBeTrue, FailureMessages.containedSameElementsInOrder(prettifier, e, right), FailureMessages.didNotContainSameElementsInOrder(prettifier, e, right))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain only ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(only: ResultOfOnlyApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(only: ResultOfOnlyApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = only.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsOnly(e, right) != shouldBeTrue) {
          val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[Iterable[_]] || right(0).isInstanceOf[Every[_]])
          indicateFailure(
            if (shouldBeTrue)
              if (withFriendlyReminder)
                FailureMessages.didNotContainOnlyElementsWithFriendlyReminder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
              else
                FailureMessages.didNotContainOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))

            else
              if (withFriendlyReminder)
                FailureMessages.containedOnlyElementsWithFriendlyReminder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
              else
                FailureMessages.containedOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        }
        else indicateSuccess(
          shouldBeTrue,
          FailureMessages.containedOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
          FailureMessages.didNotContainOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain inOrderOnly ("one", "two")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(only: ResultOfInOrderOnlyApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(only: ResultOfInOrderOnlyApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = only.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sequencing.containsInOrderOnly(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos)
        else indicateSuccess(
          shouldBeTrue,
          FailureMessages.containedInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
          FailureMessages.didNotContainInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain allOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(only: ResultOfAllOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(only: ResultOfAllOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = only.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAllOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain allElementsOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(only: ResultOfAllElementsOfApplication)(implicit evidence: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(only: ResultOfAllElementsOfApplication)(implicit evidence: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = only.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (evidence.containsAllOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllElementsOf(prettifier, e, right)
            else
              FailureMessages.containedAllElementsOf(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllElementsOf(prettifier, e, right),
            FailureMessages.didNotContainAllElementsOf(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain inOrder ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(inOrder: ResultOfInOrderApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(inOrder: ResultOfInOrderApplication)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = inOrder.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sequencing.containsInOrder(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain inOrderElementsOf (List("one"))
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(inOrderElementsOf: ResultOfInOrderElementsOfApplication)(implicit evidence: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(inOrderElementsOf: ResultOfInOrderElementsOfApplication)(implicit evidence: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = inOrderElementsOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (evidence.containsInOrder(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllElementsOfInOrder(prettifier, e, right)
            else
              FailureMessages.containedAllElementsOfInOrder(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllElementsOfInOrder(prettifier, e, right),
            FailureMessages.didNotContainAllElementsOfInOrder(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain atMostOneOf ("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(atMostOneOf: ResultOfAtMostOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(atMostOneOf: ResultOfAtMostOneOfApplication)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = atMostOneOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAtMostOneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should not contain atMostOneElementOf List("one")
     *                     ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(atMostOneElementOf: ResultOfAtMostOneElementOfApplication)(implicit evidence: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(atMostOneElementOf: ResultOfAtMostOneElementOfApplication)(implicit evidence: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  

      val right = atMostOneElementOf.right

      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (evidence.containsAtMostOneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAtMostOneElementOf(prettifier, e, right)
            else
              FailureMessages.containedAtMostOneElementOf(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAtMostOneElementOf(prettifier, e, right),
            FailureMessages.didNotContainAtMostOneElementOf(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not contain key ("three")
     *                          ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication)(implicit keyMapping: KeyMapping[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication)(implicit keyMapping: KeyMapping[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { map =>
        val expectedKey = resultOfKeyWordApplication.expectedKey
        if ((keyMapping.containsKey(map, expectedKey)) != shouldBeTrue) {
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainKey(prettifier, map, expectedKey)
            else
              FailureMessages.containedKey(prettifier, map, expectedKey),
            None,
            pos
          )
        }
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedKey(prettifier, map, expectedKey),
            FailureMessages.didNotContainKey(prettifier, map, expectedKey)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not contain value (3)
     *                          ^
     * </pre>
     */
    //DOTTY-ONLY infix def contain(resultOfValueWordApplication: ResultOfValueWordApplication)(implicit valueMapping: ValueMapping[T]): Assertion = {
    // SKIP-DOTTY-START 
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication)(implicit valueMapping: ValueMapping[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { map =>
        val expectedValue = resultOfValueWordApplication.expectedValue
        if ((valueMapping.containsValue(map, expectedValue)) != shouldBeTrue) {
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainValue(prettifier, map, expectedValue)
            else
              FailureMessages.containedValue(prettifier, map, expectedValue),
            None,
            pos
          )
        }
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedValue(prettifier, map, expectedValue),
            FailureMessages.didNotContainValue(prettifier, map, expectedValue)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not startWith ("1.7")
     *                        ^
     * </pre>
     */
    //DOTTY-ONLY infix def startWith(right: String)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-START 
    def startWith(right: String)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((e.indexOf(right) == 0) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotStartWith(prettifier, e, right)
            else
              FailureMessages.startedWith(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.startedWith(prettifier, e, right),
            FailureMessages.didNotStartWith(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not startWith regex ("Hel*o")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    //DOTTY-ONLY infix def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-START 
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = startWithRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not endWith ("1.7")
     *                        ^
     * </pre>
     */
    //DOTTY-ONLY infix def endWith(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-START 
    def endWith(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((e endsWith expectedSubstring) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotEndWith(prettifier, e, expectedSubstring)
            else
              FailureMessages.endedWith(prettifier, e, expectedSubstring),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.endedWith(prettifier, e, expectedSubstring),
            FailureMessages.didNotEndWith(prettifier, e, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not endWith regex ("wor.d")
     *                        ^
     * </pre>
     */
    //DOTTY-ONLY infix def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-START 
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = endWithRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not include regex ("wo.ld")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    //DOTTY-ONLY infix def include(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-START 
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = includeRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not include ("world")
     *                        ^
     * </pre>
     */
    //DOTTY-ONLY infix def include(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-START 
    def include(expectedSubstring: String)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((e.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotIncludeSubstring(prettifier, e, expectedSubstring)
            else
              FailureMessages.includedSubstring(prettifier, e, expectedSubstring),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.includedSubstring(prettifier, e, expectedSubstring),
            FailureMessages.didNotIncludeSubstring(prettifier, e, expectedSubstring)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    //DOTTY-ONLY infix def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-START 
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = fullyMatchRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfNotWordForCollectedAny([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfNotWordForCollectedAny(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedAny[T](collected: Collected, xs: Iterable[T], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain oneOf (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.oneOfDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (containing.containsOneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
        )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainOneOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain oneElementOf List(1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def oneElementOf(elements: Iterable[Any])(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def oneElementOf(elements: Iterable[Any])(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = elements.toList
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (containing.containsOneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainOneElementOf(prettifier, e, right)
            else
              FailureMessages.containedOneElementOf(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedOneElementOf(prettifier, e, right),
            FailureMessages.didNotContainOneElementOf(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain atLeastOneOf (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.atLeastOneOfDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAtLeastOneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
        )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain atLeastOneElementOf List(1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def atLeastOneElementOf(elements: Iterable[Any])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def atLeastOneElementOf(elements: Iterable[Any])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = elements.toList
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAtLeastOneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right)
            else
              FailureMessages.containedAtLeastOneElementOf(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAtLeastOneElementOf(prettifier, e, right),
            FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain noneOf (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.noneOfDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (containing.containsNoneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.didNotContainAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.containedAtLeastOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain noElementsOf (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def noElementsOf(elements: Iterable[Any])(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def noElementsOf(elements: Iterable[Any])(implicit containing: Containing[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = elements.toList
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (containing.containsNoneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.containedAtLeastOneElementOf(prettifier, e, right)
            else
              FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.didNotContainAtLeastOneElementOf(prettifier, e, right),
            FailureMessages.containedAtLeastOneElementOf(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain theSameElementsAs (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def theSameElementsAs(right: Iterable[_])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def theSameElementsAs(right: Iterable[_])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsTheSameElementsAs(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainSameElements(prettifier, e, right)
            else
              FailureMessages.containedSameElements(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedSameElements(prettifier, e, right),
            FailureMessages.didNotContainSameElements(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain theSameElementsInOrderAs (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def theSameElementsInOrderAs(right: Iterable[_])(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def theSameElementsInOrderAs(right: Iterable[_])(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sequencing.containsTheSameElementsInOrderAs(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainSameElementsInOrder(prettifier, e, right)
            else
              FailureMessages.containedSameElementsInOrder(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedSameElementsInOrder(prettifier, e, right),
            FailureMessages.didNotContainSameElementsInOrder(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain only (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def only(right: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def only(right: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      if (right.isEmpty)
        throw new NotAllowedException(FailureMessages.onlyEmpty, pos)
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.onlyDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsOnly(e, right) != shouldBeTrue) {
          val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[Iterable[_]] || right(0).isInstanceOf[Every[_]])
          indicateFailure(
            if (shouldBeTrue)
              if (withFriendlyReminder)
                FailureMessages.didNotContainOnlyElementsWithFriendlyReminder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
              else
                FailureMessages.didNotContainOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              if (withFriendlyReminder)
                FailureMessages.containedOnlyElementsWithFriendlyReminder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
              else
                FailureMessages.containedOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        }
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain inOrderOnly (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.inOrderOnlyDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sequencing.containsInOrderOnly(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainInOrderOnlyElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain allOf (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.allOfDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAllOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAllOfElements(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain allElementsOf (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def allElementsOf(elements: Iterable[Any])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def allElementsOf(elements: Iterable[Any])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = elements.toList
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAllOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllElementsOf(prettifier, e, right)
            else
              FailureMessages.containedAllElementsOf(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllElementsOf(prettifier, e, right),
            FailureMessages.didNotContainAllElementsOf(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain inOrder (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.inOrderDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sequencing.containsInOrder(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAllOfElementsInOrder(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * option should contain inOrderElementsOf (1, 2)
     *                       ^
     * </pre>
     */
    //DOTTY-ONLY infix def inOrderElementsOf(elements: Iterable[Any])(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-START 
    def inOrderElementsOf(elements: Iterable[Any])(implicit sequencing: Sequencing[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = elements.toList
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (sequencing.containsInOrder(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAllElementsOfInOrder(prettifier, e, right)
            else
              FailureMessages.containedAllElementsOfInOrder(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAllElementsOfInOrder(prettifier, e, right),
            FailureMessages.didNotContainAllElementsOfInOrder(prettifier, e, right)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should contain atMostOneOf (1, 2)
     *                        ^
     * </pre>
     */
    //DOTTY-ONLY infix def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages.atMostOneOfDuplicate, pos)
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAtMostOneOf(e, right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            else
              FailureMessages.containedAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))),
            FailureMessages.didNotContainAtMostOneOf(prettifier, e, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should contain atMostOneElementOf (1, 2)
     *                        ^
     * </pre>
     */
    //DOTTY-ONLY infix def atMostOneElementOf(elements: Iterable[Any])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-START 
    def atMostOneElementOf(elements: Iterable[Any])(implicit aggregating: Aggregating[T]): Assertion = {
    // SKIP-DOTTY-END  
      val right = elements.toList
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (aggregating.containsAtMostOneOf(e, right.distinct) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainAtMostOneElementOf(prettifier, e, right)
            else
              FailureMessages.containedAtMostOneElementOf(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedAtMostOneElementOf(prettifier, e, right),
            FailureMessages.didNotContainAtMostOneElementOf(prettifier, e, right)
          )
      }
    }

   /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain key ("one")
     *                              ^
     * </pre>
     */
    //DOTTY-ONLY infix def key(expectedKey: Any)(implicit keyMapping: KeyMapping[T]): Assertion = {
    // SKIP-DOTTY-START 
    def key(expectedKey: Any)(implicit keyMapping: KeyMapping[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { map =>
        if (keyMapping.containsKey(map, expectedKey) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainKey(prettifier, map, expectedKey)
            else
              FailureMessages.containedKey(prettifier, map, expectedKey),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedKey(prettifier, map, expectedKey),
            FailureMessages.didNotContainKey(prettifier, map, expectedKey)
          )
      }
    }

   /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain value (1)
     *                              ^
     * </pre>
     */
    //DOTTY-ONLY infix def value(expectedValue: Any)(implicit valueMapping: ValueMapping[T]): Assertion = {
    // SKIP-DOTTY-START 
    def value(expectedValue: Any)(implicit valueMapping: ValueMapping[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { map =>
        if (valueMapping.containsValue(map, expectedValue) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.didNotContainValue(prettifier, map, expectedValue)
            else
              FailureMessages.containedValue(prettifier, map, expectedValue),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.containedValue(prettifier, map, expectedValue),
            FailureMessages.didNotContainValue(prettifier, map, expectedValue)
          )
      }
    }
    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfContainWordForCollectedAny([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfContainWordForCollectedAny(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfBeWordForCollectedAny[T](collected: Collected, xs: Iterable[T], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    // TODO: Missing should(AMatcher) and should(AnMatcher)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    //DOTTY-ONLY infix def theSameInstanceAs(right: AnyRef)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def theSameInstanceAs(right: AnyRef)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((toAnyRef(e) eq right) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.wasNotSameInstanceAs(prettifier, e, right)
            else
              FailureMessages.wasSameInstanceAs(prettifier, e, right),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.wasSameInstanceAs(prettifier, e, right),
            FailureMessages.wasNotSameInstanceAs(prettifier, e, right)
          )
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be a ('file)
     *                   ^
     * </pre>
     */
    //DOTTY-ONLY infix def a(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def a(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, true, true, prettifier, pos)
        if (matcherResult.matches != shouldBeTrue) {
          indicateFailure(
            if (shouldBeTrue)
              matcherResult.failureMessage(prettifier)
            else
              matcherResult.negatedFailureMessage(prettifier),
            None,
            pos
          )
        }
        else
          indicateSuccess(
            shouldBeTrue,
            matcherResult.negatedFailureMessage(prettifier),
            matcherResult.failureMessage(prettifier)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be an ('orange)
     *                   ^
     * </pre>
     */
    //DOTTY-ONLY infix def an(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def an(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, true, false, prettifier, pos)
        if (matcherResult.matches != shouldBeTrue) {
          indicateFailure(
            if (shouldBeTrue)
              matcherResult.failureMessage(prettifier)
            else
              matcherResult.negatedFailureMessage(prettifier),
            None,
            pos
          )
        }
        else
          indicateSuccess(
            shouldBeTrue,
            matcherResult.negatedFailureMessage(prettifier),
            matcherResult.failureMessage(prettifier)
          )
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be a (goodRead)
     *                      ^
     * </pre>
     */
    //DOTTY-ONLY infix def a[U <: T](bePropertyMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try supporting 2.10 AnyVals
    // SKIP-DOTTY-START 
    def a[U <: T](bePropertyMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try supporting 2.10 AnyVals
    // SKIP-DOTTY-END
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = bePropertyMatcher(e.asInstanceOf[U])
        if (result.matches != shouldBeTrue) {
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.wasNotA(prettifier, e, UnquotedString(result.propertyName))
            else
              FailureMessages.wasA(prettifier, e, UnquotedString(result.propertyName)),
            None,
            pos
          )
        }
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.wasA(prettifier, e, UnquotedString(result.propertyName)),
            FailureMessages.wasNotA(prettifier, e, UnquotedString(result.propertyName))
          )
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be an (excellentRead)
     *                      ^
     * </pre>
     */
    //DOTTY-ONLY infix def an[U <: T](beTrueMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try supporting 2.10 AnyVals
    // SKIP-DOTTY-START 
    def an[U <: T](beTrueMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try supporting 2.10 AnyVals
    // SKIP-DOTTY-END
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val beTrueMatchResult = beTrueMatcher(e.asInstanceOf[U])
        if (beTrueMatchResult.matches != shouldBeTrue) {
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.wasNotAn(prettifier, e, UnquotedString(beTrueMatchResult.propertyName))
            else
              FailureMessages.wasAn(prettifier, e, UnquotedString(beTrueMatchResult.propertyName)),
            None,
            pos
          )
        }
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.wasAn(prettifier, e, UnquotedString(beTrueMatchResult.propertyName)),
            FailureMessages.wasNotAn(prettifier, e, UnquotedString(beTrueMatchResult.propertyName))
          )
      }
    }

    /**
     * This method enables the following syntax, where <code>fraction</code> is, for example, of type <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * all(xs) should be definedAt (6)
     *                   ^
     * </pre>
     */
    //DOTTY-ONLY infix def definedAt[U](right: U)(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    // SKIP-DOTTY-START 
    def definedAt[U](right: U)(implicit ev: T <:< PartialFunction[U, _]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, xs, prettifier, pos) { e =>
      if (e.isDefinedAt(right) != shouldBeTrue)
        indicateFailure(
          if (shouldBeTrue)
            FailureMessages.wasNotDefinedAt(prettifier, e, right)
          else
            FailureMessages.wasDefinedAt(prettifier, e, right),
          None,
          pos
        )
        else
        indicateSuccess(
          shouldBeTrue,
          FailureMessages.wasDefinedAt(prettifier, e, right),
          FailureMessages.wasNotDefinedAt(prettifier, e, right)
        )
      }
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfBeWordForCollectedAny([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfBeWordForCollectedAny(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfBeWordForCollectedArray[T](collected: Collected, xs: Iterable[Array[T]], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position)
    extends ResultOfBeWordForCollectedAny(collected, xs, original, shouldBeTrue, prettifier, pos) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should be ('empty)
     *                           ^
     * </pre>
     */
    def apply(right: Symbol): Matcher[Array[T]] =
      new Matcher[Array[T]] {
        def apply(left: Array[T]): MatchResult = matchSymbolToPredicateMethod(deep(left), right, false, false, prettifier, pos)
      }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfBeWordForCollectedArray([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfBeWordForCollectedArray(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedAny[T](collected: Collected, xs: Iterable[T], original: Any, prettifier: Prettifier, pos: source.Position) {

// TODO: shouldBe null works, b ut should be (null) does not when type is Any:
/*
scala> val ys = List(null, null, 1)
ys: List[Any] = List(null, null, 1)

scala> all (ys) shouldBe null
<console>:15: error: ambiguous reference to overloaded definition,
both method shouldBe in class ResultOfCollectedAny of type (spread: org.scalactic.Spread[Any])Unit
and  method shouldBe in class ResultOfCollectedAny of type (beMatcher: org.scalatest.matchers.BeMatcher[Any])Unit
match argument types (Null)
              all (ys) shouldBe null
                       ^

scala> all (ys) should be (null)
org.scalatest.exceptions.TestFailedException: org.scalatest.Matchers$ResultOfCollectedAny@18515783 was not null
	at org.scalatest.MatchersHelper$.newTestFailedException(MatchersHelper.scala:163)
	at org.scalatest.Matchers$ShouldMethodHelper$.shouldMatcher(Matchers.scala:5529)
	at org.scalatest.Matchers$AnyShouldWrapper.should(Matchers.scala:5563)
	at .<init>(<console>:15)
	at .<clinit>(<console>)
*/

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be (3)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(rightMatcher: Matcher[T]): Assertion = {
    // SKIP-DOTTY-START 
    def should(rightMatcher: Matcher[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = rightMatcher(e)
        result match {
          case resultWithAnalysis: MatchResultWithAnalysis =>
            if (resultWithAnalysis.matches)
              indicateSuccess(result.negatedFailureMessage(prettifier))
            else {
              val failureMessage = resultWithAnalysis.failureMessage(prettifier)
              val analysis = resultWithAnalysis.analysis
              indicateFailure(failureMessage, None, pos, analysis)
            }

          case _ =>
            MatchFailed.unapply(result)(prettifier) match {
              case Some(failureMessage) => indicateFailure(failureMessage, None, pos)
              case None => indicateSuccess(result.negatedFailureMessage(prettifier))
            }
        }
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) shouldEqual 7
     *          ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldEqual(right: Any)(implicit equality: Equality[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldEqual(right: Any)(implicit equality: Equality[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!equality.areEqual(e, right)) {

          val prettyPair = prettifier(e, right)
          indicateFailure(Resources.formatString(Resources.rawDidNotEqual, Array(prettyPair.left, prettyPair.right)), None, pos, prettyPair.analysis)
        }
        else indicateSuccess(FailureMessages.equaled(prettifier, e, right))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual 7.1 +- 0.2
     *        ^doCollected
     * </pre>
     */
    //DOTTY-ONLY infix def shouldEqual(spread: Spread[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldEqual(spread: Spread[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!spread.isWithin(e)) {
          indicateFailure(FailureMessages.didNotEqualPlusOrMinus(prettifier, e, spread.pivot, spread.tolerance), None, pos)
        }
        else indicateSuccess(FailureMessages.equaledPlusOrMinus(prettifier, e, spread.pivot, spread.tolerance))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe sorted
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(sortedWord: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(sortedWord: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!sortable.isSorted(e))
          indicateFailure(FailureMessages.wasNotSorted(prettifier, e), None, pos)
        else indicateSuccess(FailureMessages.wasSorted(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe readable
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(readableWord: ReadableWord)(implicit readability: Readability[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(readableWord: ReadableWord)(implicit readability: Readability[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!readability.isReadable(e))
          indicateFailure(FailureMessages.wasNotReadable(prettifier, e), None, pos)
        else indicateSuccess(FailureMessages.wasReadable(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe writable
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(writableWord: WritableWord)(implicit writability: Writability[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(writableWord: WritableWord)(implicit writability: Writability[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!writability.isWritable(e))
          indicateFailure(FailureMessages.wasNotWritable(prettifier, e), None, pos)
        else indicateSuccess(FailureMessages.wasWritable(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe empty
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!emptiness.isEmpty(e))
          indicateFailure(FailureMessages.wasNotEmpty(prettifier, e), None, pos)
        else indicateSuccess(FailureMessages.wasEmpty(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe defined
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(definedWord: DefinedWord)(implicit definition: Definition[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(definedWord: DefinedWord)(implicit definition: Definition[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!definition.isDefined(e))
          indicateFailure(FailureMessages.wasNotDefined(prettifier, e), None, pos)
        else indicateSuccess(FailureMessages.wasDefined(prettifier, e))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe a [Type]
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(aType: ResultOfATypeInvocation[_]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(aType: ResultOfATypeInvocation[_]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!aType.clazz.isAssignableFrom(e.getClass))
          indicateFailure(FailureMessages.wasNotAnInstanceOf(prettifier, e, UnquotedString(aType.clazz.getName), UnquotedString(e.getClass.getName)), None, pos)
        else indicateSuccess(FailureMessages.wasAnInstanceOf(prettifier, e, UnquotedString(aType.clazz.getName)))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe an [Type]
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(anType: ResultOfAnTypeInvocation[_]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(anType: ResultOfAnTypeInvocation[_]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!anType.clazz.isAssignableFrom(e.getClass))
          indicateFailure(FailureMessages.wasNotAnInstanceOf(prettifier, e, UnquotedString(anType.clazz.getName), UnquotedString(e.getClass.getName)), None, pos)
        else indicateSuccess(FailureMessages.wasAnInstanceOf(prettifier, e, UnquotedString(anType.clazz.getName)))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual null
     *        ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldEqual(right: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def shouldEqual(right: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (e != null) {
          indicateFailure(FailureMessages.didNotEqualNull(prettifier, e), None, pos)
        }
        else indicateSuccess(FailureMessages.equaledNull)
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
    // SKIP-DOTTY-START 
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
    // SKIP-DOTTY-END  
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = rightMatcher(e)
        result match {
          case resultWithAnalysis: MatchResultWithAnalysis =>
            if (resultWithAnalysis.matches)
              indicateSuccess(result.negatedFailureMessage(prettifier))
            else {
              val failureMessage = resultWithAnalysis.failureMessage(prettifier)
              val analysis = resultWithAnalysis.analysis
              indicateFailure(failureMessage, None, pos, analysis)
            }

          case _ =>
            MatchFailed.unapply(result)(prettifier) match {
              case Some(failureMessage) => indicateFailure(failureMessage, None, pos)
              case None => indicateSuccess(result.negatedFailureMessage(prettifier))
            }
        }
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should (equal (expected) and have length 12)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]): Assertion = {
    // SKIP-DOTTY-START 
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]): Assertion = {
    // SKIP-DOTTY-END  
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = rightMatcher(e)
        result match {
          case resultWithAnalysis: MatchResultWithAnalysis =>
            if (resultWithAnalysis.matches)
              indicateSuccess(result.negatedFailureMessage(prettifier))
            else {
              val failureMessage = resultWithAnalysis.failureMessage(prettifier)
              val analysis = resultWithAnalysis.analysis
              indicateFailure(failureMessage, None, pos, analysis)
            }

          case _ =>
            MatchFailed.unapply(result)(prettifier) match {
              case Some(failureMessage) => indicateFailure(failureMessage, None, pos)
              case None => indicateSuccess(result.negatedFailureMessage(prettifier))
            }
        }
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(beWord: BeWord): ResultOfBeWordForCollectedAny[T] =
    // SKIP-DOTTY-START 
    def should(beWord: BeWord): ResultOfBeWordForCollectedAny[T] =
    // SKIP-DOTTY-END
      new ResultOfBeWordForCollectedAny[T](collected, xs, original, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(notWord: NotWord): ResultOfNotWordForCollectedAny[T] =
    // SKIP-DOTTY-START 
    def should(notWord: NotWord): ResultOfNotWordForCollectedAny[T] =
    // SKIP-DOTTY-END
      new ResultOfNotWordForCollectedAny(collected, xs, original, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (results) should have length (3)
     *        ^
     * all (results) should have size (3)
     *        ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(haveWord: HaveWord): ResultOfHaveWordForCollectedExtent[T] =
    // SKIP-DOTTY-START 
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedExtent[T] =
    // SKIP-DOTTY-END
      new ResultOfHaveWordForCollectedExtent(collected, xs, original, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) shouldBe 7
     *          ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: Any): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY infix def shouldBe[R](right: R)(implicit caneq: scala.CanEqual[T, R]): Assertion = {
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (e != right) {
          val (eee, rightee) = Suite.getObjectsForFailureMessage(e, right)
          indicateFailure(FailureMessages.wasNot(prettifier, eee, rightee), None, pos)
        }
        else indicateSuccess(FailureMessages.was(prettifier, e, right))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(4, 5, 6) shouldBe &lt; (7)
     *              ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(comparison: ResultOfLessThanComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfLessThanComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!comparison(e)) {
          indicateFailure(
            FailureMessages.wasNotLessThan(prettifier,
              e,
              comparison.right
            ),
            None,
            pos
          )
        }
        else indicateSuccess(FailureMessages.wasLessThan(prettifier, e, comparison.right))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(4, 5, 6) shouldBe &lt;= (7)
     *              ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!comparison(e)) {
          indicateFailure(
            FailureMessages.wasNotLessThanOrEqualTo(prettifier,
              e,
              comparison.right
            ),
            None,
            pos
          )
        }
        else indicateSuccess(FailureMessages.wasLessThanOrEqualTo(prettifier, e, comparison.right))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(8, 9, 10) shouldBe &gt; (7)
     *               ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!comparison(e)) {
          indicateFailure(
            FailureMessages.wasNotGreaterThan(prettifier,
              e,
              comparison.right
            ),
            None,
            pos
          )
        }
        else indicateSuccess(FailureMessages.wasGreaterThan(prettifier, e, comparison.right))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(8, 9, 10) shouldBe &gt;= (7)
     *               ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!comparison(e)) {
          indicateFailure(
            FailureMessages.wasNotGreaterThanOrEqualTo(prettifier,
              e,
              comparison.right
            ),
            None,
            pos
          )
        }
        else indicateSuccess(FailureMessages.wasGreaterThanOrEqualTo(prettifier, e, comparison.right))
      }
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">testing
     * all(xs) shouldBe odd
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(beMatcher: BeMatcher[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(beMatcher: BeMatcher[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = beMatcher.apply(e)
        if (!result.matches)
          indicateFailure(result.failureMessage(prettifier), None, pos)
        else indicateSuccess(result.negatedFailureMessage(prettifier))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe 7.1 +- 0.2
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(spread: Spread[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(spread: Spread[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!spread.isWithin(e))
          indicateFailure(FailureMessages.wasNotPlusOrMinus(prettifier, e, spread.pivot, spread.tolerance), None, pos)
        else indicateSuccess(FailureMessages.wasPlusOrMinus(prettifier, e, spread.pivot, spread.tolerance))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe theSameInstanceAs (anotherObject)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (toAnyRef(e) ne resultOfSameInstanceAsApplication.right)
          indicateFailure(
            FailureMessages.wasNotSameInstanceAs(prettifier,
              e,
              resultOfSameInstanceAsApplication.right
            ),
            None,
            pos
          )
        else indicateSuccess(FailureMessages.wasSameInstanceAs(prettifier, e, resultOfSameInstanceAsApplication.right))
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe 'empty
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, false, true, prettifier, pos)
        if (!matcherResult.matches)
          indicateFailure(matcherResult.failureMessage(prettifier), None, pos)
        else indicateSuccess(matcherResult.negatedFailureMessage(prettifier))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe a ('empty)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAWordApplication.symbol, true, true, prettifier, pos)
        if (!matcherResult.matches) {
          indicateFailure(matcherResult.failureMessage(prettifier), None, pos)
        }
        else indicateSuccess(matcherResult.negatedFailureMessage(prettifier))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe an ('empty)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAnWordApplication.symbol, true, false, prettifier, pos)
        if (!matcherResult.matches) {
          indicateFailure(matcherResult.failureMessage(prettifier), None, pos)
        }
        else indicateSuccess(matcherResult.negatedFailureMessage(prettifier))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe null
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe(o: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-START 
    def shouldBe(o: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (e != null)
         indicateFailure(FailureMessages.wasNotNull(prettifier, e), None, pos)
        else indicateSuccess(FailureMessages.wasNull)
      }
    }

    /**
     * This method enables the following syntax, where <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe excellentRead
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe[U <: T](bePropertyMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try supporting this with 2.10 AnyVals
    // SKIP-DOTTY-START 
    def shouldBe[U <: T](bePropertyMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try supporting this with 2.10 AnyVals
    // SKIP-DOTTY-END
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = bePropertyMatcher(e.asInstanceOf[U])
        if (!result.matches)
          indicateFailure(FailureMessages.wasNot(prettifier, e, UnquotedString(result.propertyName)), None, pos)
        else indicateSuccess(FailureMessages.was(prettifier, e, UnquotedString(result.propertyName)))
      }
    }

    /**
     * This method enables the following syntax, where <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe a (goodRead)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe[U <: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {// TODO: Try supporting this with 2.10 AnyVals
    // SKIP-DOTTY-START 
    def shouldBe[U <: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {// TODO: Try supporting this with 2.10 AnyVals
    // SKIP-DOTTY-END
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = resultOfAWordApplication.bePropertyMatcher(e.asInstanceOf[U])
        if (!result.matches)
          indicateFailure(FailureMessages.wasNotA(prettifier, e, UnquotedString(result.propertyName)), None, pos)
        else indicateSuccess(FailureMessages.was(prettifier, e, UnquotedString(result.propertyName)))
      }
    }

    /**
     * This method enables the following syntax, where <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe an (excellentRead)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldBe[U <: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {// TODO: Try supporting this with 2.10 AnyVals
    // SKIP-DOTTY-START 
    def shouldBe[U <: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {// TODO: Try supporting this with 2.10 AnyVals
    // SKIP-DOTTY-END
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = resultOfAnWordApplication.bePropertyMatcher(e.asInstanceOf[U])
        if (!result.matches)
          indicateFailure(FailureMessages.wasNotAn(prettifier, e, UnquotedString(result.propertyName)), None, pos)
        else indicateSuccess(FailureMessages.wasAn(prettifier, e, UnquotedString(result.propertyName)))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) shouldNot (be (3))
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot[U <: T](rightMatcherX1: Matcher[U]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldNot[U <: T](rightMatcherX1: Matcher[U]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        try  {
          val result = rightMatcherX1.apply(e.asInstanceOf[U])
          if (result.matches)
            indicateFailure(result.negatedFailureMessage(prettifier), None, pos)
          else indicateSuccess(result.failureMessage(prettifier))
        }
        catch {
          case tfe: TestFailedException =>
            indicateFailure(tfe.getMessage, tfe.cause, pos)
        }
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) shouldNot (equal (3))
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldNot[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
    // SKIP-DOTTY-END  
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = rightMatcher(e)
        MatchSucceeded.unapply(result)(prettifier) match {
          case Some(negatedFailureMessage) =>
            indicateFailure(negatedFailureMessage, None, pos)
          case None => indicateSuccess(result.failureMessage(prettifier))
        }
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) should === (b)
     *          ^
     * </pre>
     */
    //DOTTY-ONLY infix def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: T CanEqual U): Assertion = {
    // SKIP-DOTTY-START 
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: T CanEqual U): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((constraint.areEqual(e, inv.right)) != inv.expectingEqual)
          indicateFailure(
            if (inv.expectingEqual)
              FailureMessages.didNotEqual(prettifier, e, inv.right)
            else
              FailureMessages.equaled(prettifier, e, inv.right),
            None,
            pos
          )
        else indicateSuccess(inv.expectingEqual, FailureMessages.equaled(prettifier, e, inv.right), FailureMessages.didNotEqual(prettifier, e, inv.right))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) should === (100 +- 1)
     *          ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(inv: TripleEqualsInvocationOnSpread[T])(implicit ev: Numeric[T]): Assertion = {
    // SKIP-DOTTY-START 
    def should(inv: TripleEqualsInvocationOnSpread[T])(implicit ev: Numeric[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if ((inv.spread.isWithin(e)) != inv.expectingEqual)
          indicateFailure(
            if (inv.expectingEqual)
              FailureMessages.didNotEqualPlusOrMinus(prettifier, e, inv.spread.pivot, inv.spread.tolerance)
            else
              FailureMessages.equaledPlusOrMinus(prettifier, e, inv.spread.pivot, inv.spread.tolerance),
            None,
            pos
          )
        else indicateSuccess(inv.expectingEqual, FailureMessages.equaledPlusOrMinus(prettifier, e, inv.spread.pivot, inv.spread.tolerance), FailureMessages.didNotEqualPlusOrMinus(prettifier, e, inv.spread.pivot, inv.spread.tolerance))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldNot be theSameInstanceAs anotherInstance
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot(beWord: BeWord): ResultOfBeWordForCollectedAny[T] =
    // SKIP-DOTTY-START 
    def shouldNot(beWord: BeWord): ResultOfBeWordForCollectedAny[T] =
    // SKIP-DOTTY-END
      new ResultOfBeWordForCollectedAny[T](collected, xs, original, false, prettifier, pos)

   /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) should contain oneOf (1, 2, 3)
     *          ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(containWord: ContainWord): ResultOfContainWordForCollectedAny[T] = {
    // SKIP-DOTTY-START 
    def should(containWord: ContainWord): ResultOfContainWordForCollectedAny[T] = {
    // SKIP-DOTTY-END  
      new ResultOfContainWordForCollectedAny(collected, xs, original, true, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) shouldNot contain (oneOf (1, 2, 3))
     *          ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot(containWord: ContainWord): ResultOfContainWordForCollectedAny[T] = {
    // SKIP-DOTTY-START 
    def shouldNot(containWord: ContainWord): ResultOfContainWordForCollectedAny[T] = {
    // SKIP-DOTTY-END  
      new ResultOfContainWordForCollectedAny(collected, xs, original, false, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should exist
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-START 
    def should(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (!existence.exists(e))
          indicateFailure(
            FailureMessages.doesNotExist(prettifier, e),
            None,
            pos
          )
        else indicateSuccess(FailureMessages.exists(prettifier, e))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not (exist)
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(notExist: ResultOfNotExist)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-START 
    def should(notExist: ResultOfNotExist)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (existence.exists(e))
          indicateFailure(
            FailureMessages.exists(prettifier, e),
            None,
            pos
          )
        else indicateSuccess(FailureMessages.doesNotExist(prettifier, e))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) shouldNot exist
     *         ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-START 
    def shouldNot(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        if (existence.exists(e))
          indicateFailure(
            FailureMessages.exists(prettifier, e),
            None,
            pos
          )
        else indicateSuccess(FailureMessages.doesNotExist(prettifier, e))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForCollectedString =
    // SKIP-DOTTY-START 
    def should(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfStartWithWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wo.ld")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForCollectedString =
    // SKIP-DOTTY-START 
    def should(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfEndWithWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForCollectedString =
    // SKIP-DOTTY-START 
    def should(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfIncludeWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def should(fullyMatchWord: FullyMatchWord)(implicit ev: T <:< String): ResultOfFullyMatchWordForCollectedString =
    // SKIP-DOTTY-START 
    def should(fullyMatchWord: FullyMatchWord)(implicit ev: T <:< String): ResultOfFullyMatchWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfFullyMatchWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot(fullyMatchWord: FullyMatchWord)(implicit ev: T <:< String): ResultOfFullyMatchWordForCollectedString =
    // SKIP-DOTTY-START 
    def shouldNot(fullyMatchWord: FullyMatchWord)(implicit ev: T <:< String): ResultOfFullyMatchWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfFullyMatchWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot startWith regex ("Hel*o")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForCollectedString =
    // SKIP-DOTTY-START 
    def shouldNot(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfStartWithWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot endWith regex ("wo.ld")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForCollectedString =
    // SKIP-DOTTY-START 
    def shouldNot(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfEndWithWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot include regex ("wo.ld")
     *             ^
     * </pre>
     */
    //DOTTY-ONLY infix def shouldNot(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForCollectedString =
    // SKIP-DOTTY-START 
    def shouldNot(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForCollectedString =
    // SKIP-DOTTY-END
      new ResultOfIncludeWordForCollectedString(collected, xs.asInstanceOf[Iterable[String]], original, false, prettifier, pos)

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfCollectedAny([collected], [xs])"
     */
    override def toString: String = "ResultOfCollectedAny(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForCollectedExtent[A](collected: Collected, xs: Iterable[A], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should have length (12)
     *                      ^
     * </pre>
     */
    //DOTTY-ONLY infix def length(expectedLength: Long)(implicit len: Length[A]): Assertion = {
    // SKIP-DOTTY-START 
    def length(expectedLength: Long)(implicit len: Length[A]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val eLength = len.lengthOf(e)
        if ((eLength == expectedLength) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, e, eLength, expectedLength)
            else
              FailureMessages.hadLength(prettifier, e, expectedLength),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.hadLength(prettifier, e, expectedLength),
            FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, e, eLength, expectedLength)
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all (xs) should have size (12)
     *                      ^
     * </pre>
     */
    //DOTTY-ONLY infix def size(expectedSize: Long)(implicit sz: Size[A]): Assertion = {
    // SKIP-DOTTY-START 
    def size(expectedSize: Long)(implicit sz: Size[A]): Assertion = {
    // SKIP-DOTTY-END  
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val eSize = sz.sizeOf(e)
        if ((eSize == expectedSize) != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, e, eSize, expectedSize)
            else
              FailureMessages.hadSize(prettifier, e, expectedSize),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            FailureMessages.hadSize(prettifier, e, expectedSize),
            FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, e, eSize, expectedSize)
          )
      }
    }
    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfHaveWordForCollectedExtent([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfHaveWordForCollectedExtent(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfStartWithWordForCollectedString(collected: Collected, xs: Iterable[String], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *                              ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegexString.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("a(b*)c" withGroup "bb")
     *                              ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(regexWithGroups.regex, regexWithGroups.groups)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o".r)
     *                              ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegex)

    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty): Assertion = {
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = startWithRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfStartWithWordForCollectedString([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfStartWithWordForCollectedString(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfIncludeWordForCollectedString(collected: Collected, xs: Iterable[String], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("world")
     *                            ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegexString.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("a(b*)c" withGroup "bb")
     *                            ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(regexWithGroups.regex, regexWithGroups.groups)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld".r)
     *                            ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegex)

    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty): Assertion = {
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = includeRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfIncludeWordForCollectedString([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfIncludeWordForCollectedString(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfEndWithWordForCollectedString(collected: Collected, xs: Iterable[String], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d")
     *                            ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegexString.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("a(b*)c" withGroup "bb")
     *                            ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(regexWithGroups.regex, regexWithGroups.groups)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d".r)
     *                            ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegex)

    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty): Assertion = {
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = endWithRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfEndWithWordForCollectedString([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfEndWithWordForCollectedString(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfFullyMatchWordForCollectedString(collected: Collected, xs: Iterable[String], original: Any, shouldBeTrue: Boolean, prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("Hel*o world")
     *                              ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegexString: String): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegexString.r)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("a(b*)c" withGroup "bb")
     *                              ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-START 
    def regex(regexWithGroups: RegexWithGroups): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(regexWithGroups.regex, regexWithGroups.groups)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should fullymatch regex ("Hel*o world".r)
     *                               ^
     * </pre>
     */
    //DOTTY-ONLY infix def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-START 
    def regex(rightRegex: Regex): Assertion = 
    // SKIP-DOTTY-END
      checkRegex(rightRegex)

    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty): Assertion = {
      doCollected(collected, xs, original, prettifier, pos) { e =>
        val result = fullyMatchRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          indicateFailure(
            if (shouldBeTrue)
              result.failureMessage(prettifier)
            else
              result.negatedFailureMessage(prettifier),
            None,
            pos
          )
        else
          indicateSuccess(
            shouldBeTrue,
            result.negatedFailureMessage(prettifier),
            result.failureMessage(prettifier)
          )
      }
    }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfFullyMatchWordForCollectedString([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfFullyMatchWordForCollectedString(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * all(xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def all[E, C[_]](xs: C[E])(implicit collecting: Collecting[E, C[E]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(AllCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
    * This method enables the following syntax for <code>scala.collection.GenMap</code>:
    *
    * <pre class="stHighlight">
    * all(map) should fullymatch regex ("Hel*o world".r)
    * ^
    * </pre>
    */
  def all[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](xs: MAP[K, V])(implicit collecting: Collecting[(K, V), Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[(K, V)] =
    new ResultOfCollectedAny(AllCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * all(jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def all[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(AllCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * all(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def all(xs: String)(implicit collecting: Collecting[Char, String], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(AllCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * atLeast(1, xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atLeast[E, C[_]](num: Int, xs: C[E])(implicit collecting: Collecting[E, C[E]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(AtLeastCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
    * This method enables the following syntax for <code>scala.collection.GenMap</code>:
    *
    * <pre class="stHighlight">
    * atLeast(1, map) should fullymatch regex ("Hel*o world".r)
    * ^
    * </pre>
    */
  def atLeast[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](num: Int, xs: MAP[K, V])(implicit collecting: Collecting[(K, V), Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[(K, V)] =
    new ResultOfCollectedAny(AtLeastCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * atLeast(1, jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atLeast[K, V, JMAP[k, v] <: java.util.Map[k, v]](num: Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(AtLeastCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * atLeast(1, str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atLeast(num: Int, xs: String)(implicit collecting: Collecting[Char, String], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(AtLeastCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * every(xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def every[E, C[_]](xs: C[E])(implicit collecting: Collecting[E, C[E]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(EveryCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
    * This method enables the following syntax for <code>scala.collection.GenMap</code>:
    *
    * <pre class="stHighlight">
    * every(map) should fullymatch regex ("Hel*o world".r)
    * ^
    * </pre>
    */
  def every[K, V, MAP[k, v] <: scala.collection.Map[k, v]](xs: MAP[K, V])(implicit collecting: Collecting[(K, V), Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[(K, V)] =
    new ResultOfCollectedAny(EveryCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * every(jmap) should fullymatch regex ("Hel*o world".r)
  
   * ^
   * </pre>
   */
  def every[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(EveryCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * every(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def every(xs: String)(implicit collecting: Collecting[Char, String], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(EveryCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * exactly(xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def exactly[E, C[_]](num: Int, xs: C[E])(implicit collecting: Collecting[E, C[E]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(ExactlyCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
    * This method enables the following syntax for <code>scala.collection.GenMap</code>:
    *
    * <pre class="stHighlight">
    * exactly(map) should fullymatch regex ("Hel*o world".r)
    * ^
    * </pre>
    */
  def exactly[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](num: Int, xs: MAP[K, V])(implicit collecting: Collecting[(K, V), Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[(K, V)] =
    new ResultOfCollectedAny(ExactlyCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * exactly(jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def exactly[K, V, JMAP[k, v] <: java.util.Map[k, v]](num: Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(ExactlyCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * exactly(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def exactly(num: Int, xs: String)(implicit collecting: Collecting[Char, String], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(ExactlyCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * no(xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */ 
  def no[E, C[_]](xs: C[E])(implicit collecting: Collecting[E, C[E]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(NoCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * no(jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def no[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(NoCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * no(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def no(xs: String)(implicit collecting: Collecting[Char, String], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(NoCollected, collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * between(1, 3, xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def between[E, C[_]](from: Int, upTo:Int, xs: C[E])(implicit collecting: Collecting[E, C[E]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * between(1, 3, jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def between[K, V, JMAP[k, v] <: java.util.Map[k, v]](from: Int, upTo:Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * between(1, 3, str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def between(from: Int, upTo:Int, xs: String)(implicit collecting: Collecting[Char, String], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * atMost(3, xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atMost[E, C[_]](num: Int, xs: C[E])(implicit collecting: Collecting[E, C[E]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(AtMostCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
    * This method enables the following syntax for <code>scala.collection.GenMap</code>:
    *
    * <pre class="stHighlight">
    * atMost(3, map) should fullymatch regex ("Hel*o world".r)
    * ^
    * </pre>
    */
  def atMost[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](num: Int, xs: MAP[K, V])(implicit collecting: Collecting[(K, V), Iterable[(K, V)]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[(K, V)] =
    new ResultOfCollectedAny(AtMostCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * atMost(3, jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atMost[K, V, JMAP[k, v] <: java.util.Map[k, v]](num: Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(AtMostCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * atMost(3, str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atMost(num: Int, xs: String)(implicit collecting: Collecting[Char, String], prettifier: Prettifier, pos: source.Position): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(AtMostCollected(num), collecting.iterableFrom(xs), xs, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy { ... }
   * ^
   * </pre>
   */
  def a[T: ClassTag]: ResultOfATypeInvocation[T] =
    new ResultOfATypeInvocation(classTag)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * an [Exception] should be thrownBy { ... }
   * ^
   * </pre>
   */
  def an[T : ClassTag]: ResultOfAnTypeInvocation[T] =
    new ResultOfAnTypeInvocation(classTag)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * the [FileNotFoundException] should be thrownBy { ... }
   * ^
   * </pre>
   */
  def the[T : ClassTag](implicit pos: source.Position): ResultOfTheTypeInvocation[T] =
    new ResultOfTheTypeInvocation(classTag, pos)

  // This is where ShouldMatchers.scala started

  // 13 Feb 2019: Current dotty does not seems to like inner object, this is a work around until the problem is fixed.
  private class ShouldMethodHelperClass {

    def shouldMatcher[T](left: T, rightMatcher: Matcher[T], prettifier: Prettifier, pos: source.Position): Assertion = {
      val result = rightMatcher(left)
      result match {
        case resultWithAnalysis: MatchResultWithAnalysis =>
          if (resultWithAnalysis.matches)
            indicateSuccess(result.negatedFailureMessage(prettifier))
          else {
            val failureMessage = resultWithAnalysis.failureMessage(prettifier)
            val analysis = resultWithAnalysis.analysis
            indicateFailure(failureMessage, None, pos, analysis)
          }

        case _ =>
          MatchFailed.unapply(result)(prettifier) match {
            case Some(failureMessage) => indicateFailure(failureMessage, None, pos)
            case None => indicateSuccess(result.negatedFailureMessage(prettifier))
          }
      }
    }

    def shouldNotMatcher[T](left: T, rightMatcher: Matcher[T], prettifier: Prettifier, pos: source.Position): Assertion = {
      val result = rightMatcher(left)
      MatchSucceeded.unapply(result)(prettifier) match {
        case Some(negatedFailureMessage) => indicateFailure(negatedFailureMessage, None, pos)
        case None => indicateSuccess(result.failureMessage(prettifier))
      }
    }
  }

  private val ShouldMethodHelper = new ShouldMethodHelperClass

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>Any</code>.
   * </p>
   *
   * @author Bill Venners
   */
  sealed class AnyShouldWrapper[T](val leftSideValue: T, val pos: source.Position, val prettifier: Prettifier) {
  //DOTTY-ONLY }  // We need an empty AnyShouldWrapper for now.  

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should be (3)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(rightMatcherX1: Matcher[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(rightMatcherX1: Matcher[T]): Assertion = {
      ShouldMethodHelper.shouldMatcher(leftSideValue, rightMatcherX1, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should equal (3)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T, TYPECLASS1[_]](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
      ShouldMethodHelper.shouldMatcher(leftSideValue, rightMatcherFactory1.matcher, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should (equal (expected) and have length 3)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T, TYPECLASS1[_], TYPECLASS2[_]](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]): Assertion = {
      ShouldMethodHelper.shouldMatcher(leftSideValue, rightMatcherFactory2.matcher, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * a shouldEqual b
     *   ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldEqual(right: Any)(implicit equality: Equality[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldEqual(right: Any)(implicit equality: Equality[T]): Assertion = {  
      if (!equality.areEqual(leftSideValue, right)) {
        val localPrettifier = prettifier // Grabbing a local copy so we don't attempt to serialize AnyShouldWrapper (since first param to indicateFailure is a by-name)
        val p = Prettifier.withEscapingDiffer(localPrettifier)
        val prettyPair = p(leftSideValue, right)
        indicateFailure(Resources.formatString(Resources.rawDidNotEqual, Array(prettyPair.left, prettyPair.right)), None, pos, prettyPair.analysis)
      }
      else indicateSuccess(FailureMessages.equaled(prettifier, leftSideValue, right))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual 7.1 +- 0.2
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START
    def shouldEqual(spread: Spread[T]): Assertion = {
    // SKIP-DOTTY-END  
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldEqual(spread: Spread[T]): Assertion = {
      if (!spread.isWithin(leftSideValue)) {
        indicateFailure(FailureMessages.didNotEqualPlusOrMinus(prettifier, leftSideValue, spread.pivot, spread.tolerance), None, pos)
      }
      else indicateSuccess(FailureMessages.equaledPlusOrMinus(prettifier, leftSideValue, spread.pivot, spread.tolerance))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual null
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldEqual(right: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldEqual(right: Null)(implicit ev: T <:< AnyRef): Assertion = {
      if (leftSideValue != null) {
        indicateFailure(FailureMessages.didNotEqualNull(prettifier, leftSideValue), None, pos)
      }
      else indicateSuccess(FailureMessages.equaledNull)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should not equal (3)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(notWord: NotWord): ResultOfNotWordForAny[T] = 
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(notWord: NotWord): ResultOfNotWordForAny[T] = 
      new ResultOfNotWordForAny[T](leftSideValue, false, prettifier, pos)

    // In 2.10, will work with AnyVals. TODO: Also, Need to ensure Char works
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * a should === (b)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: T CanEqual U): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T, U](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(inv: TripleEqualsInvocation[U])(implicit constraint: T CanEqual U): Assertion = {
      if ((constraint.areEqual(leftSideValue, inv.right)) != inv.expectingEqual)
        indicateFailure(
          if (inv.expectingEqual)
            FailureMessages.didNotEqual(prettifier, leftSideValue, inv.right)
          else
            FailureMessages.equaled(prettifier, leftSideValue, inv.right),
          None,
          pos
        )
      else
        indicateSuccess(
          inv.expectingEqual,
          FailureMessages.equaled(prettifier, leftSideValue, inv.right),
          FailureMessages.didNotEqual(prettifier, leftSideValue, inv.right)
        )
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (100 +- 1)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(inv: TripleEqualsInvocationOnSpread[T])(implicit ev: Numeric[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(inv: TripleEqualsInvocationOnSpread[T])(implicit ev: Numeric[T]): Assertion = {
      if ((inv.spread.isWithin(leftSideValue)) != inv.expectingEqual)
        indicateFailure(
          if (inv.expectingEqual)
            FailureMessages.didNotEqualPlusOrMinus(prettifier, leftSideValue, inv.spread.pivot, inv.spread.tolerance)
          else
            FailureMessages.equaledPlusOrMinus(prettifier, leftSideValue, inv.spread.pivot, inv.spread.tolerance),
          None,
          pos
        )
      else
        indicateSuccess(
          inv.expectingEqual,
          FailureMessages.equaledPlusOrMinus(prettifier, leftSideValue, inv.spread.pivot, inv.spread.tolerance),
          FailureMessages.didNotEqualPlusOrMinus(prettifier, leftSideValue, inv.spread.pivot, inv.spread.tolerance)
        )
    }

    // TODO: Need to make sure this works in inspector shorthands. I moved this
    // up here from NumericShouldWrapper.
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should be a aMatcher
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(beWord: BeWord): ResultOfBeWordForAny[T] = 
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(beWord: BeWord): ResultOfBeWordForAny[T] = 
      new ResultOfBeWordForAny(leftSideValue, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe 8.8
     *         ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: Any): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T, R](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(right: R)(implicit caneq: scala.CanEqual[T, R]): Assertion = {
      if (!areEqualComparingArraysStructurally(leftSideValue, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(leftSideValue, right)
        val localPrettifier = prettifier // Grabbing a local copy so we don't attempt to serialize AnyShouldWrapper (since first param to indicateFailure is a by-name)
        val p = Prettifier.withEscapingDiffer(localPrettifier)
        val prettyPair = p(leftSideValue, right)
        indicateFailure(FailureMessages.wasNotEqualTo(localPrettifier, leftee, rightee), None, pos, prettyPair.analysis)
      }
      else indicateSuccess(FailureMessages.wasEqualTo(prettifier, leftSideValue, right))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 5 shouldBe &lt; (7)
     *   ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfLessThanComparison[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(comparison: ResultOfLessThanComparison[T]): Assertion = {
      if (!comparison(leftSideValue)) {
        indicateFailure(
          FailureMessages.wasNotLessThan(prettifier,
            leftSideValue,
            comparison.right
          ),
          None,
          pos
        )
      }
      else indicateSuccess(FailureMessages.wasLessThan(prettifier, leftSideValue, comparison.right))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 8 shouldBe &gt; (7)
     *   ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(comparison: ResultOfGreaterThanComparison[T]): Assertion = {
      if (!comparison(leftSideValue)) {
        indicateFailure(
          FailureMessages.wasNotGreaterThan(prettifier,
            leftSideValue,
            comparison.right
          ),
          None,
          pos
        )
      }
      else indicateSuccess(FailureMessages.wasGreaterThan(prettifier, leftSideValue, comparison.right))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 5 shouldBe &lt;= (7)
     *   ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(comparison: ResultOfLessThanOrEqualToComparison[T]): Assertion = {
      if (!comparison(leftSideValue)) {
        indicateFailure(
          FailureMessages.wasNotLessThanOrEqualTo(prettifier,
            leftSideValue,
            comparison.right
          ),
          None,
          pos
        )
      }
      else indicateSuccess(FailureMessages.wasLessThanOrEqualTo(prettifier, leftSideValue, comparison.right))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 8 shouldBe &gt;= (7)
     *   ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(comparison: ResultOfGreaterThanOrEqualToComparison[T]): Assertion = {
      if (!comparison(leftSideValue)) {
        indicateFailure(
          FailureMessages.wasNotGreaterThanOrEqualTo(prettifier,
            leftSideValue,
            comparison.right
          ),
          None,
          pos
        )
      }
      else indicateSuccess(FailureMessages.wasGreaterThanOrEqualTo(prettifier, leftSideValue, comparison.right))
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">testing
     * 1 shouldBe odd
     *   ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(beMatcher: BeMatcher[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(beMatcher: BeMatcher[T]): Assertion = {
      val result = beMatcher.apply(leftSideValue)
      if (!result.matches)
        indicateFailure(result.failureMessage(prettifier), None, pos)
      else indicateSuccess(result.negatedFailureMessage(prettifier))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe 7.1 +- 0.2
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(spread: Spread[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(spread: Spread[T]): Assertion = {
      if (!spread.isWithin(leftSideValue)) {
        indicateFailure(FailureMessages.wasNotPlusOrMinus(prettifier, leftSideValue, spread.pivot, spread.tolerance), None, pos)
      }
      else indicateSuccess(FailureMessages.wasPlusOrMinus(prettifier, leftSideValue, spread.pivot, spread.tolerance))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe sorted
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(right: SortedWord)(implicit sortable: Sortable[T]): Assertion = {
      if (!sortable.isSorted(leftSideValue))
        indicateFailure(FailureMessages.wasNotSorted(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.wasSorted(prettifier, leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe a [Book]
     *         ^
     * </pre>
     */
    // SKIP-DOTTY-START
    def shouldBe(aType: ResultOfATypeInvocation[_]): Assertion = macro TypeMatcherMacro.shouldBeATypeImpl
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix inline def shouldBe(aType: ResultOfATypeInvocation[_]): Assertion = ${ org.scalatest.matchers.should.TypeMatcherMacro.shouldBeATypeImpl('{leftSideValue}, '{aType}, '{pos}, '{prettifier}) }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe an [Book]
     *         ^
     * </pre>
     */
    // SKIP-DOTTY-START
    def shouldBe(anType: ResultOfAnTypeInvocation[_]): Assertion = macro TypeMatcherMacro.shouldBeAnTypeImpl
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix inline def shouldBe(anType: ResultOfAnTypeInvocation[_]): Assertion = ${ org.scalatest.matchers.should.TypeMatcherMacro.shouldBeAnTypeImpl('{leftSideValue}, '{anType}, '{pos}, '{prettifier}) }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe readable
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: ReadableWord)(implicit readability: Readability[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(right: ReadableWord)(implicit readability: Readability[T]): Assertion = {
      if (!readability.isReadable(leftSideValue))
        indicateFailure(FailureMessages.wasNotReadable(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.wasReadable(prettifier, leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe writable
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: WritableWord)(implicit writability: Writability[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(right: WritableWord)(implicit writability: Writability[T]): Assertion = {
      if (!writability.isWritable(leftSideValue))
        indicateFailure(FailureMessages.wasNotWritable(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.wasWritable(prettifier, leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe empty
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(right: EmptyWord)(implicit emptiness: Emptiness[T]): Assertion = {
      if (!emptiness.isEmpty(leftSideValue))
        indicateFailure(FailureMessages.wasNotEmpty(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.wasEmpty(prettifier, leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe defined
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: DefinedWord)(implicit definition: Definition[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(right: DefinedWord)(implicit definition: Definition[T]): Assertion = {
      if (!definition.isDefined(leftSideValue))
        indicateFailure(FailureMessages.wasNotDefined(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.wasDefined(prettifier, leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldNot be (3)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(beWord: BeWord): ResultOfBeWordForAny[T] = 
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(beWord: BeWord): ResultOfBeWordForAny[T] = 
      new ResultOfBeWordForAny(leftSideValue, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldNot (be (3))
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(rightMatcherX1: Matcher[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(rightMatcherX1: Matcher[T]): Assertion = {
      ShouldMethodHelper.shouldNotMatcher(leftSideValue, rightMatcherX1, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldNot (be readable)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T, TYPECLASS1[_]](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]): Assertion = {
      ShouldMethodHelper.shouldNotMatcher(leftSideValue, rightMatcherFactory1.matcher, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldNot have length (3)
     *        ^
     * result shouldNot have size (3)
     *        ^
     * exception shouldNot have message ("file not found")
     *           ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(haveWord: HaveWord): ResultOfHaveWordForExtent[T] =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(haveWord: HaveWord): ResultOfHaveWordForExtent[T] =
      new ResultOfHaveWordForExtent(leftSideValue, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should have length (3)
     *        ^
     * result should have size (3)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(haveWord: HaveWord): ResultOfHaveWordForExtent[T] =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(haveWord: HaveWord): ResultOfHaveWordForExtent[T] =
      new ResultOfHaveWordForExtent(leftSideValue, true, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe null
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(right: Null)(implicit ev: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(right: Null)(implicit ev: T <:< AnyRef): Assertion = {
      if (leftSideValue != null) {
        indicateFailure(FailureMessages.wasNotNull(prettifier, leftSideValue), None, pos)
      }
      else indicateSuccess(FailureMessages.wasNull)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe theSameInstanceAs (anotherObject)
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
      if (resultOfSameInstanceAsApplication.right ne toAnyRef(leftSideValue)) {
        indicateFailure(
          FailureMessages.wasNotSameInstanceAs(prettifier,
            leftSideValue,
            resultOfSameInstanceAsApplication.right
          ),
          None,
          pos
        )
      }
      else indicateSuccess(FailureMessages.wasSameInstanceAs(prettifier, leftSideValue, resultOfSameInstanceAsApplication.right))
    }

    // SKIP-SCALATESTJS,NATIVE-START
// TODO: Remember to write tests for inspector shorthands uncovering the bug below, always a empty because always true true passed to matchSym
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * list shouldBe 'empty
     *      ^
     * </pre>
     */
    // SKIP-SCALATESTJS,NATIVE-END 
    // SKIP-SCALATESTJS,NATIVE-START
    // SKIP-DOTTY-START 
    def shouldBe(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END
    // SKIP-SCALATESTJS,NATIVE-END
    // SKIP-SCALATESTJS,NATIVE-START
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef): Assertion = {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(leftSideValue), symbol, false, true, prettifier, pos)
      if (!matcherResult.matches)
        indicateFailure(matcherResult.failureMessage(prettifier), None, pos)
      else indicateSuccess(matcherResult.negatedFailureMessage(prettifier))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * list shouldBe a ('empty)
     *      ^
     * </pre>
     */
    // SKIP-SCALATESTJS,NATIVE-END
    // SKIP-SCALATESTJS,NATIVE-START
    // SKIP-DOTTY-START 
    def shouldBe(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END
    // SKIP-SCALATESTJS,NATIVE-END
    // SKIP-SCALATESTJS,NATIVE-START
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(leftSideValue), resultOfAWordApplication.symbol, true, true, prettifier, pos)
      if (!matcherResult.matches) {
        indicateFailure(
          matcherResult.failureMessage(prettifier),
          None,
          pos
        )
      }
      else indicateSuccess(matcherResult.negatedFailureMessage(prettifier))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * list shouldBe an ('empty)
     *      ^
     * </pre>
     */
    // SKIP-SCALATESTJS,NATIVE-END
    // SKIP-SCALATESTJS,NATIVE-START
    // SKIP-DOTTY-START 
    def shouldBe(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
    // SKIP-DOTTY-END
    // SKIP-SCALATESTJS,NATIVE-END
    // SKIP-SCALATESTJS,NATIVE-START
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef): Assertion = {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(leftSideValue), resultOfAnWordApplication.symbol, true, false, prettifier, pos)
      if (!matcherResult.matches) {
        indicateFailure(
          matcherResult.failureMessage(prettifier),
          None,
          pos
        )
      }
      else indicateSuccess(matcherResult.negatedFailureMessage(prettifier))
    }
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax, where <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * programmingInScala shouldBe excellentRead
     *                    ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = { // TODO: Try expanding this to 2.10 AnyVal
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef): Assertion = {
      val result = bePropertyMatcher(leftSideValue)
      if (!result.matches)
        indicateFailure(FailureMessages.wasNot(prettifier, leftSideValue, UnquotedString(result.propertyName)), None, pos)
      else indicateSuccess(FailureMessages.was(prettifier, leftSideValue, UnquotedString(result.propertyName)))
    }

    /**
     * This method enables the following syntax, where <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * programmingInScala shouldBe a (goodRead)
     *                    ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {// TODO: Try expanding this to 2.10 AnyVal
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T, U >: T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {
      val result = resultOfAWordApplication.bePropertyMatcher(leftSideValue)
      if (!result.matches) {
        indicateFailure(FailureMessages.wasNotA(prettifier, leftSideValue, UnquotedString(result.propertyName)), None, pos)
      }
      else indicateSuccess(FailureMessages.wasA(prettifier, leftSideValue, UnquotedString(result.propertyName)))
    }

    /**
     * This method enables the following syntax, where <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * programmingInScala shouldBe an (excellentRead)
     *                    ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldBe[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {// TODO: Try expanding this to 2.10 AnyVal
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T, U >: T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldBe(resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef): Assertion = {
      val result = resultOfAnWordApplication.bePropertyMatcher(leftSideValue)
      if (!result.matches) {
        indicateFailure(FailureMessages.wasNotAn(prettifier, leftSideValue, UnquotedString(result.propertyName)), None, pos)
      }
      else indicateSuccess(FailureMessages.wasAn(prettifier, leftSideValue, UnquotedString(result.propertyName)))
    }

/*
    def shouldBe[U](right: AType[U]) {
      if (!right.isAssignableFromClassOf(leftSideValue)) {
        throw newTestFailedException(FailureMessages.wasNotAnInstanceOf(prettifier, leftSideValue, UnquotedString(right.className), UnquotedString(leftSideValue.getClass.getName)))
      }
    }
*/

   /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * xs should contain oneOf (1, 2, 3)
     *    ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(containWord: ContainWord): ResultOfContainWord[T] = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(containWord: ContainWord): ResultOfContainWord[T] = {
      new ResultOfContainWord(leftSideValue, true, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * xs shouldNot contain (oneOf (1, 2, 3))
     *    ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(contain: ContainWord): ResultOfContainWord[T] =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(contain: ContainWord): ResultOfContainWord[T] =
      new ResultOfContainWord(leftSideValue, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * file should exist
     *      ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
      if (!existence.exists(leftSideValue))
        indicateFailure(FailureMessages.doesNotExist(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.exists(prettifier, leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * file should not (exist)
     *      ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(notExist: ResultOfNotExist)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(notExist: ResultOfNotExist)(implicit existence: Existence[T]): Assertion = {
      if (existence.exists(leftSideValue))
        indicateFailure(FailureMessages.exists(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.doesNotExist(prettifier, leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * file shouldNot exist
     *      ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(existWord: ExistWord)(implicit existence: Existence[T]): Assertion = {
      if (existence.exists(leftSideValue))
        indicateFailure(FailureMessages.exists(prettifier, leftSideValue), None, pos)
      else indicateSuccess(FailureMessages.doesNotExist(prettifier, leftSideValue))
    }

    // From StringShouldWrapper
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should include regex ("hi")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForString = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForString = {
      new ResultOfIncludeWordForString(leftSideValue, true, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should startWith regex ("hello")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForString = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForString = {
      new ResultOfStartWithWordForString(leftSideValue, true, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should endWith regex ("world")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForString = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def should(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForString = {
      new ResultOfEndWithWordForString(leftSideValue, true, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot startWith regex ("hello")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForString =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForString =
      new ResultOfStartWithWordForString(leftSideValue, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot endWith regex ("world")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForString =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForString =
      new ResultOfEndWithWordForString(leftSideValue, false, prettifier, pos)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot include regex ("hi")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForString =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension [T](leftSideValue: T)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForString =
      new ResultOfIncludeWordForString(leftSideValue, false, prettifier, pos)
  // SKIP-DOTTY-START
  }
  // SKIP-DOTTY-END

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on <code>String</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  // SKIP-DOTTY-START 
  final class StringShouldWrapper(val leftSideString: String, pos: source.Position, prettifier: Prettifier) extends AnyShouldWrapper(leftSideString, pos, prettifier) with StringShouldWrapperForVerb {
  // SKIP-DOTTY-END
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("a(b*)c" withGroup "bb")
     *                                          ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def withGroup(group: String): RegexWithGroups =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (leftSideString: String)(using pos: source.Position, prettifier: Prettifier) infix def withGroup(group: String): RegexWithGroups =
      new RegexWithGroups(leftSideString.r, IndexedSeq(group))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
     *                                             ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def withGroups(groups: String*): RegexWithGroups =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (leftSideString: String)(using pos: source.Position, prettifier: Prettifier) infix def withGroups(groups: String*): RegexWithGroups =
      new RegexWithGroups(leftSideString.r, IndexedSeq(groups: _*))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = {
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (leftSideString: String)(using pos: source.Position, prettifier: Prettifier) infix def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = {
      new ResultOfFullyMatchWordForString(leftSideString, true, prettifier, pos)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def shouldNot(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (leftSideString: String)(using pos: source.Position, prettifier: Prettifier) infix def shouldNot(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString =
      new ResultOfFullyMatchWordForString(leftSideString, false, prettifier, pos)

    //DOTTY-ONLY import scala.compiletime.testing.{typeChecks,typeCheckErrors}

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should compile
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START
    def should(compileWord: CompileWord)(implicit pos: source.Position): Assertion = macro CompileMacro.shouldCompileImpl
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (inline leftSideString: String)(using pos: source.Position, prettifier: Prettifier) infix transparent inline def should(compileWord: CompileWord): Assertion = ${ org.scalatest.matchers.should.CompileMacro.shouldCompileImpl('{leftSideString}, '{typeCheckErrors(leftSideString)}, '{compileWord})('{pos}) }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot compile
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START
    def shouldNot(compileWord: CompileWord)(implicit pos: source.Position): Assertion = macro CompileMacro.shouldNotCompileImpl
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (inline leftSideString: String)(using pos: source.Position, prettifier: Prettifier) infix transparent inline def shouldNot(compileWord: CompileWord): Assertion = ${ org.scalatest.matchers.should.CompileMacro.shouldNotCompileImpl('{leftSideString}, '{typeChecks(leftSideString)}, '{compileWord})('{pos}) }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot typeCheck
     *        ^
     * </pre>
     */
    // SKIP-DOTTY-START
    def shouldNot(typeCheckWord: TypeCheckWord)(implicit pos: source.Position): Assertion = macro CompileMacro.shouldNotTypeCheckImpl
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (inline leftSideString: String)(using pos: source.Position, prettifier: Prettifier) infix transparent inline def shouldNot(typeCheckWord: TypeCheckWord): Assertion = ${ org.scalatest.matchers.should.CompileMacro.shouldNotTypeCheckImpl('{leftSideString}, '{typeCheckErrors(leftSideString)}, '{typeCheckWord})('{pos}) }
  // SKIP-DOTTY-START
  }
  // SKIP-DOTTY-END

  // SKIP-DOTTY-START
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>withGroup</code> and <code>withGroups</code> methods to
   * be invoked on <code>Regex</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  final class RegexWrapper(regex: Regex) {
  // SKIP-DOTTY-END  

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * regex should fullyMatch regex ("a(b*)c" withGroup "bb")
     *                                         ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def withGroup(group: String): RegexWithGroups =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (regex: Regex) infix def withGroup(group: String): RegexWithGroups =
      new RegexWithGroups(regex, IndexedSeq(group))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * regex should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
     *                                            ^
     * </pre>
     */
    // SKIP-DOTTY-START 
    def withGroups(groups: String*): RegexWithGroups =
    // SKIP-DOTTY-END
    //DOTTY-ONLY extension (regex: Regex) infix def withGroups(groups: String*): RegexWithGroups =
      new RegexWithGroups(regex, IndexedSeq(groups: _*))
  // SKIP-DOTTY-START
  }
  // SKIP-DOTTY-END

  // SKIP-DOTTY-START 
  /**
   * Implicitly converts an object of type <code>T</code> to a <code>AnyShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToAnyShouldWrapper[T](o: T)(implicit pos: source.Position, prettifier: Prettifier): AnyShouldWrapper[T] = new AnyShouldWrapper(o, pos, prettifier)

  /**
   * Implicitly converts an object of type <code>java.lang.String</code> to a <code>StringShouldWrapper</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToStringShouldWrapper(o: String)(implicit pos: source.Position, prettifier: Prettifier): StringShouldWrapper = new StringShouldWrapper(o, pos, prettifier)

  /**
   * Implicitly converts an object of type <code>scala.util.matching.Regex</code> to a <code>RegexWrapper</code>,
   * to enable <code>withGroup</code> and <code>withGroups</code> methods to be invokable on that object.
   */
  implicit def convertToRegexWrapper(o: Regex): RegexWrapper = new RegexWrapper(o)
  // SKIP-DOTTY-END

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * book should have (message ("A TALE OF TWO CITIES") (of [Book]), title ("A Tale of Two Cities"))
   *                                                     ^
   * </pre>
   */ 
  def of[T](implicit ev: ClassTag[T]): ResultOfOfTypeInvocation[T] = 
    new ResultOfOfTypeInvocation[T]
}

/**
 * Companion object that facilitates the importing of <code>Matchers</code> members as
 * an alternative to mixing it the trait. One use case is to import <code>Matchers</code> members so you can use
 * them in the Scala interpreter.
 *
 * @author Bill Venners
 */
object Matchers extends Matchers
