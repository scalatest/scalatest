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

import org.scalatest.matchers._
import org.scalatest.enablers._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.reflect.Manifest
import MatchersHelper.transformOperatorChars
import scala.collection.Traversable
import Assertions.areEqualComparingArraysStructurally
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import org.scalactic.Tolerance
import org.scalactic.Explicitly
import org.scalactic.TripleEqualsSupport.Spread
import org.scalactic.TripleEqualsSupport.TripleEqualsInvocation
import org.scalactic.Equality
import org.scalactic.TripleEqualsSupport.TripleEqualsInvocationOnSpread
import org.scalactic.Constraint
import org.scalactic.Prettifier
import org.scalactic.Every
import MatchersHelper.andMatchersAndApply
import MatchersHelper.orMatchersAndApply
import org.scalatest.words._
import MatchersHelper.matchSymbolToPredicateMethod
import MatchersHelper.accessProperty
import MatchersHelper.newTestFailedException
import MatchersHelper.fullyMatchRegexWithGroups
import MatchersHelper.startWithRegexWithGroups
import MatchersHelper.endWithRegexWithGroups
import MatchersHelper.includeRegexWithGroups
import org.scalactic.NormalizingEquality
import Assertions.checkExpectedException
import Assertions.checkNoException
import exceptions.StackDepthExceptionHelper.getStackDepthFun
import scala.language.experimental.macros
import scala.language.higherKinds

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
 * in the unit value, <code>()</code>). Otherwise, a <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a>
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
 * Trait <a href="MustMatchers.html"><code>MustMatchers</code></a> is an alternative to <code>Matchers</code> that provides the exact same
 * meaning, syntax, and behavior as <code>Matchers</code>, but uses the verb <code>must</code> instead of <!-- PRESERVE --><code>should</code>.
 * The two traits differ only in the English semantics of the verb: <!-- PRESERVE --><code>should</code>
 * is informal, making the code feel like conversation between the writer and the reader; <code>must</code> is more formal, making the code feel more like 
 * a written specification.
 * </p>
 *
 * <a name="matchersMigration"></a>
 * <h2>Matchers migration in ScalaTest 2.0 and 2.1.0</h2>
 *
 * <h3>Deprecations</h3>
 *
 * <p>
 * Prior to 2.0, ScalaTest's matchers DSL was provided by traits
 * <code>org.scalatest.matchers.ShouldMatchers</code> and
 * <code>org.scalatest.matchers.MustMatchers</code>. These are now deprecated in favor of
 * traits in package <code>org.scalatest</code>. The fully qualified name of the original
 * <code>ShouldMatchers</code> is now <code>org.scalatest.Matchers</code>, and the fully qualified
 * name of the original <code>MustMatchers</code> is now <code>org.scalatest.MustMatchers</code>.
 * The old fully qualified names will continue to work during a lengthy deprecation cycle, but
 * will generate a deprecation warning and eventually be removed in a future version
 * of ScalaTest. You can migrate existing uses of <code>ShouldMatchers</code> by simply importing
 * or mixing in <code>org.scalatest.Matchers</code> instead of
 * <code>org.scalatest.matchers.ShouldMatchers</code>, and can migrate existing
 * uses of <code>org.scalatest.matchers.MustMatchers</code> by importing or
 * mixing in <code>org.scalatest.MustMatchers</code> instead of <code>org.scalatest.matchers.MustMatchers</code>.
 * </p>
 *
 * <p>
 * Two other deprecations in ScalaTest 2.0 matchers are <code>be</code> <code>===</code> <code>&lt;value&gt;</code> and <code>evaluating</code> <code>...</code>
 * <code>should</code> <code>produce</code> syntax. This will both continue to work as before, but will generate a deprecation
 * warning and eventually be removed in a future version of ScalaTest. the <code>be</code> <code>===</code> syntax is being deprecated so that all uses
 * of <code>===</code> in ScalaTest consistently provide the new
 * features of tunable type checking, tolerance support, and customized equality.  Please replace uses of this syntax with one of the other
 * ways to check equality described in the next section. The <code>eventually</code> syntax is being deprecated because it is replaced by <code>thrownBy</code>
 * clauses, as <a href="#expectedExceptions">described below</a>.
 * </p>
 *
 * <h3>Potential breakages</h3>
 * 
 * <p>
 * Although ScalaTest's matchers have undergone a major refactor in 2.0, all previously documented syntax for matchers should continue to work exactly
 * the same with one potential exception, which should in practice be extremely rare. The potential breakage is that if you included <code>length</code> or <code>size</code>
 * along with custom have-property matchers that you wrote, you'll get a compiler error. To fix such an error, add after
 * your <code>length</code> or <code>size</code> invocation an <code>(of [&lt;type&gt;])</code> clause, as
 * <a href="#lengthSizeHavePropertyMatchers">described below</a>.
 * </p>
 *
 * <p>
 * The only other source of potential breakage is the fragile base class problem. We have added fields and methods to <code>Matchers</code> in 2.0 that may
 * conflict with fields and methods in your existing classes and cause a compiler error. Such issues can usually be easily fixed locally with simple renames or refactors,
 * but if you prefer to subtract a token from <code>Matchers</code>, you can do so by mixing together your own <code>Matchers</code> trait
 * from component traits, as <a>described below</a>. Note that you should not see any new implicit conflicts, because we managed to <em>reduce</em> the number
 * of implicits brought into scope by 2.0 matchers compared to 1.x by about 75%.
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
 * <a href="../scalactic/Equality.html"><code>org.scalactic.Equality[L]</code></a> to be provided (either implicitly or explicitly), where
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
 * For an example, see the main documentation of <a href="../scalactic/Equality.html">trait <code>Equality</code></a>.
 * </p>
 *
 * <p>
 * You can always supply implicit parameters explicitly, but in the case of implicit parameters of type <code>Equality[T]</code>, Scalactic provides a
 * simple "explictly" DSL. For example, here's how you could explicitly supply an <code>Equality[String]</code> instance that normalizes both left and right
 * sides (which must be strings), by transforming them to lowercase:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
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
 * for trait <a href="../scalactic/Explicitly.html"><code>Explicitly</code></a>.
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
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
 *
 * scala&gt; import org.scalactic.TypeCheckedTripleEquals._
 * import org.scalactic.TypeCheckedTripleEquals._
 *
 * scala&gt; Some(2) should === (2)
 * &lt;console&gt;:17: error: types Some[Int] and Int do not adhere to the equality constraint
 * selected for the === and !== operators; the missing implicit parameter is of
 * type org.scalactic.Constraint[Some[Int],Int]
 *               Some(2) should === (2)
 *                       ^
 * </pre>
 *
 * <p>
 * By default, the "<code>Some(2)</code> <code>should</code> <code>===</code> <code>(2)</code>" statement would fail at runtime. By mixing in
 * the equality constraints provided by <code>TypeCheckedTripleEquals</code>, however, the statement fails to compile. For more information
 * and examples, see the main documentation for <a href="../scalactic/TypeCheckedTripleEquals.html">trait <code>TypeCheckedTripleEquals</code></a>.
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
 * Similarly, the <code>size</code> syntax can be used with <code>Array</code>, any <code>scala.collection.GenTraversable</code>,
 * any <code>java.util.Collection</code>, any <code>java.util.Map</code>, and any type <code>T</code> for which an implicit <code>Size[T]</code> type class is 
 * available in scope. You can enable the <code>length</code> or <code>size</code> syntax for your own arbitrary types, therefore,
 * by defining <a href="enablers/Length.html"><code>Length</code></a> or <a href="enablers/Size.html"><code>Size</code></a> type
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
 * process, see the documentation for <a href="words/BeWord.html"><code>BeWord</code></a>.)
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
 * The <code>Emptiness</code> companion object provides implicits for <code>GenTraversable[E]</code>, <code>java.util.Collection[E]</code>, 
 * <code>java.util.Map[K, V]</code>, <code>String</code>, <code>Array[E]</code>, and <code>Option[E]</code>. In addition, the
 * <code>Emptiness</code> companion object provides structural implicits for types that declare an <code>isEmpty</code> method that
 * returns a <code>Boolean</code>. Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
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
 * companion object, implicits are provided for types <code>GenTraversable[E]</code>, <code>java.util.Collection[E]</code>, 
 * <code>java.util.Map[K, V]</code>, <code>String</code>, <code>Array[E]</code>, and <code>Option[E]</code>. 
 * Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
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
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
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
 * org.scalatest.exceptions.TestFailedException: List(1, 2, 3) did not contain one of (2, 3, 4)
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
 * for types <code>GenTraversable[E]</code>, <code>java.util.Collection[E]</code>, 
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
 * The "<code>contain</code> <code>atMostOneOf</code>" syntax lets you specify a set of objects at most one of which should be contained in the containing object:
 * </p>
 *
 * <pre class="stHighlight">
 * List(1, 2, 3, 4, 5) should contain atMostOneOf (5, 6, 7)
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
 * in that the right hand side is a <code>GenTraversable[_]</code> rather than a varargs of <code>Any</code>. (Note: in a future 2.0 milestone release, possibly
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
 * Althought it seems desireable to provide similar matcher syntax for Scala and Java iterators to that provided for sequences like
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
 * You can use the <a href="Inspectors.html"><code>Inspectors</code></a> syntax with matchers as well as assertions. If you have a multi-dimensional collection, such as a
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
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
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
 * is availabe, which by default includes <code>GenTraversable</code>, 
 * Java <code>Collection</code>, Java <code>Map</code>, <code>Array</code>s, and <code>String</code>s.
 * Here are some examples:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 * 
 * scala&gt; import Matchers._
 * import Matchers._
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
 * the <code>loneElement</code> syntax provided by trait <a href="LoneElement.html"><code>LoneElement</code></a>. For example, if a
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
 * You can invoke <code>loneElement</code> on any type <code>T</code> for which an implicit <a href="enablers/Collecting.html"><code>Collecting[E, T]</code></a>
 * is available, where <code>E</code> is the element type returned by the <code>loneElement</code> invocation. By default, you can use <code>loneElement</code>
 * on <code>GenTraversable</code>, Java <code>Collection</code>, Java <code>Map</code>, <code>Array</code>, and <code>String</code>.
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
 * <a href="Entry.html"><code>org.scalatest.Entry</code></a>. Here's how you use it:
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
 * scala&gt; import Matchers._
 * import Matchers._
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
 * the following expression would <em>not</em> throw a <a href="exceptions/TestFailedException.html"><code>TestFailedException</code></a>:
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
 * If <code>map</code> is <code>null</code>, the test will indeed fail, but with a <code>NullPointerException</code>, not a
 * <code>TestFailedException</code>. Here, the <code>NullPointerException</code> is the visible right-hand side effect. To get a
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
 * ScalaTest matchers has no special support for <code>Option</code>s, but you can 
 * work with them quite easily using syntax shown previously. For example, if you wish to check
 * whether an option is <code>None</code>, you can write any of:
 * </p>
 * 
 * <pre class="stHighlight">
 * option shouldEqual None
 * option shouldBe None
 * option should not be defined
 * option shouldBe empty
 * </pre>
 * 
 * <p>
 * If you wish to check an option is defined, and holds a specific value, you can write either of:
 * </p>
 * 
 * <pre class="stHighlight">
 * option shouldEqual Some("hi")
 * option shouldBe Some("hi")
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
 * If you mix in (or import the members of) <a href="OptionValues.html"><code>OptionValues</code></a>,
 * you can write one statement that indicates you believe an option should be defined and then say something else about its value. Here's an example:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.OptionValues._
 * option.value should be &lt; 7
 * </pre>
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
 * ScalaTest's <a href="Inside.html"><code>Inside</code></a> trait allows you to make assertions after a pattern match.
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
 * <p>
 * Note: the following syntax from ScalaTest 1.x has been deprecated:
 * </p>
 *
 * <pre class="stHighlight">
 * evaluating { s.charAt(-1) } should produce [IndexOutOfBoundsException]
 * </pre>
 *
 * <p>
 * Such uses will continue to work during the deprecation cycle, but support for this syntax will
 * eventually be removed in a future version of ScalaTest. Please change all uses to
 * a corresponding use of the syntax described previously in this section.
 * <p>
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
  final class HavePropertyMatcherGenerator(symbol: Symbol) {

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

              throw newTestFailedException(Resources("propertyNotFound", methodNameToInvoke, expectedValue.toString, methodNameToInvokeWithGet))

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
  implicit def convertSymbolToHavePropertyMatcherGenerator(symbol: Symbol): HavePropertyMatcherGenerator = new HavePropertyMatcherGenerator(symbol)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  class ResultOfBeWordForAny[T](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax (positiveNumber is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * 1 should be a positiveNumber
     *             ^
     * </pre>
     */
    def a(aMatcher: AMatcher[T]) {
      val matcherResult = aMatcher(left)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax (positiveNumber is a <code>AnMatcher</code>):
     *
     * <pre class="stHighlight">
     * 1 should be an oddNumber
     *             ^
     * </pre>
     */
    def an(anMatcher: AnMatcher[T]) {
      val matcherResult = anMatcher(left)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef)(implicit toAnyRef: T <:< AnyRef) {
      if ((toAnyRef(left) eq right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
            left,
            right
          )
        )
    }

    /* *
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be a [String]
     *                  ^
     * </pre>
    def a[EXPECTED : ClassManifest] {
      val clazz = implicitly[ClassManifest[EXPECTED]].erasure.asInstanceOf[Class[EXPECTED]]
      if (clazz.isAssignableFrom(left.getClass)) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAnInstanceOf", left, UnquotedString(clazz.getName), UnquotedString(left.getClass.getName))
          else
            FailureMessages("wasAnInstanceOf")
        )
      }
    }
    */
     

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fileMock should be a ('file)
     *                    ^
     * </pre>
     */
    def a(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef) {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), symbol, true, true)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

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
    def a(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef) { // TODO: Try expanding this to 2.10 AnyVals
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }
    
    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * fruit should be an ('orange)
     *                 ^
     * </pre>
     */
    def an(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef) {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(left), symbol, true, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should be an (excellentRead)
     *                ^
     * </pre>
     */ 
    def an(beTrueMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef) { // TODO: Try expanding this to 2.10 AnyVals
      val beTrueMatchResult = beTrueMatcher(left)
      if (beTrueMatchResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAn", left, UnquotedString(beTrueMatchResult.propertyName))
          else
            FailureMessages("wasAn", left, UnquotedString(beTrueMatchResult.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>fraction</code> is, for example, of type <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * fraction should be definedAt (6)
     *                    ^
     * </pre>
     */
    def definedAt[U](right: U)(implicit ev: T <:< PartialFunction[U, _]) {
      if (left.isDefinedAt(right) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotDefinedAt", left, right)
          else
            FailureMessages("wasDefinedAt", left, right)
        )
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
    def apply(regexString: String): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regexString, IndexedSeq.empty)

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                                     ^
     * </pre>
     */
    def apply(regex: Regex): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regex, IndexedSeq.empty)

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
  final class ResultOfIncludeWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should include regex ("world")
     *                       ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should include regex ("a(b*)c" withGroup "bb")
     *                       ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) {
      val result = includeRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
       throw newTestFailedException(
         if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
       )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should include regex ("wo.ld".r)
     *                       ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.findFirstIn(left).isDefined != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
            left,
            rightRegex
          )
        )
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
  final class ResultOfStartWithWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o")
     *                         ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should startWith regex ("a(b*)c" withGroup "bb")
     *                         ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) {
      val result = startWithRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
       throw newTestFailedException(
         if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
       )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should startWith regex ("Hel*o".r)
     *                         ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.pattern.matcher(left).lookingAt != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
            left,
            rightRegex
          )
        )
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
  final class ResultOfEndWithWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d")
     *                       ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should endWith regex ("a(b*)c" withGroup "bb")
     *                       ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) {
      val result = endWithRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
       throw newTestFailedException(
         if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
       )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should endWith regex ("wor.d".r)
     *                       ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      val allMatches = rightRegex.findAllIn(left)
      if ((allMatches.hasNext && (allMatches.end == left.length)) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
            left,
            rightRegex
          )
        )
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
  final class ResultOfFullyMatchWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should fullMatch regex ("Hel*o world")
     *                         ^
     * </pre>
     */
    def regex(rightRegexString: String) { regex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should fullMatch regex ("a(b*)c" withGroup "bb") 
     *                         ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) {
      val result = fullyMatchRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      if (result.matches != shouldBeTrue)
       throw newTestFailedException(
         if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
       )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should fullymatch regex ("Hel*o world".r)
     *                          ^
     * </pre>
     */
    def regex(rightRegex: Regex) {
      if (rightRegex.pattern.matcher(left).matches != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
            left,
            rightRegex
          )
        )
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
            FailureMessages("didNotEqual", leftee, rightee),
            FailureMessages("equaled", left, right)
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
          Resources("didNotEqualPlusOrMinus"),
          Resources("equaledPlusOrMinus"), 
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
          Resources("didNotEqualNull"),
          Resources("equaledNull"),
          Resources("didNotEqualNull"),
          Resources("midSentenceEqualedNull"), 
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
    def apply(expectedKey: Any): ResultOfKeyWordApplication = new ResultOfKeyWordApplication(expectedKey)

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
    def apply(expectedValue: Any): ResultOfValueWordApplication = new ResultOfValueWordApplication(expectedValue)

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
    def apply(symbol: Symbol): ResultOfAWordToSymbolApplication = new ResultOfAWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>goodRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be a (goodRead)
     *                         ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAWordToBePropertyMatcherApplication[T] = new ResultOfAWordToBePropertyMatcherApplication(beTrueMatcher)
    
    /**
     * This method enables the following syntax, where, <code>positiveNumber</code> is an <code>AMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * result should not be a (positiveNumber)
     *                        ^
     * </pre>
     */
    def apply[T](aMatcher: AMatcher[T]): ResultOfAWordToAMatcherApplication[T] = new ResultOfAWordToAMatcherApplication(aMatcher)

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
    def apply(symbol: Symbol): ResultOfAnWordToSymbolApplication = new ResultOfAnWordToSymbolApplication(symbol)

    /**
     * This method enables the following syntax, where, for example, <code>badBook</code> is of type <code>Book</code> and <code>excellentRead</code>
     * is a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * badBook should not be an (excellentRead)
     *                          ^
     * </pre>
     */
    def apply[T](beTrueMatcher: BePropertyMatcher[T]): ResultOfAnWordToBePropertyMatcherApplication[T] = new ResultOfAnWordToBePropertyMatcherApplication(beTrueMatcher)
    
    /**
     * This method enables the following syntax, where, <code>positiveNumber</code> is an <code>AnMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * result should not be an (positiveNumber)
     *                         ^
     * </pre>
     */
    def apply[T](anMatcher: AnMatcher[T]): ResultOfAnWordToAnMatcherApplication[T] = new ResultOfAnWordToAnMatcherApplication(anMatcher)

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
    def apply(anyRef: AnyRef): ResultOfTheSameInstanceAsApplication = new ResultOfTheSameInstanceAsApplication(anyRef)

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
  final class ResultOfHaveWordForExtent[A](left: A, shouldBeTrue: Boolean) {

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
    def length(expectedLength: Long)(implicit len: Length[A]) {
      val leftLength = len.lengthOf(left)
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue) 
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadLength", left, expectedLength)
        )
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
    def size(expectedSize: Long)(implicit sz: Size[A]) {
      val leftSize = sz.sizeOf(left)
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadSize", left, expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * exception should have message ("file not found")
     *                       ^
     * </pre>
     */
    def message(expectedMessage: String)(implicit messaging: Messaging[A]) {
      val actualMessage = messaging.messageOf(left)
      if ((actualMessage== expectedMessage) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadMessageInsteadOfExpectedMessage", left, actualMessage, expectedMessage)
          else
            FailureMessages("hadExpectedMessage", left, expectedMessage)
        )
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
   * num should (not be >= (10) and not be < (7))
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
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfEvaluatingApplication(val fun: () => Any) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
     *                                ^
     * </pre>
     */
     def should[T](resultOfProduceApplication: ResultOfProduceInvocation[T]): T =  {
       val clazz = resultOfProduceApplication.clazz
       val caught = try {
         fun()
         None
       }
       catch {
         case u: Throwable => {
           if (!clazz.isAssignableFrom(u.getClass)) {
             val s = Resources("wrongException", clazz.getName, u.getClass.getName)
             throw newTestFailedException(s, Some(u))
             // throw new TestFailedException(s, u, 3) 
           }
           else {
             Some(u)
           }
         }
       }
       caught match {
         case None =>
           val message = Resources("exceptionExpected", clazz.getName)
           throw newTestFailedException(message)
           // throw new TestFailedException(message, 3)
         case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase isAssignableFrom succeeded above
       }
     }

    /**
     * Overrides to return pretty toString.
     *
     * @return "evaluating { ... }"
     */
    override def toString: String = "evaluating { ... }"
  }

  /**
   * <strong>The <code>evaluating { ... } should produce [...Exception]</code> syntax has been deprecated and
   * will be removed in a future version of ScalaTest. Please use <code>a/an [...Exception] should be
   * thrownBy { ... }</code> instead.</strong>
   *
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   * ^
   * </pre>
   */
  @deprecated("Please use 'an [Exception] should be thrownBy { ... }' syntax instead")
  def evaluating(fun: => Any): ResultOfEvaluatingApplication =
    new ResultOfEvaluatingApplication(fun _)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfProduceInvocation[T](val clazz: Class[T]) {
    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfProduceInvocation(classOf([className]))"
     */
    override def toString: String = "ResultOfProduceInvocation(classOf[" + clazz.getName + "])"
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   *                                       ^
   * </pre>
   */
  def produce[T : Manifest]: ResultOfProduceInvocation[T] =
    new ResultOfProduceInvocation(manifest.erasure.asInstanceOf[Class[T]])

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (oneOf(1, 2))
   *                               ^
   * </pre>
   */
  def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("oneOfDuplicate"), getStackDepthFun("Matchers.scala", "oneOf"))
    new ResultOfOneOfApplication(xs)
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (atLeastOneOf(1, 2))
   *                               ^
   * </pre>
   */
  def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("atLeastOneOfDuplicate"), getStackDepthFun("Matchers.scala", "atLeastOneOf"))
    new ResultOfAtLeastOneOfApplication(xs)
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (noneOf(1, 2))
   *                               ^
   * </pre>
   */
  def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("noneOfDuplicate"), getStackDepthFun("Matchers.scala", "noneOf"))
    new ResultOfNoneOfApplication(xs)
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (theSameElementsAs(List(1, 2, 3)))
   *                               ^
   * </pre>
   */
  def theSameElementsAs(xs: GenTraversable[_]) = new ResultOfTheSameElementsAsApplication(xs)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (theSameElementsInOrderAs(List(1, 2)))
   *                               ^
   * </pre>
   */
  def theSameElementsInOrderAs(xs: GenTraversable[_]) = new ResultOfTheSameElementsInOrderAsApplication(xs)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (only(1, 2))
   *                               ^
   * </pre>
   */
  def only(xs: Any*) = {
    if (xs.isEmpty)
      throw new NotAllowedException(FailureMessages("onlyEmpty"), getStackDepthFun("Matchers.scala", "only"))
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("onlyDuplicate"), getStackDepthFun("Matchers.scala", "only"))
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
  def inOrderOnly[T](firstEle: Any, secondEle: Any, remainingEles: Any*) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("inOrderOnlyDuplicate"), getStackDepthFun("Matchers.scala", "inOrderOnly"))
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
  def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("allOfDuplicate"), getStackDepthFun("Matchers.scala", "allOf"))
    new ResultOfAllOfApplication(xs)
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrder(1, 2))
   *                               ^
   * </pre>
   */
  def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("inOrderDuplicate"), getStackDepthFun("Matchers.scala", "inOrder"))
    new ResultOfInOrderApplication(xs)
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (atMostOneOf(1, 2))
   *                               ^
   * </pre>
   */
  def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*) = {
    val xs = firstEle :: secondEle :: remainingEles.toList
    if (xs.distinct.size != xs.size)
      throw new NotAllowedException(FailureMessages("atMostOneOfDuplicate"), getStackDepthFun("Matchers.scala", "atMostOneOf"))
    new ResultOfAtMostOneOfApplication(xs)
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy {...}
   *                                ^
   * </pre>
   */
  def thrownBy(fun: => Any) = new ResultOfThrownByApplication(fun)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * exception should not have message ("file not found")
   *                           ^
   * </pre>
   */
  def message(expectedMessage: String) = new ResultOfMessageWordApplication(expectedMessage)
  
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

  private sealed trait Collected
  private case object AllCollected extends Collected
  private case object EveryCollected extends Collected
  private case class BetweenCollected(from: Int, to: Int) extends Collected
  private case class AtLeastCollected(num: Int) extends Collected
  private case class AtMostCollected(num: Int) extends Collected
  private case object NoCollected extends Collected
  private case class ExactlyCollected(num: Int) extends Collected
  
  import InspectorsHelper._
  
  private[scalatest] def doCollected[T](collected: Collected, xs: scala.collection.GenTraversable[T], original: Any, methodName: String, stackDepth: Int)(fun: T => Unit) {
    collected match {
      case AllCollected =>
        doForAll(xs, original, "allShorthandFailed", "Matchers.scala", methodName, stackDepth) { e => 
          fun(e)
        }
      case AtLeastCollected(num) => 
        doForAtLeast(num, xs, original, "atLeastShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case EveryCollected => 
        doForEvery(xs, original, "everyShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case ExactlyCollected(num) => 
        doForExactly(num, xs, original, "exactlyShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case NoCollected =>
        doForNo(xs, original, "noShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case BetweenCollected(from, to) =>
        doForBetween(from, to, xs, original, "betweenShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case AtMostCollected(num) =>
        doForAtMost(num, xs, original, "atMostShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
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
  sealed class ResultOfNotWordForCollectedAny[T](collected: Collected, xs: scala.collection.GenTraversable[T], original: Any, shouldBeTrue: Boolean) {

    import org.scalatest.InspectorsHelper._
 
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (7)
     *                    ^
     * </pre>
     */
    def equal(right: Any)(implicit equality: Equality[T]) {
      doCollected(collected, xs, original, "equal", 1) { e =>
        if ((equality.areEqual(e, right)) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEqual" else "equaled",
              e,
              right
            ), 
            None, 
            6
          )
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
    def be(right: Any) {
      doCollected(collected, xs, original, "be", 1) { e =>
        if ((e == right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
              e,
              right
            ), 
            None, 
            6
          )
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
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotLessThanOrEqualTo" else "wasLessThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            6
          )
        }
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
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotGreaterThanOrEqualTo" else "wasGreaterThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            6
          )
        }
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
    def be(comparison: ResultOfLessThanComparison[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotLessThan" else "wasLessThan",
              e,
              comparison.right
            ), 
            None, 
            6
          ) 
        }
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
    def be(comparison: ResultOfGreaterThanComparison[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (comparison(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotGreaterThan" else "wasGreaterThan",
              e,
              comparison.right
            ), 
            None, 
            6
          )
        }
      }
    }

    /**
     * <strong>
     * The should be === syntax has been deprecated and will be removed in a future version of ScalaTest. Please use should equal, should ===, shouldEqual,
     * should be, or shouldBe instead. Note, the reason this was deprecated was so that === would mean only one thing in ScalaTest: a customizable, type-
     * checkable equality comparison.
     * </strong>
     *
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be === (7)
     *                    ^
     * </pre>
     */
    @deprecated("The should be === syntax has been deprecated. Please use should equal, should ===, shouldEqual, should be, or shouldBe instead.")
    def be(comparison: TripleEqualsInvocation[_]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if ((e == comparison.right) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
              e,
              comparison.right
            ), 
            None, 
            6
          )
        }
      }
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
    def be(beMatcher: BeMatcher[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        val result = beMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              result.failureMessage
            else
              result.negatedFailureMessage, 
            None, 
            10
          )
        }
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
    def be(bePropertyMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        val result = bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNot", e, UnquotedString(result.propertyName))
            else
              FailureMessages("was", e, UnquotedString(result.propertyName)), 
            None, 
            6
          )
        }
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
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        val result = resultOfAWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotA", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasA", e, UnquotedString(result.propertyName)), 
            None, 
            6
          )
        }
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
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        val result = resultOfAnWordApplication.bePropertyMatcher(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotAn", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasAn", e, UnquotedString(result.propertyName)), 
            None, 
            6
          )
        }
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
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication) {
      doCollected(collected, xs, original, "be", 1) { e => 
        e match {
          case ref: AnyRef =>
            if ((resultOfSameInstanceAsApplication.right eq ref) != shouldBeTrue) {
              throw newTestFailedException(
                FailureMessages(
                  if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
                  e,
                  resultOfSameInstanceAsApplication.right
                ), 
                None, 
                6
              )
            }
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
    def be[U](resultOfDefinedAt: ResultOfDefinedAt[U])(implicit ev: T <:< PartialFunction[U, _]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (e.isDefinedAt(resultOfDefinedAt.right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotDefinedAt" else "wasDefinedAt", 
              e, 
              resultOfDefinedAt.right
            ), 
            None, 
            6  
          )
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
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit len: Length[T]) {
      doCollected(collected, xs, original, "have", 1) { e => 
        val right = resultOfLengthWordApplication.expectedLength
        val leftLength = len.lengthOf(e)
        if ((leftLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, leftLength, right)
            else
              FailureMessages("hadLength", e, right), 
            None, 
            6
          )
        }
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
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication)(implicit sz: Size[T]) {
      doCollected(collected, xs, original, "have", 1) { e => 
        val right = resultOfSizeWordApplication.expectedSize
        val leftSize = sz.sizeOf(e)
        if ((leftSize == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, leftSize, right)
            else
              FailureMessages("hadSize", e, right),
            None, 
            6
          )
        }
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
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {
      doCollected(collected, xs, original, "have", 1) { e => 
      
        val results =
          for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
            propertyVerifier(e)

        val firstFailureOption = results.find(pv => !pv.matches)

        val justOneProperty = propertyMatchers.length == 0

        // if shouldBeTrue is false, then it is like "not have ()", and should throw TFE if firstFailureOption.isDefined is false
        // if shouldBeTrue is true, then it is like "not (not have ()), which should behave like have ()", and should throw TFE if firstFailureOption.isDefined is true
        if (firstFailureOption.isDefined == shouldBeTrue) {
          firstFailureOption match {
            case Some(firstFailure) =>
              // This is one of these cases, thus will only get here if shouldBeTrue is true
              // 0 0 | 0 | 1
              // 0 1 | 0 | 1
              // 1 0 | 0 | 1
              throw newTestFailedException(
                FailureMessages(
                  "propertyDidNotHaveExpectedValue",
                  UnquotedString(firstFailure.propertyName),
                  firstFailure.expectedValue,
                  firstFailure.actualValue,
                  e
                ), 
                None, 
                6
              )
            case None =>
              // This is this cases, thus will only get here if shouldBeTrue is false
              // 1 1 | 1 | 0
              val failureMessage =
                if (justOneProperty) {
                  val firstPropertyResult = results.head // know this will succeed, because firstPropertyMatcher was required
                  FailureMessages(
                    "propertyHadExpectedValue",
                    UnquotedString(firstPropertyResult.propertyName),
                    firstPropertyResult.expectedValue,
                    e
                  )
                }
                else FailureMessages("allPropertiesHadExpectedValues", e)

              throw newTestFailedException(failureMessage, None, 6)
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
    def be(o: Null)(implicit ev: T <:< AnyRef) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if ((e == null) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotNull", e) 
            else
              FailureMessages("wasNull"), 
            None, 
            6
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be ('empty)
     *                    ^
     * </pre>
     */
    def be(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, false, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            6
          )
        }
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
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAWordApplication.symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            6
          )
        }
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
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAnWordApplication.symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
              None, 
              6
            )
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be sorted
     *                    ^
     * </pre>
     */
    def be(sortedWord: SortedWord)(implicit sortable: Sortable[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (sortable.isSorted(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(if (shouldBeTrue) "wasNotSorted" else "wasSorted", e), 
            None, 
            6
          )
        }
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
    def be(readableWord: ReadableWord)(implicit readability: Readability[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (readability.isReadable(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(if (shouldBeTrue) "wasNotReadable" else "wasReadable", e), 
            None, 
            6
          )
        }
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
    def be(writableWord: WritableWord)(implicit writability: Writability[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (writability.isWritable(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(if (shouldBeTrue) "wasNotWritable" else "wasWritable", e), 
            None, 
            6
          )
        }
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
    def be(emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (emptiness.isEmpty(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(if (shouldBeTrue) "wasNotEmpty" else "wasEmpty", e), 
            None, 
            6
          )
        }
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
    def be(definedWord: DefinedWord)(implicit definition: Definition[T]) {
      doCollected(collected, xs, original, "be", 1) { e => 
        if (definition.isDefined(e) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(if (shouldBeTrue) "wasNotDefined" else "wasDefined", e), 
            None, 
            6
          )
        }
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
    def contain(expectedElement: Any)(implicit containing: Containing[T]) {
      doCollected(collected, xs, original, "contain", 1) { e =>
        val right = expectedElement
        if ((containing.contains(e, right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              e,
              right
            ), 
            None, 
            6
          )
        }
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
    def contain(oneOf: ResultOfOneOfApplication)(implicit containing: Containing[T]) {

      val right = oneOf.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (containing.containsOneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainOneOfElements" else "containedOneOfElements",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
          )
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
    def contain(atLeastOneOf: ResultOfAtLeastOneOfApplication)(implicit aggregating: Aggregating[T]) {

      val right = atLeastOneOf.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (aggregating.containsAtLeastOneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAtLeastOneOf" else "containedAtLeastOneOf",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
          )
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
    def contain(noneOf: ResultOfNoneOfApplication)(implicit containing: Containing[T]) {

      val right = noneOf.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (containing.containsNoneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "containedAtLeastOneOf" else "didNotContainAtLeastOneOf",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
          )
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
    def contain(theSameElementsAs: ResultOfTheSameElementsAsApplication)(implicit aggregating: Aggregating[T]) {

      val right = theSameElementsAs.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (aggregating.containsTheSameElementsAs(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainSameElements" else "containedSameElements",
              e,
              right
            ),
            None,
            6
          )
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
    def contain(theSameElementsInOrderAs: ResultOfTheSameElementsInOrderAsApplication)(implicit sequencing: Sequencing[T]) {

      val right = theSameElementsInOrderAs.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (sequencing.containsTheSameElementsInOrderAs(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainSameElementsInOrder" else "containedSameElementsInOrder",
              e,
              right
            ),
            None,
            6
          )
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
    def contain(only: ResultOfOnlyApplication)(implicit aggregating: Aggregating[T]) {

      val right = only.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (aggregating.containsOnly(e, right) != shouldBeTrue) {
          val postfix =
            if (right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]]))
              "WithFriendlyReminder"
            else
              ""
          throw newTestFailedException(
            FailureMessages(
              (if (shouldBeTrue) "didNotContainOnlyElements" else "containedOnlyElements") + postfix,
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
          )
        }
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
    def contain(only: ResultOfInOrderOnlyApplication)(implicit sequencing: Sequencing[T]) {

      val right = only.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (sequencing.containsInOrderOnly(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainInOrderOnlyElements" else "containedInOrderOnlyElements",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def contain(only: ResultOfAllOfApplication)(implicit aggregating: Aggregating[T]) {

      val right = only.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (aggregating.containsAllOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAllOfElements" else "containedAllOfElements",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def contain(inOrder: ResultOfInOrderApplication)(implicit sequencing: Sequencing[T]) {

      val right = inOrder.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (sequencing.containsInOrder(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAllOfElementsInOrder" else "containedAllOfElementsInOrder",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def contain(atMostOneOf: ResultOfAtMostOneOfApplication)(implicit aggregating: Aggregating[T]) {

      val right = atMostOneOf.right

      doCollected(collected, xs, original, "contain", 1) { e =>
        if (aggregating.containsAtMostOneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAtMostOneOf" else "containedAtMostOneOf",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication)(implicit keyMapping: KeyMapping[T]) {
      doCollected(collected, xs, original, "contain", 1) { map =>
        val expectedKey = resultOfKeyWordApplication.expectedKey
        if ((keyMapping.containsKey(map, expectedKey)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              map,
              expectedKey
            ), 
            None, 
            6
          )
        }
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
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication)(implicit valueMapping: ValueMapping[T]) {
      doCollected(collected, xs, original, "contain", 1) { map =>
        val expectedValue = resultOfValueWordApplication.expectedValue
        if ((valueMapping.containsValue(map, expectedValue)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              map,
              expectedValue
            ), 
            None, 
            6
          )
        }
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
    def startWith(right: String)(implicit ev: T <:< String) {
      doCollected(collected, xs, original, "startWith", 1) { e =>
        if ((e.indexOf(right) == 0) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWith" else "startedWith",
              e,
              right
            ), 
            None, 
            6
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
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String) {
      doCollected(collected, xs, original, "startWith", 1) { e =>
        val result = startWithRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            6
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
    def endWith(expectedSubstring: String)(implicit ev: T <:< String) {
      doCollected(collected, xs, original, "endWith", 1) { e =>
        if ((e endsWith expectedSubstring) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWith" else "endedWith",
              e,
              expectedSubstring
            ), 
            None, 
            6
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
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String) {
      doCollected(collected, xs, original, "endWith", 1) { e =>
        val result = endWithRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            6
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
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String) {
      doCollected(collected, xs, original, "include", 1) { e =>
        val result = includeRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            6
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
    def include(expectedSubstring: String)(implicit ev: T <:< String) {
      doCollected(collected, xs, original, "include", 1) { e =>
        if ((e.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeSubstring" else "includedSubstring",
              e,
              expectedSubstring
            ), 
            None, 
            6
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
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication)(implicit ev: T <:< String) {
      doCollected(collected, xs, original, "fullyMatch", 1) { e =>
        val result = fullyMatchRegexWithGroups(e, resultOfRegexWordApplication.regex, resultOfRegexWordApplication.groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            6
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
  final class ResultOfContainWordForCollectedAny[T](collected: Collected, xs: scala.collection.GenTraversable[T], original: Any, shouldBeTrue: Boolean) {
  
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * option should contain oneOf (1, 2)
     *                       ^
     * </pre>
     */
    def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[T]) {
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("oneOfDuplicate"), getStackDepthFun("Matchers.scala", "oneOf"))
      doCollected(collected, xs, original, "oneOf", 1) { e =>
        if (containing.containsOneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainOneOfElements" else "containedOneOfElements",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]) {
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("atLeastOneOfDuplicate"), getStackDepthFun("Matchers.scala", "atLeastOneOf"))
      doCollected(collected, xs, original, "atLeastOneOf", 1) { e =>
        if (aggregating.containsAtLeastOneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAtLeastOneOf" else "containedAtLeastOneOf",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit containing: Containing[T]) {
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("noneOfDuplicate"), getStackDepthFun("Matchers.scala", "noneOf"))
      doCollected(collected, xs, original, "noneOf", 1) { e =>
        if (containing.containsNoneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "containedAtLeastOneOf" else "didNotContainAtLeastOneOf",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def theSameElementsAs(right: GenTraversable[_])(implicit aggregating: Aggregating[T]) {
      doCollected(collected, xs, original, "theSameElementsAs", 1) { e =>
        if (aggregating.containsTheSameElementsAs(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainSameElements" else "containedSameElements",
              e,
              right
            ),
            None,
            6
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
    def theSameElementsInOrderAs(right: GenTraversable[_])(implicit sequencing: Sequencing[T]) {
      doCollected(collected, xs, original, "theSameElementsInOrderAs", 1) { e =>
        if (sequencing.containsTheSameElementsInOrderAs(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainSameElementsInOrder" else "containedSameElementsInOrder",
              e,
              right
            ),
            None,
            6
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
    def only(right: Any*)(implicit aggregating: Aggregating[T]) {
      if (right.isEmpty)
        throw new NotAllowedException(FailureMessages("onlyEmpty"), getStackDepthFun("Matchers.scala", "only"))
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("onlyDuplicate"), getStackDepthFun("Matchers.scala", "only"))
      doCollected(collected, xs, original, "only", 1) { e =>
        if (aggregating.containsOnly(e, right) != shouldBeTrue) {
          val postfix =
            if (right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]]))
              "WithFriendlyReminder"
            else
              ""
          throw newTestFailedException(
            FailureMessages(
              (if (shouldBeTrue) "didNotContainOnlyElements" else "containedOnlyElements") + postfix,
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
          )
        }
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
    def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[T]) {
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("inOrderOnlyDuplicate"), getStackDepthFun("Matchers.scala", "inOrderOnly"))
      doCollected(collected, xs, original, "inOrderOnly", 1) { e =>
        if (sequencing.containsInOrderOnly(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainInOrderOnlyElements" else "containedInOrderOnlyElements",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]) {
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("allOfDuplicate"), getStackDepthFun("Matchers.scala", "allOf"))
      doCollected(collected, xs, original, "allOf", 1) { e =>
        if (aggregating.containsAllOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAllOfElements" else "containedAllOfElements",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: Sequencing[T]) {
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("inOrderDuplicate"), getStackDepthFun("Matchers.scala", "inOrder"))
      doCollected(collected, xs, original, "inOrder", 1) { e =>
        if (sequencing.containsInOrder(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAllOfElementsInOrder" else "containedAllOfElementsInOrder",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit aggregating: Aggregating[T]) {
      val right = firstEle :: secondEle :: remainingEles.toList
      if (right.distinct.size != right.size)
        throw new NotAllowedException(FailureMessages("atMostOneOfDuplicate"), getStackDepthFun("Matchers.scala", "atMostOneOf"))
      doCollected(collected, xs, original, "atMostOneOf", 1) { e =>
        if (aggregating.containsAtMostOneOf(e, right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainAtMostOneOf" else "containedAtMostOneOf",
              e,
              UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
            ),
            None,
            6
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
    def key(expectedKey: Any)(implicit keyMapping: KeyMapping[T]) {
      doCollected(collected, xs, original, "key", 1) { map =>
        if (keyMapping.containsKey(map, expectedKey) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              map,
              expectedKey),
              None,
              6
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
    def value(expectedValue: Any)(implicit valueMapping: ValueMapping[T]) {
      doCollected(collected, xs, original, "value", 1) { map =>
        if (valueMapping.containsValue(map, expectedValue) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              map,
              expectedValue),
              None,
              6
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
  sealed class ResultOfBeWordForCollectedAny[T](collected: Collected, xs: scala.collection.GenTraversable[T], original: Any, shouldBeTrue: Boolean) {

    // TODO: Missing should(AMatcher) and should(AnMatcher)

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "theSameInstanceAs", 1) { e =>
        if ((toAnyRef(e) eq right) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
              e,
              right
            ),
            None, 
            6
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be a ('file)
     *                   ^
     * </pre>
     */
    def a(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "a", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            6
          )
        }
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
    def an(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "an", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            6
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be a (goodRead)
     *                      ^
     * </pre>
     */
    def a[U <: T](bePropertyMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef) { // TODO: Try supporting 2.10 AnyVals
      doCollected(collected, xs, original, "a", 1) { e =>
        val result = bePropertyMatcher(e.asInstanceOf[U])
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotA", e, UnquotedString(result.propertyName))
            else
              FailureMessages("wasA", e, UnquotedString(result.propertyName)), 
            None, 
            6
          )
        }
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
    def an[U <: T](beTrueMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef) { // TODO: Try supporting 2.10 AnyVals
      doCollected(collected, xs, original, "an", 1) { e =>
        val beTrueMatchResult = beTrueMatcher(e.asInstanceOf[U])
        if (beTrueMatchResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("wasNotAn", e, UnquotedString(beTrueMatchResult.propertyName))
            else
              FailureMessages("wasAn", e, UnquotedString(beTrueMatchResult.propertyName)), 
            None, 
            6
          )
        }
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
    def definedAt[U](right: U)(implicit ev: T <:< PartialFunction[U, _]) {
      doCollected(collected, xs, xs, "definedAt", 1) { e =>
      if (e.isDefinedAt(right) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotDefinedAt", e, right)
          else
            FailureMessages("wasDefinedAt", e, right), 
          None, 
          6
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
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfBeWordForCollectedArray[T](collected: Collected, xs: scala.collection.GenTraversable[Array[T]], original: Any, shouldBeTrue: Boolean)
    extends ResultOfBeWordForCollectedAny(collected, xs, original, shouldBeTrue) {
  
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
        def apply(left: Array[T]): MatchResult = matchSymbolToPredicateMethod(left.deep, right, false, false)
      }

    /**
     * Overrides to return pretty toString.
     *
     * @return "ResultOfBeWordForCollectedArray([collected], [xs], [shouldBeTrue])"
     */
    override def toString: String = "ResultOfBeWordForCollectedArray(" + Prettifier.default(collected) + ", " + Prettifier.default(xs) + ", " + Prettifier.default(shouldBeTrue) + ")"
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedAny[T](collected: Collected, xs: scala.collection.GenTraversable[T], original: Any) {

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
    def should(rightMatcher: Matcher[T]) {
      doCollected(collected, xs, original, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchFailed(failureMessage) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
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
    def shouldEqual(right: Any)(implicit equality: Equality[T]) {
      doCollected(collected, xs, original, "shouldEqual", 1) { e =>
        if (!equality.areEqual(e, right)) {
          val (eee, rightee) = Suite.getObjectsForFailureMessage(e, right)
          throw newTestFailedException(FailureMessages("didNotEqual", eee, rightee), None, 6)
        }
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual 7.1 +- 0.2
     *        ^
     * </pre>
     */
    def shouldEqual(spread: Spread[T]) {
      doCollected(collected, xs, original, "shouldEqual", 1) { e =>
        if (!spread.isWithin(e)) {
          throw newTestFailedException(FailureMessages("didNotEqualPlusOrMinus", e, spread.pivot, spread.tolerance), None, 6)
        }
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
    def shouldBe(sortedWord: SortedWord)(implicit sortable: Sortable[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!sortable.isSorted(e))
          throw newTestFailedException(FailureMessages("wasNotSorted", e), None, 6)
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
    def shouldBe(readableWord: ReadableWord)(implicit readability: Readability[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!readability.isReadable(e))
          throw newTestFailedException(FailureMessages("wasNotReadable", e), None, 6)
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
    def shouldBe(writableWord: WritableWord)(implicit writability: Writability[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!writability.isWritable(e))
          throw newTestFailedException(FailureMessages("wasNotWritable", e), None, 6)
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
    def shouldBe(emptyWord: EmptyWord)(implicit emptiness: Emptiness[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!emptiness.isEmpty(e))
          throw newTestFailedException(FailureMessages("wasNotEmpty", e), None, 6)
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
    def shouldBe(definedWord: DefinedWord)(implicit definition: Definition[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!definition.isDefined(e))
          throw newTestFailedException(FailureMessages("wasNotDefined", e), None, 6)
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
    def shouldBe(aType: ResultOfATypeInvocation[_]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!aType.clazz.isAssignableFrom(e.getClass))
          throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", e, UnquotedString(aType.clazz.getName), UnquotedString(e.getClass.getName)), None, 6)
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
    def shouldBe(anType: ResultOfAnTypeInvocation[_]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!anType.clazz.isAssignableFrom(e.getClass))
          throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", e, UnquotedString(anType.clazz.getName), UnquotedString(e.getClass.getName)), None, 6)
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
    def shouldEqual(right: Null)(implicit ev: T <:< AnyRef) { 
      doCollected(collected, xs, original, "shouldEqual", 1) { e =>
        if (e != null) {
          throw newTestFailedException(FailureMessages("didNotEqualNull", e), None, 6)
        }
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
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, original, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchFailed(failureMessage) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
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
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, original, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchFailed(failureMessage) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
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
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAny[T](collected, xs, original, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedAny[T] = 
      new ResultOfNotWordForCollectedAny(collected, xs, original, false)

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
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedExtent[T] =
      new ResultOfHaveWordForCollectedExtent(collected, xs, original, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) shouldBe 7
     *          ^
     * </pre>
     */
    def shouldBe(right: Any) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (e != right) {
          val (eee, rightee) = Suite.getObjectsForFailureMessage(e, right)
          throw newTestFailedException(FailureMessages("wasNot", eee, rightee), None, 6)
        }
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
    def shouldBe(comparison: ResultOfLessThanComparison[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!comparison(e)) {
          throw newTestFailedException(
            FailureMessages(
              "wasNotLessThan",
              e,
              comparison.right
            ), 
            None, 
            6
          ) 
        }
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
    def shouldBe(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!comparison(e)) {
          throw newTestFailedException(
            FailureMessages(
              "wasNotLessThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            6
          ) 
        }
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
    def shouldBe(comparison: ResultOfGreaterThanComparison[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!comparison(e)) {
          throw newTestFailedException(
            FailureMessages(
              "wasNotGreaterThan",
              e,
              comparison.right
            ), 
            None, 
            6
          ) 
        }
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
    def shouldBe(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!comparison(e)) {
          throw newTestFailedException(
            FailureMessages(
              "wasNotGreaterThanOrEqualTo",
              e,
              comparison.right
            ), 
            None, 
            6
          ) 
        }
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
    def shouldBe(beMatcher: BeMatcher[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        val result = beMatcher.apply(e)
        if (!result.matches)
          throw newTestFailedException(result.failureMessage, None, 6)
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
    def shouldBe(spread: Spread[T]) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (!spread.isWithin(e))
          throw newTestFailedException(FailureMessages("wasNotPlusOrMinus", e, spread.pivot, spread.tolerance), None, 6)
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
    def shouldBe(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (toAnyRef(e) ne resultOfSameInstanceAsApplication.right)
          throw newTestFailedException(
            FailureMessages(
              "wasNotSameInstanceAs",
              e,
              resultOfSameInstanceAsApplication.right
            ),
            None, 
            6
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe 'empty
     *         ^
     * </pre>
     */
    def shouldBe(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), symbol, false, true, 6)
        if (!matcherResult.matches) 
          throw newTestFailedException(matcherResult.failureMessage, None, 6)
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
    def shouldBe(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAWordApplication.symbol, true, true, 6)
        if (!matcherResult.matches) {
          throw newTestFailedException(matcherResult.failureMessage, None, 6)
        }
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
    def shouldBe(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(toAnyRef(e), resultOfAnWordApplication.symbol, true, false, 6)
        if (!matcherResult.matches) {
          throw newTestFailedException(matcherResult.failureMessage, None, 6)
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) shouldBe null
     *         ^
     * </pre>
     */
    def shouldBe(o: Null)(implicit ev: T <:< AnyRef) {
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        if (e != null)
         throw newTestFailedException(FailureMessages("wasNotNull", e), None, 6) 
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
    def shouldBe[U <: T](bePropertyMatcher: BePropertyMatcher[U])(implicit ev: T <:< AnyRef) { // TODO: Try supporting this with 2.10 AnyVals
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        val result = bePropertyMatcher(e.asInstanceOf[U])
        if (!result.matches) 
          throw newTestFailedException(FailureMessages("wasNot", e, UnquotedString(result.propertyName)), None, 6)
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
    def shouldBe[U <: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef) {// TODO: Try supporting this with 2.10 AnyVals
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        val result = resultOfAWordApplication.bePropertyMatcher(e.asInstanceOf[U])
        if (!result.matches)
          throw newTestFailedException(FailureMessages("wasNotA", e, UnquotedString(result.propertyName)), None, 6)
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
    def shouldBe[U <: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef) {// TODO: Try supporting this with 2.10 AnyVals
      doCollected(collected, xs, original, "shouldBe", 1) { e =>
        val result = resultOfAnWordApplication.bePropertyMatcher(e.asInstanceOf[U])
        if (!result.matches)
          throw newTestFailedException(FailureMessages("wasNotAn", e, UnquotedString(result.propertyName)), None, 6)
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
    def shouldNot[U <: T](rightMatcherX1: Matcher[U]) {
      doCollected(collected, xs, original, "shouldNot", 1) { e =>
        val result = 
          try rightMatcherX1.apply(e.asInstanceOf[U])
          catch {
            case tfe: TestFailedException => 
              throw newTestFailedException(tfe.getMessage, tfe.cause, 6)
          }
        if (result.matches)
          throw newTestFailedException(result.negatedFailureMessage, None, 6)
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
    def shouldNot[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, original, "shouldNot", 1) { e =>
        rightMatcher(e) match {
          case MatchSucceeded(negatedFailureMessage) => 
            throw newTestFailedException(negatedFailureMessage, None, 6)
          case _ => ()
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
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: Constraint[T, U]) {
      doCollected(collected, xs, original, "should", 1) { e =>
        if ((constraint.areEqual(e, inv.right)) != inv.expectingEqual)
          throw newTestFailedException(
            FailureMessages(
             if (inv.expectingEqual) "didNotEqual" else "equaled",
              e,
              inv.right
            ),
            None,
            6
          )
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
    def should(inv: TripleEqualsInvocationOnSpread[T])(implicit ev: Numeric[T]) {
      doCollected(collected, xs, original, "should", 1) { e =>
        if ((inv.spread.isWithin(e)) != inv.expectingEqual)
          throw newTestFailedException(
            FailureMessages(
              if (inv.expectingEqual) "didNotEqualPlusOrMinus" else "equaledPlusOrMinus",
              e,
              inv.spread.pivot,
              inv.spread.tolerance
            ),
            None,
            6
          )
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
    def shouldNot(beWord: BeWord): ResultOfBeWordForCollectedAny[T] =
      new ResultOfBeWordForCollectedAny[T](collected, xs, original, false)

   /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) should contain oneOf (1, 2, 3)
     *          ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedAny[T] = {
      new ResultOfContainWordForCollectedAny(collected, xs, original, true)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all (xs) shouldNot contain (oneOf (1, 2, 3))
     *          ^
     * </pre>
     */
    def shouldNot(containWord: ContainWord): ResultOfContainWordForCollectedAny[T] = {
      new ResultOfContainWordForCollectedAny(collected, xs, original, false)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should exist
     *         ^
     * </pre>
     */
    def should(existWord: ExistWord)(implicit existence: Existence[T]) {
      doCollected(collected, xs, original, "should", 1) { e =>
        if (!existence.exists(e))
          throw newTestFailedException(
            FailureMessages("doesNotExist", e), 
            None, 
            6
          )
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
    def should(notExist: ResultOfNotExist)(implicit existence: Existence[T]) {
      doCollected(collected, xs, original, "should", 1) { e =>
        if (existence.exists(e))
          throw newTestFailedException(
            FailureMessages("exists", e), 
            None, 
            6
          )
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
    def shouldNot(existWord: ExistWord)(implicit existence: Existence[T]) {
      doCollected(collected, xs, original, "shouldNot", 1) { e =>
        if (existence.exists(e))
          throw newTestFailedException(
            FailureMessages("exists", e), 
            None, 
            6
          )
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
    def should(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForCollectedString = 
      new ResultOfStartWithWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForCollectedString = 
      new ResultOfEndWithWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForCollectedString = 
      new ResultOfIncludeWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *             ^
     * </pre>
     */
    def should(fullyMatchWord: FullyMatchWord)(implicit ev: T <:< String): ResultOfFullyMatchWordForCollectedString = 
      new ResultOfFullyMatchWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *             ^
     * </pre>
     */
    def shouldNot(fullyMatchWord: FullyMatchWord)(implicit ev: T <:< String): ResultOfFullyMatchWordForCollectedString = 
      new ResultOfFullyMatchWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot startWith regex ("Hel*o")
     *             ^
     * </pre>
     */
    def shouldNot(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForCollectedString = 
      new ResultOfStartWithWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot endWith regex ("wo.ld")
     *             ^
     * </pre>
     */
    def shouldNot(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForCollectedString = 
      new ResultOfEndWithWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) shouldNot include regex ("wo.ld")
     *             ^
     * </pre>
     */
    def shouldNot(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForCollectedString = 
      new ResultOfIncludeWordForCollectedString(collected, xs.asInstanceOf[GenTraversable[String]], original, false)

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
  final class ResultOfHaveWordForCollectedExtent[A](collected: Collected, xs: scala.collection.GenTraversable[A], original: Any, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all (xs) should have length (12)
     *                      ^
     * </pre>
     */
    def length(expectedLength: Long)(implicit len: Length[A]) {
      doCollected(collected, xs, original, "length", 1) { e =>
        val eLength = len.lengthOf(e)
        if ((eLength == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, expectedLength)
            else
              FailureMessages("hadLength", e, expectedLength), 
            None, 
            6
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
    def size(expectedSize: Long)(implicit sz: Size[A]) {
      doCollected(collected, xs, original, "size", 1) { e =>
        val eSize = sz.sizeOf(e)
        if ((eSize == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, expectedSize)
            else
              FailureMessages("hadSize", e, expectedSize), 
            None, 
            6
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
  final class ResultOfStartWithWordForCollectedString(collected: Collected, xs: scala.collection.GenTraversable[String], original: Any, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *                              ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("a(b*)c" withGroup "bb") 
     *                              ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) { checkRegex(regexWithGroups.regex, regexWithGroups.groups) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o".r)
     *                              ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty) {
      doCollected(collected, xs, original, "regex", 2) { e =>
        val result = startWithRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            7
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
  final class ResultOfIncludeWordForCollectedString(collected: Collected, xs: scala.collection.GenTraversable[String], original: Any, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("world")
     *                            ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("a(b*)c" withGroup "bb") 
     *                            ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) { checkRegex(regexWithGroups.regex, regexWithGroups.groups) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty) {
      doCollected(collected, xs, original, "regex", 2) { e =>
        val result = includeRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            7
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
  final class ResultOfEndWithWordForCollectedString(collected: Collected, xs: scala.collection.GenTraversable[String], original: Any, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d")
     *                            ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("a(b*)c" withGroup "bb") 
     *                            ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) { checkRegex(regexWithGroups.regex, regexWithGroups.groups) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wor.d".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty) {
      doCollected(collected, xs, original, "regex", 2) { e =>
        val result = endWithRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            7
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
  final class ResultOfFullyMatchWordForCollectedString(collected: Collected, xs: scala.collection.GenTraversable[String], original: Any, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("Hel*o world")
     *                              ^
     * </pre>
     */
    def regex(rightRegexString: String) { checkRegex(rightRegexString.r) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullMatch regex ("a(b*)c" withGroup "bb") 
     *                              ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups) { checkRegex(regexWithGroups.regex, regexWithGroups.groups) }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should fullymatch regex ("Hel*o world".r)
     *                               ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex, groups: IndexedSeq[String] = IndexedSeq.empty) {
      doCollected(collected, xs, original, "regex", 2) { e =>
        val result = fullyMatchRegexWithGroups(e, rightRegex, groups)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            7
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
  def all[E, C[_]](xs: C[E])(implicit collecting: Collecting[E, C[E]]): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(AllCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * all(jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def all[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(AllCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * all(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def all(xs: String)(implicit collecting: Collecting[Char, String]): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(AllCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * atLeast(1, xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atLeast[E, C[_]](num: Int, xs: C[E])(implicit collecting: Collecting[E, C[E]]): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(AtLeastCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * atLeast(1, jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atLeast[K, V, JMAP[k, v] <: java.util.Map[k, v]](num: Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(AtLeastCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * atLeast(1, str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atLeast(num: Int, xs: String)(implicit collecting: Collecting[Char, String]): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(AtLeastCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * every(xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def every[E, C[_]](xs: C[E])(implicit collecting: Collecting[E, C[E]]): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(EveryCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * every(jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def every[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(EveryCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * every(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def every(xs: String)(implicit collecting: Collecting[Char, String]): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(EveryCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * exactly(xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def exactly[E, C[_]](num: Int, xs: C[E])(implicit collecting: Collecting[E, C[E]]): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(ExactlyCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * exactly(jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def exactly[K, V, JMAP[k, v] <: java.util.Map[k, v]](num: Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(ExactlyCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * exactly(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def exactly(num: Int, xs: String)(implicit collecting: Collecting[Char, String]): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(ExactlyCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * no(xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def no[E, C[_]](xs: C[E])(implicit collecting: Collecting[E, C[E]]): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(NoCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * no(jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def no[K, V, JMAP[k, v] <: java.util.Map[k, v]](xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(NoCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * no(str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def no(xs: String)(implicit collecting: Collecting[Char, String]): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(NoCollected, collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * between(1, 3, xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def between[E, C[_]](from: Int, upTo:Int, xs: C[E])(implicit collecting: Collecting[E, C[E]]): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * between(1, 3, jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def between[K, V, JMAP[k, v] <: java.util.Map[k, v]](from: Int, upTo:Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * between(1, 3, str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def between(from: Int, upTo:Int, xs: String)(implicit collecting: Collecting[Char, String]): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * atMost(3, xs) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atMost[E, C[_]](num: Int, xs: C[E])(implicit collecting: Collecting[E, C[E]]): ResultOfCollectedAny[E] =
    new ResultOfCollectedAny(AtMostCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>java.util.Map</code>:
   *
   * <pre class="stHighlight">
   * atMost(3, jmap) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atMost[K, V, JMAP[k, v] <: java.util.Map[k, v]](num: Int, xs: JMAP[K, V])(implicit collecting: Collecting[org.scalatest.Entry[K, V], JMAP[K, V]]): ResultOfCollectedAny[org.scalatest.Entry[K, V]] =
    new ResultOfCollectedAny(AtMostCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax for <code>String</code>:
   *
   * <pre class="stHighlight">
   * atMost(3, str) should fullymatch regex ("Hel*o world".r)
   * ^
   * </pre>
   */
  def atMost(num: Int, xs: String)(implicit collecting: Collecting[Char, String]): ResultOfCollectedAny[Char] =
    new ResultOfCollectedAny(AtMostCollected(num), collecting.genTraversableFrom(xs), xs)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy { ... }
   * ^
   * </pre>
   */
  def a[T : Manifest]: ResultOfATypeInvocation[T] = 
    new ResultOfATypeInvocation(manifest.erasure.asInstanceOf[Class[T]])

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [Exception] should be thrownBy { ... }
   * ^
   * </pre>
   */
  def an[T : Manifest]: ResultOfAnTypeInvocation[T] = 
    new ResultOfAnTypeInvocation(manifest.erasure.asInstanceOf[Class[T]])

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * the [FileNotFoundException] should be thrownBy { ... }
   * ^
   * </pre>
   */
  def the[T : Manifest]: ResultOfTheTypeInvocation[T] = 
    new ResultOfTheTypeInvocation(manifest.erasure.asInstanceOf[Class[T]])

  // This is where ShouldMatchers.scala started 

  private object ShouldMethodHelper {
    def shouldMatcher[T](left: T, rightMatcher: Matcher[T], stackDepthAdjustment: Int = 0) {
      rightMatcher(left) match {
        case MatchFailed(failureMessage) => throw newTestFailedException(failureMessage, None, stackDepthAdjustment)
        case _ => ()
      }
    }
    def shouldNotMatcher[T](left: T, rightMatcher: Matcher[T], stackDepthAdjustment: Int = 0) {
      rightMatcher(left) match {
        case MatchSucceeded(negatedFailureMessage) => throw newTestFailedException(negatedFailureMessage, None, stackDepthAdjustment)
        case _ => ()
      }
    }
  }

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
  sealed class AnyShouldWrapper[T](val leftSideValue: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should be (3)
     *        ^
     * </pre>
     */
    def should(rightMatcherX1: Matcher[T]) {
      ShouldMethodHelper.shouldMatcher(leftSideValue, rightMatcherX1)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should equal (3)
     *        ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]) {
      ShouldMethodHelper.shouldMatcher(leftSideValue, rightMatcherFactory1.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should (equal (expected) and have length 3)
     *        ^
     * </pre>
     */
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]) {
      ShouldMethodHelper.shouldMatcher(leftSideValue, rightMatcherFactory2.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * a shouldEqual b
     *   ^
     * </pre>
     */
    def shouldEqual(right: Any)(implicit equality: Equality[T]) {
      if (!equality.areEqual(leftSideValue, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(leftSideValue, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual 7.1 +- 0.2
     *        ^
     * </pre>
     */
    def shouldEqual(spread: Spread[T]) {
      if (!spread.isWithin(leftSideValue)) {
        throw newTestFailedException(FailureMessages("didNotEqualPlusOrMinus", leftSideValue, spread.pivot, spread.tolerance))
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
    def shouldEqual(right: Null)(implicit ev: T <:< AnyRef) { 
      if (leftSideValue != null) {
        throw newTestFailedException(FailureMessages("didNotEqualNull", leftSideValue))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should not equal (3)
     *        ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForAny[T] = new ResultOfNotWordForAny[T](leftSideValue, false)

    // In 2.10, will work with AnyVals. TODO: Also, Need to ensure Char works
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * a should === (b)
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: Constraint[T, U]) {
      if ((constraint.areEqual(leftSideValue, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            leftSideValue,
            inv.right
          )
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
    def should(inv: TripleEqualsInvocationOnSpread[T])(implicit ev: Numeric[T]) {
      if ((inv.spread.isWithin(leftSideValue)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
            if (inv.expectingEqual) "didNotEqualPlusOrMinus" else "equaledPlusOrMinus",
            leftSideValue,
            inv.spread.pivot,
            inv.spread.tolerance
          )
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
    def should(beWord: BeWord): ResultOfBeWordForAny[T] = new ResultOfBeWordForAny(leftSideValue, true)
  
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe 8.8
     *         ^
     * </pre>
     */
    def shouldBe(right: Any) {
      if (!areEqualComparingArraysStructurally(leftSideValue, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(leftSideValue, right)
        throw newTestFailedException(FailureMessages("wasNotEqualTo", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 5 shouldBe &lt; (7) 
     *   ^
     * </pre>
     */
    def shouldBe(comparison: ResultOfLessThanComparison[T]) {
      if (!comparison(leftSideValue)) {
        throw newTestFailedException(
          FailureMessages(
            "wasNotLessThan",
            leftSideValue,
            comparison.right
          )
        ) 
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 8 shouldBe &gt; (7) 
     *   ^
     * </pre> 
     */
    def shouldBe(comparison: ResultOfGreaterThanComparison[T]) {
      if (!comparison(leftSideValue)) {
        throw newTestFailedException(
          FailureMessages(
            "wasNotGreaterThan",
            leftSideValue,
            comparison.right
          )
        ) 
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 5 shouldBe &lt;= (7) 
     *   ^
     * </pre> 
     */
    def shouldBe(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      if (!comparison(leftSideValue)) {
        throw newTestFailedException(
          FailureMessages(
            "wasNotLessThanOrEqualTo",
            leftSideValue,
            comparison.right
          )
        ) 
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * 8 shouldBe &gt;= (7) 
     *   ^
     * </pre> 
     */
    def shouldBe(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      if (!comparison(leftSideValue)) {
        throw newTestFailedException(
          FailureMessages(
            "wasNotGreaterThanOrEqualTo",
            leftSideValue,
            comparison.right
          )
        ) 
      }
    }
    
    /**
     * This method enables the following syntax, where <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">testing
     * 1 shouldBe odd
     *   ^
     * </pre>
     */
    def shouldBe(beMatcher: BeMatcher[T]) {
      val result = beMatcher.apply(leftSideValue)
      if (!result.matches)
        throw newTestFailedException(result.failureMessage)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe 7.1 +- 0.2
     *        ^
     * </pre>
     */
    def shouldBe(spread: Spread[T]) {
      if (!spread.isWithin(leftSideValue)) {
        throw newTestFailedException(FailureMessages("wasNotPlusOrMinus", leftSideValue, spread.pivot, spread.tolerance))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe sorted
     *        ^
     * </pre>
     */
    def shouldBe(right: SortedWord)(implicit sortable: Sortable[T]) {
      if (!sortable.isSorted(leftSideValue))
        throw newTestFailedException(FailureMessages("wasNotSorted", leftSideValue))
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe a [Book]
     *         ^
     * </pre>
     */
    def shouldBe(aType: ResultOfATypeInvocation[_]) = macro TypeMatcherMacro.shouldBeATypeImpl
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe an [Book]
     *         ^
     * </pre>
     */
    def shouldBe(anType: ResultOfAnTypeInvocation[_]) = macro TypeMatcherMacro.shouldBeAnTypeImpl
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe readable
     *        ^
     * </pre>
     */
    def shouldBe(right: ReadableWord)(implicit readability: Readability[T]) {
      if (!readability.isReadable(leftSideValue))
        throw newTestFailedException(FailureMessages("wasNotReadable", leftSideValue))
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe writable
     *        ^
     * </pre>
     */
    def shouldBe(right: WritableWord)(implicit writability: Writability[T]) {
      if (!writability.isWritable(leftSideValue))
        throw newTestFailedException(FailureMessages("wasNotWritable", leftSideValue))
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe empty
     *        ^
     * </pre>
     */
    def shouldBe(right: EmptyWord)(implicit emptiness: Emptiness[T]) {
      if (!emptiness.isEmpty(leftSideValue))
        throw newTestFailedException(FailureMessages("wasNotEmpty", leftSideValue))
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe defined
     *        ^
     * </pre>
     */
    def shouldBe(right: DefinedWord)(implicit definition: Definition[T]) {
      if (!definition.isDefined(leftSideValue))
        throw newTestFailedException(FailureMessages("wasNotDefined", leftSideValue))
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldNot be (3)
     *        ^
     * </pre>
     */
    def shouldNot(beWord: BeWord): ResultOfBeWordForAny[T] = new ResultOfBeWordForAny(leftSideValue, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldNot (be (3))
     *        ^
     * </pre>
     */
    def shouldNot(rightMatcherX1: Matcher[T]) {
      ShouldMethodHelper.shouldNotMatcher(leftSideValue, rightMatcherX1)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldNot (be readable)
     *        ^
     * </pre>
     */
    def shouldNot[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]) {
      ShouldMethodHelper.shouldNotMatcher(leftSideValue, rightMatcherFactory1.matcher)
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
    def shouldNot(haveWord: HaveWord): ResultOfHaveWordForExtent[T] =
      new ResultOfHaveWordForExtent(leftSideValue, false)

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
    def should(haveWord: HaveWord): ResultOfHaveWordForExtent[T] =
      new ResultOfHaveWordForExtent(leftSideValue, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe null
     *        ^
     * </pre>
     */
    def shouldBe(right: Null)(implicit ev: T <:< AnyRef) {
      if (leftSideValue != null) {
        throw newTestFailedException(FailureMessages("wasNotNull", leftSideValue))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldBe theSameInstanceAs (anotherObject)
     *        ^
     * </pre>
     */
    def shouldBe(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication)(implicit toAnyRef: T <:< AnyRef) {
      if (resultOfSameInstanceAsApplication.right ne toAnyRef(leftSideValue)) {
        throw newTestFailedException(
          FailureMessages(
            "wasNotSameInstanceAs",
            leftSideValue,
            resultOfSameInstanceAsApplication.right
          )
        )
      }
    }
    
// TODO: Remember to write tests for inspector shorthands uncovering the bug below, always a empty because always true true passed to matchSym
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * list shouldBe 'empty
     *      ^
     * </pre>
     */
    def shouldBe(symbol: Symbol)(implicit toAnyRef: T <:< AnyRef) {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(leftSideValue), symbol, false, true)
      if (!matcherResult.matches) 
        throw newTestFailedException(matcherResult.failureMessage)
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * list shouldBe a ('empty)
     *      ^
     * </pre>
     */
    def shouldBe(resultOfAWordApplication: ResultOfAWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef) {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(leftSideValue), resultOfAWordApplication.symbol, true, true)
      if (!matcherResult.matches) {
        throw newTestFailedException(
          matcherResult.failureMessage
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * list shouldBe an ('empty)
     *      ^
     * </pre>
     */
    def shouldBe(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication)(implicit toAnyRef: T <:< AnyRef) {
      val matcherResult = matchSymbolToPredicateMethod(toAnyRef(leftSideValue), resultOfAnWordApplication.symbol, true, false)
      if (!matcherResult.matches) {
        throw newTestFailedException(
          matcherResult.failureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * programmingInScala shouldBe excellentRead
     *                    ^
     * </pre>
     */
    def shouldBe(bePropertyMatcher: BePropertyMatcher[T])(implicit ev: T <:< AnyRef) { // TODO: Try expanding this to 2.10 AnyVal
      val result = bePropertyMatcher(leftSideValue)
      if (!result.matches) 
        throw newTestFailedException(FailureMessages("wasNot", leftSideValue, UnquotedString(result.propertyName)))
    }
    
    /**
     * This method enables the following syntax, where <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * programmingInScala shouldBe a (goodRead)
     *                    ^
     * </pre>
     */
    def shouldBe[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef) {// TODO: Try expanding this to 2.10 AnyVal
      val result = resultOfAWordApplication.bePropertyMatcher(leftSideValue)
        if (!result.matches) {
          throw newTestFailedException(FailureMessages("wasNotA", leftSideValue, UnquotedString(result.propertyName)))
        }
    }
    
    /**
     * This method enables the following syntax, where <code>excellentRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * programmingInScala shouldBe an (excellentRead)
     *                    ^
     * </pre>
     */
    def shouldBe[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U])(implicit ev: T <:< AnyRef) {// TODO: Try expanding this to 2.10 AnyVal
      val result = resultOfAnWordApplication.bePropertyMatcher(leftSideValue)
        if (!result.matches) {
          throw newTestFailedException(FailureMessages("wasNotAn", leftSideValue, UnquotedString(result.propertyName)))
        }
    }

/*
    def shouldBe[U](right: AType[U]) {
      if (!right.isAssignableFromClassOf(leftSideValue)) {
        throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", leftSideValue, UnquotedString(right.className), UnquotedString(leftSideValue.getClass.getName)))
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
    def should(containWord: ContainWord): ResultOfContainWord[T] = {
      new ResultOfContainWord(leftSideValue, true)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * xs shouldNot contain (oneOf (1, 2, 3))
     *    ^
     * </pre>
     */
    def shouldNot(contain: ContainWord): ResultOfContainWord[T] = 
      new ResultOfContainWord(leftSideValue, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * file should exist
     *      ^
     * </pre>
     */
    def should(existWord: ExistWord)(implicit existence: Existence[T]) {
      if (!existence.exists(leftSideValue))
        throw newTestFailedException(FailureMessages("doesNotExist", leftSideValue))
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * file should not (exist)
     *      ^
     * </pre>
     */
    def should(notExist: ResultOfNotExist)(implicit existence: Existence[T]) {
      if (existence.exists(leftSideValue))
        throw newTestFailedException(FailureMessages("exists", leftSideValue))
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * file shouldNot exist
     *      ^
     * </pre>
     */
    def shouldNot(existWord: ExistWord)(implicit existence: Existence[T]) {
      if (existence.exists(leftSideValue))
        throw newTestFailedException(FailureMessages("exists", leftSideValue))
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
    def should(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForString = {
      new ResultOfIncludeWordForString(leftSideValue, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should startWith regex ("hello")
     *        ^
     * </pre>
     */
    def should(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForString = {
      new ResultOfStartWithWordForString(leftSideValue, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should endWith regex ("world")
     *        ^
     * </pre>
     */
    def should(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForString = {
      new ResultOfEndWithWordForString(leftSideValue, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot startWith regex ("hello")
     *        ^
     * </pre>
     */
    def shouldNot(startWithWord: StartWithWord)(implicit ev: T <:< String): ResultOfStartWithWordForString = 
      new ResultOfStartWithWordForString(leftSideValue, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot endWith regex ("world")
     *        ^
     * </pre>
     */
    def shouldNot(endWithWord: EndWithWord)(implicit ev: T <:< String): ResultOfEndWithWordForString = 
      new ResultOfEndWithWordForString(leftSideValue, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot include regex ("hi")
     *        ^
     * </pre>
     */
    def shouldNot(includeWord: IncludeWord)(implicit ev: T <:< String): ResultOfIncludeWordForString = 
      new ResultOfIncludeWordForString(leftSideValue, false)
  }

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
  final class StringShouldWrapper(val leftSideString: String) extends AnyShouldWrapper(leftSideString) with StringShouldWrapperForVerb {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("a(b*)c" withGroup "bb") 
     *                                          ^
     * </pre>
     */
    def withGroup(group: String) = 
      new RegexWithGroups(leftSideString.r, IndexedSeq(group))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
     *                                             ^
     * </pre>
     */
    def withGroups(groups: String*) =
      new RegexWithGroups(leftSideString.r, IndexedSeq(groups: _*))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *        ^
     * </pre>
     */
    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = {
      new ResultOfFullyMatchWordForString(leftSideString, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *        ^
     * </pre>
     */
    def shouldNot(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = 
      new ResultOfFullyMatchWordForString(leftSideString, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should compile
     *        ^
     * </pre>
     */
    def should(compileWord: CompileWord): Unit = macro CompileMacro.shouldCompileImpl

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot compile
     *        ^
     * </pre>
     */
    def shouldNot(compileWord: CompileWord): Unit = macro CompileMacro.shouldNotCompileImpl

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot typeCheck
     *        ^
     * </pre>
     */
    def shouldNot(typeCheckWord: TypeCheckWord): Unit = macro CompileMacro.shouldNotTypeCheckImpl

/*
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should include regex ("hi")
     *        ^
     * </pre>
     */
    def should(includeWord: IncludeWord): ResultOfIncludeWordForString = {
      new ResultOfIncludeWordForString(leftSideString, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should startWith regex ("hello")
     *        ^
     * </pre>
     */
    def should(startWithWord: StartWithWord): ResultOfStartWithWordForString = {
      new ResultOfStartWithWordForString(leftSideString, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should endWith regex ("world")
     *        ^
     * </pre>
     */
    def should(endWithWord: EndWithWord): ResultOfEndWithWordForString = {
      new ResultOfEndWithWordForString(leftSideString, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *        ^
     * </pre>
     */
    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = {
      new ResultOfFullyMatchWordForString(leftSideString, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should not have length (3)
     *        ^
     * </pre>
     */
    override def should(notWord: NotWord): ResultOfNotWordForString = {
      new ResultOfNotWordForString(leftSideString, false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("a(b*)c" withGroup "bb") 
     *                                          ^
     * </pre>
     */
    def withGroup(group: String) = 
      new RegexWithGroups(leftSideString.r, IndexedSeq(group))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
     *                                             ^
     * </pre>
     */
    def withGroups(groups: String*) = 
      new RegexWithGroups(leftSideString.r, IndexedSeq(groups: _*))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *        ^
     * </pre>
     */
    def shouldNot(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForString = 
      new ResultOfFullyMatchWordForString(leftSideString, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot startWith regex ("hello")
     *        ^
     * </pre>
     */
    def shouldNot(startWithWord: StartWithWord): ResultOfStartWithWordForString = 
      new ResultOfStartWithWordForString(leftSideString, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot endWith regex ("world")
     *        ^
     * </pre>
     */
    def shouldNot(endWithWord: EndWithWord): ResultOfEndWithWordForString = 
      new ResultOfEndWithWordForString(leftSideString, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string shouldNot include regex ("hi")
     *        ^
     * </pre>
     */
    def shouldNot(includeWord: IncludeWord): ResultOfIncludeWordForString = 
      new ResultOfIncludeWordForString(leftSideString, false)
*/
  }

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

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * regex should fullyMatch regex ("a(b*)c" withGroup "bb") 
     *                                         ^
     * </pre>
     */
    def withGroup(group: String) = 
      new RegexWithGroups(regex, IndexedSeq(group))

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * regex should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
     *                                            ^
     * </pre>
     */
    def withGroups(groups: String*) = 
      new RegexWithGroups(regex, IndexedSeq(groups: _*))
  }

  /**
   * Implicitly converts an object of type <code>T</code> to a <code>AnyShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToAnyShouldWrapper[T](o: T): AnyShouldWrapper[T] = new AnyShouldWrapper(o)

  /**
   * Implicitly converts an object of type <code>java.lang.String</code> to a <code>StringShouldWrapper</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit override def convertToStringShouldWrapper(o: String): StringShouldWrapper = new StringShouldWrapper(o)

  /**
   * Implicitly converts an object of type <code>scala.util.matching.Regex</code> to a <code>RegexWrapper</code>,
   * to enable <code>withGroup</code> and <code>withGroups</code> methods to be invokable on that object.
   */
  implicit def convertToRegexWrapper(o: Regex): RegexWrapper = new RegexWrapper(o)

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * book should have (message ("A TALE OF TWO CITIES") (of [Book]), title ("A Tale of Two Cities"))
   *                                                     ^
   * </pre>
   */
  def of[T](implicit ev: Manifest[T]): ResultOfOfTypeInvocation[T] = new ResultOfOfTypeInvocation[T]
}

/**
 * Companion object that facilitates the importing of <code>Matchers</code> members as 
 * an alternative to mixing it the trait. One use case is to import <code>Matchers</code> members so you can use
 * them in the Scala interpreter.
 *
 * @author Bill Venners
 */
object Matchers extends Matchers
