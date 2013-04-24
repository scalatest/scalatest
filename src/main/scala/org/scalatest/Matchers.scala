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
package org.scalatest // Change me in MustMatchers

import org.scalatest.matchers._
import org.scalatest.enablers._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.reflect.Manifest
import Helper.transformOperatorChars
import scala.collection.Traversable
import Assertions.areEqualComparingArraysStructurally
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import org.scalautils.Tolerance
import org.scalautils.Explicitly
import org.scalautils.Interval
import org.scalautils.TripleEqualsInvocation
import scala.annotation.tailrec
import org.scalautils.Equality
import org.scalatest.words.ShouldVerb
import org.scalautils.TripleEqualsInvocationOnInterval
import org.scalautils.EqualityConstraint
import Matchers.andMatchersAndApply
import Matchers.orMatchersAndApply
import words.MatcherWords
import words.FullyMatchWord
import words.StartWithWord
import words.EndWithWord
import words.IncludeWord
import words.HaveWord
import words.BeWord
import words.NotWord
import words.ContainWord
import words.NoneOfContainMatcher
import words.OnlyContainMatcher
import words.TheSameIteratedElementsAsContainMatcher
import words.AllOfContainMatcher
import words.InOrderContainMatcher
import words.InOrderOnlyContainMatcher
import words.OneOfContainMatcher
import words.TheSameElementsAsContainMatcher
import Matchers.matchSymbolToPredicateMethod
import words.ResultOfLengthWordApplication
import words.ResultOfSizeWordApplication
import words.ResultOfLessThanComparison
import words.ResultOfGreaterThanComparison
import words.ResultOfLessThanOrEqualToComparison
import words.ResultOfGreaterThanOrEqualToComparison
import words.ResultOfAWordToSymbolApplication
import words.ResultOfAWordToBePropertyMatcherApplication
import words.ResultOfAWordToAMatcherApplication
import words.ResultOfAnWordToSymbolApplication
import words.ResultOfAnWordToBePropertyMatcherApplication
import words.ResultOfAnWordToAnMatcherApplication
import words.ResultOfTheSameInstanceAsApplication
import words.ResultOfRegexWordApplication
import words.ResultOfKeyWordApplication
import words.ResultOfValueWordApplication

// TODO: drop generic support for be as an equality comparison, in favor of specific ones.
// TODO: mention on JUnit and TestNG docs that you can now mix in ShouldMatchers or MustMatchers
// TODO: Put links from ShouldMatchers to wherever I reveal the matrix and algo of how properties are checked dynamically.
// TODO: double check that I wrote tests for (length (7)) and (size (8)) in parens
// TODO: document how to turn off the === implicit conversion
// TODO: Document you can use JMock, EasyMock, etc.

import Helper.accessProperty

/**
 * Trait that provides a domain specific language (DSL) for expressing assertions in tests
 * using the word <code>should</code>. For example, if you mix <code>Matchers</code> into
 * a suite class, you can write an equality assertion in that suite like this:
 * 
 * <pre class="stHighlight">
 * result should equal (3)
 * </pre>
 * 
 * <p>
 * Here <code>result</code> is a variable, and can be of any type. If the object is an
 * <code>Int</code> with the value 3, execution will continue (<em>i.e.</em>, the expression will result
 * in the unit value, <code>()</code>). Otherwise, a <code>TestFailedException</code>
 * will be thrown with a detail message that explains the problem, such as <code>"7 did not equal 3"</code>.
 * This <code>TestFailedException</code> will cause the test to fail.
 * </p>
 * 
 * <p>
 * The <code>left should equal (right)</code> syntax works by calling <code>==</code>  on the <code>left</code>
 * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
 * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
 * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
 * </p>
 * 
 * <pre class="stHighlight">
 * Array(1, 2) == Array(1, 2) // yields false
 * </pre>
 *
 * <p>
 * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest compares
 * the two arrays structurally, taking into consideration the equality of the array's contents:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) should equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
 * <code>be theSameInstanceAs</code> syntax, described below.
 * </p>
 *
 * <h2>Checking size and length</h2>
 * 
 * <p>
 * You can check the size or length of just about any type of object for which it
 * would make sense. Here's how checking for length looks:
 * </p>
 * <pre class="stHighlight">
 * result should have length (3)
 * </pre>
 * 
 * <p>
 * Size is similar:
 * </p>
 * 
 * <pre class="stHighlight">
 * result should have size (10)
 * </pre>
 * 
 * <p>
 * The <code>length</code> syntax can be used with <code>String</code>, <code>Array</code>, any <code>scala.collection.GenSeq</code>,
 * any <code>java.util.List</code>, and any type <code>T</code> for which an implicit <code>Length[T]</code> type class is 
 * available in scope.
 * Similarly, the <code>size</code> syntax can be used with <code>Array</code>, any <code>scala.collection.GenTraversable</code>,
 * any <code>java.util.List</code>, and any type <code>T</code> for which an implicit <code>Size[T]</code> type class is 
 * available in scope. You can enable the <code>length</code> or <code>size</code> syntax for your own arbitrary types, therefore,
 * by defining <a href="ClassicMatchers$Length.html"><code>Length</code></a> or <a href="ClassicMatchers$Size.html"><code>Size</code></a> type
 * classes for those types.
 * </p>
 * 
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
 * string should startWith regex ("Hel*o")
 * string should endWith regex ("wo.ld")
 * string should include regex ("wo.ld")
 * </pre>
 * 
 * <p>
 * And you can check whether a string fully matches a regular expression, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * string should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
 * </pre>
 * 
 * <p>
 * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
 * or a <code>scala.util.matching.Regex</code>.
 * </p>
 *
 * <h2>Greater and less than</h2>
 * <p>
 * You can check whether any type that is, or can be implicitly converted to,
 * an <code>Ordered[T]</code> is greater than, less than, greater than or equal, or less
 * than or equal to a value of type <code>T</code>. The syntax is:
 * </p>
 * <pre class="stHighlight">
 * one should be < (7)
 * one should be > (0)
 * one should be <= (7)
 * one should be >= (0)
 * </pre>
 * 
 * <h2>Checking equality with <code>be</code> <code>=</code><code>=</code><code>=</code></h2>
 *
 * <p>
 * An alternate way to check for equality of two objects is to use <code>be</code> with
 * <code>===</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * result should be === (3)
 * </pre>
 *
 * <p>
 * Here <code>result</code> is a variable, and can be of any type. If the object is an
 * <code>Int</code> with the value 3, execution will continue (<em>i.e.</em>, the expression will result
 * in the unit value, <code>()</code>). Otherwise, a <code>TestFailedException</code>
 * will be thrown with a detail message that explains the problem, such as <code>"7 was not equal to 3"</code>.
 * This <code>TestFailedException</code> will cause the test to fail.
 * </p>
 *
 * <p>
 * The <code>left should be === (right)</code> syntax works by calling <code>==</code>  on the <code>left</code>
 * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
 * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
 * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) == Array(1, 2) // yields false
 * </pre>
 *
 * <p>
 * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest compares
 * the two arrays structurally, taking into consideration the equality of the array's contents:
 * </p>
 *
 * <pre class="stHighlight">
 * Array(1, 2) should be === (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
 * </pre>
 *
 * <p>
 * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
 * <code>be theSameInstanceAs</code> syntax, described below.
 * </p>
 *
 * <h2>Checking <code>Boolean</code> properties with <code>be</code></h2>
 * 
 * <p>
 * If an object has a method that takes no parameters and returns boolean, you can check
 * it by placing a <code>Symbol</code> (after <code>be</code>) that specifies the name
 * of the method (excluding an optional prefix of "<code>is</code>"). A symbol literal
 * in Scala begins with a tick mark and ends at the first non-identifier character. Thus,
 * <code>'empty</code> results in a <code>Symbol</code> object at runtime, as does
 * <code>'defined</code> and <code>'file</code>. Here's an example:
 * </p>
 * 
 * <pre class="stHighlight">
 * emptySet should be ('empty)
 * </pre>
 * 
 * Given this code, ScalaTest will use reflection to look on the object referenced from
 * <code>emptySet</code> for a method that takes no parameters and results in <code>Boolean</code>,
 * with either the name <code>empty</code> or <code>isEmpty</code>. If found, it will invoke
 * that method. If the method returns <code>true</code>, execution will continue. But if it returns
 * <code>false</code>, a <code>TestFailedException</code> will be thrown that will contain a detail message, such as:
 * 
 * <pre class="stHighlight">
 * Set(1, 2, 3) was not empty
 * </pre>
 * 
 * <p>
 * This <code>be</code> syntax can be used with any type.  If the object does
 * not have an appropriately named predicate method, you'll get a <code>TestFailedException</code>
 * at runtime with a detail message that explains the problem.
 * (For the details on how a field or method is selected during this
 * process, see the documentation for <a href="Matchers$BeWord.html"><code>BeWord</code></a>.)
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
 * temp should be a ('file)
 * </pre>
 * 
 * <p>
 * Or, given <code>java.awt.event.KeyEvent</code> has a method <code>isActionKey</code> that takes
 * no arguments and returns <code>Boolean</code>, you could assert that a <code>KeyEvent</code> is
 * an action key with:
 *</p>
 *
 * <pre class="stHighlight">
 * keyEvent should be an ('actionKey)
 * </pre>
 * 
 * <p>
 * If you prefer to check <code>Boolean</code> properties in a type-safe manner, you can use a <code>BePropertyMatcher</code>.
 * This would allow you to write expressions such as:
 * </p>
 *
 * <pre class="stHighlight">
 * emptySet should be (empty)
 * temp should be a (file)
 * keyEvent should be an (actionKey)
 * </pre>
 * 
 * <p>
 * These expressions would fail to compile if <code>should</code> is used on an inappropriate type, as determined
 * by the type parameter of the <code>BePropertyMatcher</code> being used. (For example, <code>file</code> in this example
 * would likely be of type <code>BePropertyMatcher[java.io.File]</code>. If used with an appropriate type, such an expression will compile
 * and at run time the <code>Boolean</code> property method or field will be accessed directly; <em>i.e.</em>, no reflection will be used.
 * See the documentation for <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a> for more information.
 * </p>
 *
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
 * num should be (odd)
 * num should not be (even)
 * </pre>
 *
 * For more information, see the documentation for <a href="BeMatcher.html"><code>BeMatcher</code></a>.
 *
 * <h2>Checking object identity</h2>
 * 
 * <p>
 * If you need to check that two references refer to the exact same object, you can write:
 * </p>
 * 
 * <pre class="stHighlight">
 * ref1 should be theSameInstanceAs (ref2)
 * </pre>
 * 
 * <h2>Checking numbers against a range</h2>
 * 
 * <p>
 * To check whether a floating point number has a value that exactly matches another, you
 * can use <code>should equal</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * sevenDotOh should equal (7.0)
 * </pre>
 * 
 * <p>
 * Often, however, you may want to check whether a floating point number is within a
 * range. You can do that using <code>be</code> and <code>plusOrMinus</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * sevenDotOh should be (6.9 plusOrMinus 0.2)
 * </pre>
 * 
 * <p>
 * This expression will cause a <code>TestFailedException</code> to be thrown if the floating point
 * value, <code>sevenDotOh</code> is outside the range <code>6.7</code> to <code>7.1</code>.
 * You can also use <code>plusOrMinus</code> with integral types, for example:
 * </p>
 * 
 * <pre class="stHighlight">
 * seven should be (6 plusOrMinus 2)
 * </pre>
 * 
 * <h2>Traversables, iterables, sets, sequences, and maps</h2>
 * 
 * <p>
 * You can use some of the syntax shown previously with <code>Iterable</code> and its
 * subtypes. For example, you can check whether an <code>Iterable</code> is <code>empty</code>,
 * like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * iterable should be ('empty)
 * </pre>
 * 
 * <p>
 * You can check the length of an <code>Seq</code> (<code>Array</code>, <code>List</code>, etc.),
 * like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * array should have length (3)
 * list should have length (9)
 * </pre>
 * 
 * <p>
 * You can check the size of any <code>Traversable</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * map should have size (20)
 * set should have size (90)
 * </pre>
 * 
 * <p>
 * In addition, you can check whether an <code>Iterable</code> contains a particular
 * element, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * iterable should contain ("five")
 * </pre>
 * 
 * <p>
 * You can also check whether a <code>Map</code> contains a particular key, or value, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * map should contain key (1)
 * map should contain value ("Howdy")
 * </pre>
 * 
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
 * javaList should have length (9)
 * </pre>
 * 
 * <p>
 * You can check the size of any Java <code>Collection</code> or <code>Map</code>, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * javaMap should have size (20)
 * javaSet should have size (90)
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
 * One difference to note between the syntax supported on Java collections and that of Scala
 * iterables is that you can't use <code>contain (...)</code> syntax with a Java <code>Map</code>.
 * Java differs from Scala in that its <code>Map</code> is not a subtype of its <code>Collection</code> type.
 * If you want to check that a Java <code>Map</code> contains a specific key/value pair, the best approach is
 * to invoke <code>entrySet</code> on the Java <code>Map</code> and check that entry set for the appropriate
 * element (a <code>java.util.Map.Entry</code>) using <code>contain (...)</code>.
 * </p>
 *
 * <p>
 * Despite this difference, the other (more commonly used) map matcher syntax works just fine on Java <code>Map</code>s.
 * You can, for example, check whether a Java <code>Map</code> contains a particular key, or value, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * javaMap should contain key (1)
 * javaMap should contain value ("Howdy")
 * </pre>
 * 
 * <h2>Be as an equality comparison</h2>
 * 
 * <p>
 * All uses of <code>be</code> other than those shown previously perform an equality comparison. In other words, they work
 * the same as <code>equals</code>. This redundance between <code>be</code> and <code>equals</code> exists because it enables syntax
 * that sometimes sounds more natural. For example, instead of writing: 
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
 * As with <code>equal</code>, using <code>be</code> on two arrays results in <code>deep</code> being called on both arrays prior to
 * calling <code>equal</code>. As a result,
 * the following expression would <em>not</em> throw a <code>TestFailedException</code>:
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
 * <h2>Being negative</h2>
 * 
 * <p>
 * If you wish to check the opposite of some condition, you can simply insert <code>not</code> in the expression.
 * Here are a few examples:
 * </p>
 * 
 * <pre class="stHighlight">
 * result should not be (null)
 * sum should not be <= (10)
 * mylist should not equal (yourList)
 * string should not startWith ("Hello")
 * </pre>
 * 
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
 * number should (be > (0) and be <= (10))
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
 * on regular <code>Boolean</code>s using its <code>&&</code> and <code>||</code> operators. First, expressions with <code>and</code>
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
 * The other difference with <code>Boolean</code> operators is that although <code>&&</code> has a higher precedence than <code>||</code>,
 * <code>and</code> and <code>or</code>
 * have the same precedence. Thus although the <code>Boolean</code> expression <code>(a || b && c)</code> will evaluate the <code>&&</code> expression
 * before the <code>||</code> expression, like <code>(a || (b && c))</code>, the following expression:
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
 * <h2>Working with <code>Option</code>s</h2>
 * 
 * <p>
 * ScalaTest matchers has no special support for <code>Option</code>s, but you can 
 * work with them quite easily using syntax shown previously. For example, if you wish to check
 * whether an option is <code>None</code>, you can write any of:
 * </p>
 * 
 * <pre class="stHighlight">
 * option should equal (None)
 * option should be (None)
 * option should not be ('defined)
 * option should be ('empty)
 * </pre>
 * 
 * <p>
 * If you wish to check an option is defined, and holds a specific value, you can write either of:
 * </p>
 * 
 * <pre class="stHighlight">
 * option should equal (Some("hi"))
 * option should be (Some("hi"))
 * </pre>
 * 
 * <p>
 * If you only wish to check that an option is defined, but don't care what it's value is, you can write:
 * </p>
 * 
 * <pre class="stHighlight">
 * option should be ('defined)
 * </pre>
 * 
 * <p>
 * If you mix in (or import the members of) <a href="../OptionValues.html"><code>OptionValues</code></a>,
 * you can write one statement that indicates you believe an option should be defined and then say something else about its value. Here's an example:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.OptionValues._
 * option.value should be &lt; (7)
 * </pre>
 * 
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
 * with a detail message that explains the problem. For example, if you assert the following on
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
 * See the documentation for <a href="HavePropertyMatcher.html"><code>HavePropertyMatcher</code></a> for more information.
 * </p>
 *
 * <h2>Using custom matchers</h2>
 * 
 * <p>
 * If none of the built-in matcher syntax (or options shown so far for extending the syntax) satisfy a particular need you have, you can create
 * custom <code>Matcher</code>s that allow
 * you to place your own syntax directly after <code>should</code>. For example, class <code>java.io.File</code> has a method <code>exists</code>, which
 * indicates whether a file of a certain path and name exists. Because the <code>exists</code> method takes no parameters and returns <code>Boolean</code>,
 * you can call it using <code>be</code> with a symbol or <code>BePropertyMatcher</code>, yielding assertions like:
 * </p>
 * 
 * <pre class="stHighlight">
 * file should be ('exists)  // using a symbol
 * file should be (inExistance)   // using a BePropertyMatcher
 * </pre>
 * 
 * <p>
 * Although these expressions will achieve your goal of throwing a <code>TestFailedException</code> if the file does not exist, they don't produce
 * the most readable code because the English is either incorrect or awkward. In this case, you might want to create a
 * custom <code>Matcher[java.io.File]</code>
 * named <code>exist</code>, which you could then use to write expressions like:
 * </p>
 *
 * <pre class="stHighlight">
 * // using a plain-old Matcher
 * file should exist
 * file should not (exist)
 * file should (exist and have ('name ("temp.txt")))
 * </pre>
 * 
 * <p>
 * Note that when you use custom <code>Matcher</code>s, you will need to put parentheses around the custom matcher in more cases than with
 * the built-in syntax. For example you will often need the parentheses after <code>not</code>, as shown above. (There's no penalty for
 * always surrounding custom matchers with parentheses, and if you ever leave them off when they are needed, you'll get a compiler error.)
 * For more information about how to create custom <code>Matcher</code>s, please see the documentation for the <a href="Matcher.html"><code>Matcher</code></a> trait.
 * </p>
 *
 * <h2>Checking for expected exceptions</h2>
 *
 * <p>
 * Sometimes you need to test whether a method throws an expected exception under certain circumstances, such
 * as when invalid arguments are passed to the method. With <code>Matchers</code> mixed in, you can
 * check for an expected exception like this:
 * </p>
 *
 * <pre class="stHighlight">
 * evaluating { s.charAt(-1) } should produce [IndexOutOfBoundsException]
 * </pre>
 *
 * <p>
 * If <code>charAt</code> throws an instance of <code>StringIndexOutOfBoundsException</code>,
 * this expression will result in that exception. But if <code>charAt</code> completes normally, or throws a different
 * exception, this expression will complete abruptly with a <code>TestFailedException</code>.
 * This expression returns the caught exception so that you can inspect it further if you wish, for
 * example, to ensure that data contained inside the exception has the expected values. Here's an
 * example:
 * </p>
 *
 * <pre class="stHighlight">
 * val thrown = evaluating { s.charAt(-1) } should produce [IndexOutOfBoundsException]
 * thrown.getMessage should equal ("String index out of range: -1")
 * </pre>
 *
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
 * 1. Although you don't always need them, it is recommended style to always put parentheses
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
 * 2. Except for <code>length</code> and <code>size</code>, you must always put parentheses around
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
 * 4. Although you don't always need them, it is recommended style to always put parentheses
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
 */
trait Matchers extends Assertions with Tolerance with ShouldVerb with LoneElement with MatcherWords with Explicitly { matchers =>

  private[scalatest] def newTestFailedException(message: String, optionalCause: Option[Throwable] = None, stackDepthAdjustment: Int = 0): Throwable = {
    val temp = new RuntimeException
    // should not look for anything in the first 2 elements, caller stack element is at 3rd/4th
    // also, it solves the problem when the suite file that mixin in Matchers has the [suiteFileName]:newTestFailedException appears in the top 2 elements
    // this approach should be better than adding && _.getMethodName == newTestFailedException we used previously.
    val elements = temp.getStackTrace.drop(2) 
    val stackDepth = elements.indexWhere(st => st.getFileName != "Matchers.scala") + 2 // the first 2 elements dropped previously
    optionalCause match {
      case Some(cause) => new TestFailedException(message, cause, stackDepth + stackDepthAdjustment)
      case None => new TestFailedException(message, stackDepth + stackDepthAdjustment)
    }
  }

  private[scalatest] def matchContainMatcher[T](left: GenTraversable[T], containMatcher: ContainMatcher[T], shouldBeTrue: Boolean) {
    val result = containMatcher(left)
    if (result.matches != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
        None, 
        2
      )
  }
  
  private[scalatest] class JavaCollectionWrapper[T](underlying: java.util.Collection[T]) extends Traversable[T] {
    def foreach[U](f: (T) => U) {
      val javaIterator = underlying.iterator
      while (javaIterator.hasNext)
        f(javaIterator.next)
    }
    override def toString: String = if (underlying == null) "null" else underlying.toString
  }
  
  private[scalatest] def matchContainMatcher[T](left: java.util.Collection[T], containMatcher: ContainMatcher[T], shouldBeTrue: Boolean) {
    val result = containMatcher(new JavaCollectionWrapper(left))
    if (result.matches != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
        None, 
        2
      )
  }
  
  private[scalatest] class JavaMapWrapper[K, V](val underlying: java.util.Map[K, V]) extends scala.collection.Map[K, V] {
    // Even though the java map is mutable I just wrap it it to a plain old Scala map, because
    // I have no intention of mutating it.
    override def size: Int = underlying.size
    def get(key: K): Option[V] =
      if (underlying.containsKey(key)) Some(underlying.get(key)) else None
    override def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
      private val javaIterator = underlying.entrySet.iterator
      def next: (K, V) = {
        val nextEntry = javaIterator.next
        (nextEntry.getKey, nextEntry.getValue)
      }
      def hasNext: Boolean = javaIterator.hasNext
    }
    override def +[W >: V] (kv: (K, W)): scala.collection.Map[K, W] = {
      val newJavaMap = new java.util.LinkedHashMap[K, W](underlying)
      val (key, value) = kv
      newJavaMap.put(key, value)
      new JavaMapWrapper[K, W](newJavaMap)
    }
    override def - (key: K): scala.collection.Map[K, V] = {
      val newJavaMap = new java.util.LinkedHashMap[K, V](underlying)
      newJavaMap.remove(key)
      new JavaMapWrapper[K, V](underlying)
    }
    override def empty = new JavaMapWrapper[K, V](new java.util.LinkedHashMap[K, V]())
    override def toString: String = if (underlying == null) "null" else underlying.toString
  }
  
  private[scalatest] def matchContainMatcher[K, V](left: java.util.Map[K, V], containMatcher: ContainMatcher[(K, V)], shouldBeTrue: Boolean) {
    val result = containMatcher(new JavaMapWrapper(left))
    if (result.matches != shouldBeTrue)
      throw newTestFailedException(
        if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
        None, 
        2
      )
  }
  
  /**
   * This wrapper gives better toString (Array(x, x, x)) as compared to Scala default one (WrappedArray(x, x, x)).
   */
  private[scalatest] class ArrayWrapper[T](underlying: Array[T]) extends Traversable[T] {
    def foreach[U](f: (T) => U) {
      var index = 0
      while (index < underlying.length) {
        index += 1
        f(underlying(index - 1))
      }
    }
    // Need to prettify the array's toString, because by the time it gets to decorateToStringValue, the array
    // has been wrapped in this Traversable and so it won't get prettified anymore by FailureMessages.decorateToStringValue.
    override def toString: String = FailureMessages.prettifyArrays(underlying).toString
  }

  /**
   * This implicit conversion method enables ScalaTest matchers expressions that involve <code>and</code> and <code>or</code>.
   */
  // implicit def convertToMatcherWrapper[T](leftMatcher: Matcher[T]): MatcherWrapper[T] = new MatcherWrapper(leftMatcher)

  //
  // This class is used as the return type of the overloaded should method (in MapShouldWrapper)
  // that takes a HaveWord. It's key method will be called in situations like this:
  //
  // map should have key 1
  //
  // This gets changed to :
  //
  // convertToMapShouldWrapper(map).should(have).key(1)
  //
  // Thus, the map is wrapped in a convertToMapShouldWrapper call via an implicit conversion, which results in
  // a MapShouldWrapper. This has a should method that takes a HaveWord. That method returns a
  // ResultOfHaveWordPassedToShould that remembers the map to the left of should. Then this class
  // ha a key method that takes a K type, they key type of the map. It does the assertion thing.
  // 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForMap[K, V](left: scala.collection.GenMap[K, V], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should contain key ("one")
     *                    ^
     * </pre>
     */
    def key(expectedKey: K) {
      if (left.exists(_._1 == expectedKey) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            expectedKey)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should contain value (1)
     *                    ^
     * </pre>
     */
    def value(expectedValue: V) {
      // if (left.values.contains(expectedValue) != shouldBeTrue) CHANGING FOR 2.8.0 RC1
      if (left.exists(expectedValue == _._2) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            expectedValue)
        )
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain theSameElementsAs List(1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new TheSameElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain theSameIteratedElementsAs List(1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new TheSameIteratedElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain allOf (1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def allOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new AllOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain inOrder (1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def inOrder(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new InOrderContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def oneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new OneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain only (1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def only(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new OnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain inOrderOnly (1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def inOrderOnly(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new InOrderOnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * map should contain noneOf (1 -> "one", 2 -> "two", 3 -> "three")
     *                    ^
     * </pre>
     */
    def noneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new NoneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax (positiveNumberKey is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * map should contain a positiveNumberKey
     *                    ^
     * </pre>
     */
    def a(aMatcher: AMatcher[(K, V)]) {
      left.find(aMatcher(_).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e)
            throw newTestFailedException(FailureMessages("containedA", left, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", left, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax (oddNumberKey is a <code>AnMatcher</code>):
     *
     * <pre class="stHighlight">
     * map should contain an oddNumberKey
     *                    ^
     * </pre>
     */
    def an(anMatcher: AnMatcher[(K, V)]) {
      left.find(anMatcher(_).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e)
            throw newTestFailedException(FailureMessages("containedAn", left, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", left, UnquotedString(anMatcher.nounName)))
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForJavaMap[K, V](left: java.util.Map[K, V], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain key ("two")
     *                        ^
     * </pre>
     */
    def key(expectedKey: K) {
      if (left.containsKey(expectedKey) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
            left,
            expectedKey)
        )
    }

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain value ("2")
     *                        ^
     * </pre>
     */
    def value(expectedValue: V) {
      if (left.containsValue(expectedValue) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
            left,
            expectedValue)
        )
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain theSameElementsAs traversable
     *                        ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new TheSameElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain theSameIteratedElementsAs traversable
     *                        ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new TheSameIteratedElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain allOf (1, 2)
     *                        ^
     * </pre>
     */
    def allOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new AllOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain inOrder (1, 2)
     *                        ^
     * </pre>
     */
    def inOrder(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new InOrderContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain oneOf (1, 2)
     *                        ^
     * </pre>
     */
    def oneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new OneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain only (1, 2)
     *                        ^
     * </pre>
     */
    def only(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new OnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain inOrderOnly (1, 2)
     *                        ^
     * </pre>
     */
    def inOrderOnly(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new InOrderOnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaMap should contain noneOf (1, 2)
     *                        ^
     * </pre>
     */
    def noneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      matchContainMatcher(left, new NoneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax (positiveNumberKey is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain a positiveNumberKey
     *                        ^
     * </pre>
     */
    def a(aMatcher: AMatcher[(K, V)]) {
      val leftWrapper = new JavaMapWrapper(left.asInstanceOf[java.util.Map[K, V]])
      leftWrapper.find(e => aMatcher(e).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e)
            throw newTestFailedException(FailureMessages("containedA", leftWrapper, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", leftWrapper, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax (oddNumberKey is a <code>AnMatcher</code>):
     *
     * <pre class="stHighlight">
     * javaMap should contain an oddNumberKey
     *                        ^
     * </pre>
     */
    def an(anMatcher: AnMatcher[(K, V)]) {
      val leftWrapper = new JavaMapWrapper(left.asInstanceOf[java.util.Map[K, V]])
      leftWrapper.find(e => anMatcher(e).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e)
            throw newTestFailedException(FailureMessages("containedAn", leftWrapper, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", leftWrapper, UnquotedString(anMatcher.nounName)))
      }
    }
  }

  /** 
   * This implicit conversion method enables the following syntax (<code>javaColl</code> is a <code>java.util.Collection</code>):
   *
   * <pre class="stHighlight">
   * javaColl should contain ("two")
   * </pre>
   *
   * The <code>(contain ("two"))</code> expression will result in a <code>Matcher[GenTraversable[String]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[java.util.Collection[String]]</code>.
   */
  implicit def convertTraversableMatcherToJavaCollectionMatcher[T](traversableMatcher: Matcher[GenTraversable[T]]): Matcher[java.util.Collection[T]] =
    new Matcher[java.util.Collection[T]] {
      def apply(left: java.util.Collection[T]): MatchResult = 
        traversableMatcher.apply(new JavaCollectionWrapper(left))
    }

  /**
   * This implicit conversion method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (not contain (3) and not contain (2))
   * </pre>
   *
   * The <code>(not contain ("two"))</code> expression will result in a <code>Matcher[GenTraversable[String]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[Array[String]]</code>.
  */
  implicit def convertTraversableMatcherToArrayMatcher[T](traversableMatcher: Matcher[GenTraversable[T]]): Matcher[Array[T]] =
    new Matcher[Array[T]] {
      def apply(left: Array[T]): MatchResult = 
        traversableMatcher.apply(new ArrayWrapper(left))
    }

  /**
   * This implicit conversion method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
   *
   * <pre class="stHighlight">
   * javaMap should (contain key ("two"))
   * </pre>
   *
   * The <code>(contain key ("two"))</code> expression will result in a <code>Matcher[scala.collection.GenMap[String, Any]]</code>. This
   * implicit conversion method will convert that matcher to a <code>Matcher[java.util.Map[String, Any]]</code>.
   */
  implicit def convertMapMatcherToJavaMapMatcher[K, V](mapMatcher: Matcher[scala.collection.GenMap[K, V]]): Matcher[java.util.Map[K, V]] =
    new Matcher[java.util.Map[K, V]] {
      def apply(left: java.util.Map[K, V]): MatchResult = 
        mapMatcher.apply(new JavaMapWrapper(left))
    }

  // Ack. The above conversion doesn't apply to java.util.Maps, because java.util.Map is not a subinterface
  // of java.util.Collection. But right now Matcher[Traversable] supports only "contain" and "have size"
  // syntax, and thus that should work on Java maps too, why not. Well I'll tell you why not. It is too complicated.
  // Since java Map is not a java Collection, I'll say the contain syntax doesn't work on it. But you can say
  // have key.

// The getLength and getSize field conversions seem inconsistent with
// what I do in symbol HavePropertyMatchers. It isn't, though because the difference is here
// it's a Scala field and there a Java field: a val getLength is a 
// perfectly valid Scala way to get a JavaBean property Java method in the bytecodes.

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
      }
  }

  /**
   * This implicit conversion method converts a <code>Symbol</code> to a
   * <code>HavePropertyMatcherGenerator</code>, to enable the symbol to be used with the <code>have ('author ("Dickens"))</code> syntax.
   */
  implicit def convertSymbolToHavePropertyMatcherGenerator(symbol: Symbol): HavePropertyMatcherGenerator = new HavePropertyMatcherGenerator(symbol)

  //
  // This class is used as the return type of the overloaded should method (in TraversableShouldWrapper) 
  // that takes a HaveWord. It's size method will be called in situations like this:
  //
  // list should have size 1
  //
  // This gets changed to :
  //
  // convertToTraversableShouldWrapper(list).should(have).size(1)
  //
  // Thus, the list is wrapped in a convertToTraversableShouldWrapper call via an implicit conversion, which results in
  // a TraversableShouldWrapper. This has a should method that takes a HaveWord. That method returns a
  // ResultOfHaveWordForTraverablePassedToShould that remembers the map to the left of should. Then this class
  // has a size method that takes a T type, type parameter of the Traversable. It does the assertion thing.
  // 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfHaveWordForTraversable[T](left: GenTraversable[T], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should have size (10)
     *                        ^
     * </pre>
     */
    def size(expectedSize: Int) {
      val leftSize = left.size
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfHaveWordForJavaCollection[E, L[_] <: java.util.Collection[_]](left: L[E], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should have size (10)
     *                       ^
     * </pre>
     */
    def size(expectedSize: Int) {
      val leftSize = left.size
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForJavaMap(left: java.util.Map[_, _], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should have size (10)
     *                     ^
     * </pre>
     */
    def size(expectedSize: Int) {
      val leftSize = left.size
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForSeq[T](left: GenSeq[T], shouldBeTrue: Boolean) extends ResultOfHaveWordForTraversable[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * seq should have length (20)
     *                 ^
     * </pre>
     */
    def length(expectedLength: Int) {
      val leftLength = left.length
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
class ResultOfHaveWordForArray[T](left: Array[T], shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * array should have size (10)
     *                   ^
     * </pre>
     */
    def size(expectedSize: Int) {
      val leftSize = left.size
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * array should have length (20)
     *                   ^
     * </pre>
     */
    def length(expectedLength: Int) {
      val leftLength = left.length
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForTraversable[E, T[_] <: GenTraversable[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * iterable should not contain ("one")
     *                     ^
     * </pre>
     */
    def contain(expectedElement: E)(implicit holder: Holder[T[E]]) {
      val right = expectedElement
      if (holder.containsElement(left, right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not contain containMatcher
     *                       ^
     * </pre>
     */
    def contain(right: ContainMatcher[E]) {
      val result = right(left.asInstanceOf[GenTraversable[E]])
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>positiveNumber</code> refers to
     * an <code>AMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * collection should not contain a (positiveNumber)
     *                       ^
     * </pre>
     */
    def contain(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[E]) {
      val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
      left.find (e => aMatcher(e.asInstanceOf[E]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e.asInstanceOf[E])
            throw newTestFailedException(FailureMessages("containedA", left, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", left, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax, where <code>positiveNumber</code> refers to
     * an <code>AnMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * collection should not contain a (positiveNumber)
     *                       ^
     * </pre>
     */
    def contain(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[E]) {
      val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
      left.find (e => anMatcher(e.asInstanceOf[E]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e.asInstanceOf[E])
            throw newTestFailedException(FailureMessages("containedAn", left, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", left, UnquotedString(anMatcher.nounName)))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not have size (3)
     *                       ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      val leftSize = left.size
      if ((leftSize == right) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, right)
          else
            FailureMessages("hadExpectedSize", left, right)
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForJavaCollection[E, T[_] <: java.util.Collection[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should not have size (3)
     *                           ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      val leftSize = left.size
      if ((leftSize == right) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, right)
          else
            FailureMessages("hadExpectedSize", left, right)
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should not contain ("elephant")
     *                           ^
     * </pre>
     */
    def contain(expectedElement: E)(implicit holder: Holder[T[E]]) {
      val right = expectedElement
      // if ((left.contains(right)) != shouldBeTrue) {
      if (holder.containsElement(left, right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaCollection should not contain containMatcher
     *                           ^
     * </pre>
     */
    def contain(right: ContainMatcher[E]) {
      val result = right(new JavaCollectionWrapper(left.asInstanceOf[java.util.Collection[E]]))
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>positiveNumber</code> refers to
     * an <code>AMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * javaCol should not contain a (positiveNumber)
     *                    ^
     * </pre>
     */
    def contain(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[E]) {
      val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
      val leftWrapper = new JavaCollectionWrapper(left.asInstanceOf[java.util.Collection[E]])
      leftWrapper.find (e => aMatcher(e.asInstanceOf[E]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e.asInstanceOf[E])
            throw newTestFailedException(FailureMessages("containedA", left, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", left, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax, where <code>oddNumber</code> refers to
     * an <code>AnMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * javaCol should not contain an (oddNumber)
     *                    ^
     * </pre>
     */
    def contain(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[E]) {
      val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
      val leftWrapper = new JavaCollectionWrapper(left.asInstanceOf[java.util.Collection[E]])
      leftWrapper.find (e => anMatcher(e.asInstanceOf[E]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e.asInstanceOf[E])
            throw newTestFailedException(FailureMessages("containedAn", left, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", left, UnquotedString(anMatcher.nounName)))
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForMap[K, V, L[_, _] <: scala.collection.GenMap[_, _]](left: L[K, V], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not contain key ("three")
     *                ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      val right = resultOfKeyWordApplication.expectedKey
      if ((left.asInstanceOf[GenMap[K, V]].exists(_._1 == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -> 1, "two" -> 2) should not contain value (3)
     *                                        ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      val right = resultOfValueWordApplication.expectedValue
      if ((left.asInstanceOf[GenMap[K, V]].exists(_._2 == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
              left,
              right
            )
          )
      }
    }

    // TODO: Had to pull these methods out of ReusltOfNotWordForTraversable, because can't exent
    // it without losing precision on the inferred types. Map[String, Int] becomes GenIterable[(Any, Any)]
    // So the wrong Equality type class was chosen. By going around ResultOfNotWordForTraversable, I can
    // get the precise Map type up to ResultOfNotWord's equal method, which requires the Equality type class.

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * iterable should not contain ("one")
     *                     ^
     * </pre>
     */
    def contain(expectedElement: (K, V)) {
      val right = expectedElement
      if ((left.exists(_ == right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not contain containMatcher
     *                ^
     * </pre>
     */
    def contain(right: ContainMatcher[(K, V)]) {
      val result = right(left.asInstanceOf[scala.collection.GenTraversable[(K, V)]])
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>positiveNumberKey</code> refers to
     * an <code>AMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * map should not contain a (positiveNumberKey)
     *                ^
     * </pre>
     */
    def contain(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[(K, V)]) {
      val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
      left.find (e => aMatcher(e.asInstanceOf[(K, V)]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e.asInstanceOf[(K, V)])
            throw newTestFailedException(FailureMessages("containedA", left, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", left, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax, where <code>oddNumberKey</code> refers to
     * an <code>AnMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * map should not contain a (oddNumberKey)
     *                ^
     * </pre>
     */
    def contain(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[(K, V)]) {
      val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
      left.find (e => anMatcher(e.asInstanceOf[(K, V)]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e.asInstanceOf[(K, V)])
            throw newTestFailedException(FailureMessages("containedAn", left, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", left, UnquotedString(anMatcher.nounName)))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should not have size (3)
     *                       ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      val leftSize = left.size
      if ((leftSize == right) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, right)
          else
            FailureMessages("hadExpectedSize", left, right)
        )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForJavaMap[K, V, L[_, _] <: java.util.Map[_, _]](left: L[K, V], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should not contain key ("three")
     *                    ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      val right = resultOfKeyWordApplication.expectedKey
      if ((left.containsKey(right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainKey" else "containedKey",
              left,
              right
            )
          )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should not contain value (3)
     *                            ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      val right = resultOfValueWordApplication.expectedValue
      if ((left.containsValue(right)) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainValue" else "containedValue",
              left,
              right
            )
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaMap should not contain containMatcher
     *                    ^
     * </pre>
     */
    def contain(right: ContainMatcher[(K, V)]) {
      val result = right(new JavaMapWrapper(left.asInstanceOf[java.util.Map[K, V]]))
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>positiveNumber</code> refers to
     * an <code>AMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * javaMap should not contain a (positiveNumber)
     *                    ^
     * </pre>
     */
    def contain(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[(K, V)]) {
      val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
      val leftWrapper = new JavaMapWrapper(left.asInstanceOf[java.util.Map[K, V]])
      leftWrapper.find (e => aMatcher(e.asInstanceOf[(K, V)]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e.asInstanceOf[(K, V)])
            throw newTestFailedException(FailureMessages("containedA", leftWrapper, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", leftWrapper, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax, where <code>oddNumber</code> refers to
     * an <code>AnMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * javaMap should not contain an (oddNumber)
     *                    ^
     * </pre>
     */
    def contain(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[(K, V)]) {
      val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
      val leftWrapper = new JavaMapWrapper(left.asInstanceOf[java.util.Map[K, V]])
      leftWrapper.find (e => anMatcher(e.asInstanceOf[(K, V)]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e.asInstanceOf[(K, V)])
            throw newTestFailedException(FailureMessages("containedAn", leftWrapper, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", leftWrapper, UnquotedString(anMatcher.nounName)))
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForSeq[E, T[_] <: GenSeq[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForTraversable[E, T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * List(1, 2) should not have length (12)
     *                       ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      val leftLength = left.length
      if ((leftLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
            else
              FailureMessages("hadExpectedLength", left, right)
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForArray[E](left: Array[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array("two", "three") should not contain ("one")
     *                                  ^
     * </pre>
     */
    def contain(expectedElement: E)(implicit holder: Holder[Array[E]]) {
      val right = expectedElement
      // if ((left.exists(_ == right)) != shouldBeTrue) {
      if (holder.containsElement(left, right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>containMatcher</code> refers to
     * a <code>ContainMatcher</code>:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should not contain containMatcher
     *                           ^
     * </pre>
     */
    def contain(right: ContainMatcher[E]) {
      val result = right(new ArrayWrapper(left))
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>positiveNumber</code> refers to
     * an <code>AMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * Array(-1, -2) should not contain a (positiveNumber)
     *                          ^
     * </pre>
     */
    def contain(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[E]) {
      val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
      left.find (e => aMatcher(e.asInstanceOf[E]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e.asInstanceOf[E])
            throw newTestFailedException(FailureMessages("containedA", left, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", left, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax, where <code>oddNumber</code> refers to
     * an <code>AnMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * Array(-1, -2) should not contain an (oddNumber)
     *                          ^
     * </pre>
     */
    def contain(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[E]) {
      val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
      left.find (e => anMatcher(e.asInstanceOf[E]).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e.asInstanceOf[E])
            throw newTestFailedException(FailureMessages("containedAn", left, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", left, UnquotedString(anMatcher.nounName)))
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should not have size (3)
     *                        ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      val right = resultOfSizeWordApplication.expectedSize
      val leftSize = left.size
      if ((leftSize == right) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, right)
          else
            FailureMessages("hadExpectedSize", left, right)
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should not have length (12)
     *                        ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      val leftLength = left.length
      if ((leftLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
            else
              FailureMessages("hadExpectedLength", left, right)

          )
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForJavaList[E, L[_] <: java.util.List[_]](left: L[E], shouldBeTrue: Boolean) extends ResultOfHaveWordForJavaCollection[E, L](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaList should have length (12)
     *                      ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def length(expectedLength: Int) {
      val leftLength = left.size
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue) 
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForJavaList[E, T[_] <: java.util.List[_]](left: T[E], shouldBeTrue: Boolean)
      extends ResultOfNotWordForJavaCollection[E, T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * javaList should not have length (12)
     *                     ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      val leftLength = left.size
      if ((leftLength == right) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
          else
            FailureMessages("hadExpectedLength", left, right)
        )
      }
    }
  }
  
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
     *               ^
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
     * This method enables the following syntax (positiveNumber is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * 1 should be an oddNumber
     *                ^
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
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfBeWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean) extends ResultOfBeWordForAny(left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *                  ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef) {
      if ((left eq right) != shouldBeTrue)
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
            FailureMessages("wasNotAnInstanceOf", left, UnquotedString(clazz.getName))
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
    def a(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, true)
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
    def a(bePropertyMatcher: BePropertyMatcher[T]) {
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
    def an(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, true, false)
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
    def an(beTrueMatcher: BePropertyMatcher[T]) {
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
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWord[T](left: T, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not equal (7)
     *                   ^
     * </pre>
     */
    def equal(right: Any)(implicit equality: Equality[T]) {
      if (equality.areEqual(left, right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
           if (shouldBeTrue) "didNotEqual" else "equaled",
            left,
            right
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be (7)
     *                   ^
     * </pre>
     */
    def be(right: Any) {
      if ((left == right) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
           if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
            left,
            right
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be &lt;= (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotLessThanOrEqualTo" else "wasLessThanOrEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be &gt;= (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotGreaterThanOrEqualTo" else "wasGreaterThanOrEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be &lt; (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotLessThan" else "wasLessThan",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be &gt; (7)
     *                   ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanComparison[T]) {
      if (comparison(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotGreaterThan" else "wasGreaterThan",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should not be === (7)
     *                   ^
     * </pre>
     */
    def be(comparison: TripleEqualsInvocation[_]) {
      if ((left == comparison.right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotEqualTo" else "wasEqualTo",
            left,
            comparison.right
          )
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>odd</code> refers to
     * a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * 2 should not be (odd)
     *              ^
     * </pre>
     */
    def be(beMatcher: BeMatcher[T]) {
      val result = beMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            result.failureMessage
          else
            result.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>positiveNumber</code> refers to
     * an <code>AMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * 2 should not be a (positiveNumber)
     *              ^
     * </pre>
     */
    def be(resultOfAWordToAMatcherApplication: ResultOfAWordToAMatcherApplication[T]) {
      val aMatcher = resultOfAWordToAMatcherApplication.aMatcher
      val result = aMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            result.failureMessage
          else
            result.negatedFailureMessage
        )
      }
    }
    
    /**
     * This method enables the following syntax, where <code>oddNumber</code> refers to
     * an <code>AnMatcher[Int]</code>:
     *
     * <pre class="stHighlight">
     * 2 should not be an (oddNumber)
     *              ^
     * </pre>
     */
    def be(resultOfAnWordToAnMatcherApplication: ResultOfAnWordToAnMatcherApplication[T]) {
      val anMatcher = resultOfAnWordToAnMatcherApplication.anMatcher
      val result = anMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            result.failureMessage
          else
            result.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should not be (6.5 +- 0.2)
     *                       ^
     * </pre>
     */
    def be(interval: Interval[T]) {
      if (interval.isWithin(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotPlusOrMinus" else "wasPlusOrMinus",
            left,
            interval.pivot,
            interval.tolerance
          )
        )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * sevenDotOh should not equal (6.5 +- 0.2)
     *                       ^
     * </pre>
     */
    def equal(interval: Interval[T]) {
      if (interval.isWithin(left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEqualPlusOrMinus" else "equaledPlusOrMinus",
            left,
            interval.pivot,
            interval.tolerance
          )
        )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * result should not equal null
     *                   ^
     * </pre>
     */
    def equal(right: Null) {
      if ((left == null) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEqualNull" else "equaledNull",
            left
          )
        )
      }
    }

/*
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit len: Length[T]) {
      val right = resultOfLengthWordApplication.expectedLength
      val leftLength = len.extentOf(left)
      if ((leftLength == right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
            else
              FailureMessages("hadExpectedLength", left, right)
          )
        )
      }
    }
*/
    // TODO: Explain this matrix somewhere
    // The type parameter U has T as its lower bound, which means that U must be T or a supertype of T. Left is T, oh, because
    // HavePropertyMatcher is contravariant in its type parameter T, and that nmakes sense, because a HavePropertyMatcher of Any should
    // be able to match on a String.
    // <code>not have (a (1), b (2))</code> must mean the opposite of <code>have (a (1), b (2))</code>, which means that 
    // <code>not have (a (1), b (2))</code> will be true if either <code>(a (1)).matches</code> or <code>(b (1)).matches</code> is false.
    // Only if both <code>(a (1)).matches</code> or <code>(b (1)).matches</code> are true will <code>not have (a (1), b (2))</code> be false.
    // title/author matches | have | have not
    // 0 0 | 0 | 1 
    // 0 1 | 0 | 1
    // 1 0 | 0 | 1
    // 1 1 | 1 | 0
    // 
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * book should not have (title ("One Hundred Years of Solitude"))
     *                 ^
     * </pre>
     */
    //def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {
    def have(firstPropertyMatcher: HavePropertyMatcher[T, _], propertyMatchers: HavePropertyMatcher[T, _]*) {

      val results =
        for (propertyVerifier <- firstPropertyMatcher :: propertyMatchers.toList) yield
          propertyVerifier(left)

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
                 left
              )
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
                  left
                )
              }
              else FailureMessages("allPropertiesHadExpectedValues", left)

            throw newTestFailedException(failureMessage)
        } 
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  sealed class ResultOfNotWordForAnyRef[T <: AnyRef](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWord[T](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should not be (null)
     *                ^
     * </pre>
     */
    def be(o: Null) {
      if ((left == null) != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotNull", left) 
          else
            FailureMessages("wasNull")
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * stack should not be ('empty)
     *                  ^
     * </pre>
     */
    def be(symbol: Symbol) {
      val matcherResult = matchSymbolToPredicateMethod(left, symbol, false, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>stack</code> is, for example, of type <code>Stack</code> and
     * <code>empty</code> refers to a <code>BePropertyMatcher[Stack]</code>:
     *
     * <pre class="stHighlight">
     * stack should not be (empty)
     *                      ^
     * </pre>
     */
    def be(bePropertyMatcher: BePropertyMatcher[T]) {
      val result = bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNot", left, UnquotedString(result.propertyName))
          else
            FailureMessages("was", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notFileMock should not be a ('file)
     *                        ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      val matcherResult = matchSymbolToPredicateMethod(left, resultOfAWordApplication.symbol, true, true)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>notFileMock</code> is, for example, of type <code>File</code> and
     * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
     *
     * <pre class="stHighlight">
     * notFileMock should not be a (file)
     *                        ^
     * </pre>
     */
    def be[U >: T](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]) {
      val result = resultOfAWordApplication.bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotA", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasA", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * keyEvent should not be an ('actionKey)
     *                     ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      val matcherResult = matchSymbolToPredicateMethod(left, resultOfAnWordApplication.symbol, true, false)
      if (matcherResult.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage
        )
      }
    }

    /**
     * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
     * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
     *
     * <pre class="stHighlight">
     * keyEvent should not be an (actionKey)
     *                     ^
     * </pre>
     */
    def be[U >: T](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]) {
      val result = resultOfAnWordApplication.bePropertyMatcher(left)
      if (result.matches != shouldBeTrue) {
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("wasNotAn", left, UnquotedString(result.propertyName))
          else
            FailureMessages("wasAn", left, UnquotedString(result.propertyName))
        )
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * otherString should not be theSameInstanceAs (string)
     *                        ^
     * </pre>
     */
    def be(resultOfSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication) {
      if ((resultOfSameInstanceAsApplication.right eq left) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "wasNotSameInstanceAs" else "wasSameInstanceAs",
            left,
            resultOfSameInstanceAsApplication.right
          )
        )
      }
    }

  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForString(left: String, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef[String](left, shouldBeTrue) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should not have length (12)
     *                   ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      val leftLength = left.length
      if ((leftLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
            else
              FailureMessages("hadExpectedLength", left, right)
          )
      }
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                   ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
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
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not include regex ("wo.ld")
     *                   ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
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
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not include ("world")
     *                   ^
     * </pre>
     */
    def include(expectedSubstring: String) {
      if ((left.indexOf(expectedSubstring) >= 0) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotIncludeSubstring" else "includedSubstring",
            left,
            expectedSubstring
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should not startWith regex ("Hel*o")
     *                   ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
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
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not startWith ("1.7")
     *                    ^
     * </pre>
     */
    def startWith(expectedSubstring: String) {
      if ((left.indexOf(expectedSubstring) == 0) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotStartWith" else "startedWith",
            left,
            expectedSubstring
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * greeting should not endWith regex ("wor.d")
     *                     ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      val rightRegex = resultOfRegexWordApplication.regex
      val allMatches = rightRegex.findAllIn(left)
      if (allMatches.hasNext && (allMatches.end == left.length) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
            left,
            rightRegex
          )
        )
    }

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not endWith ("1.7")
     *                    ^
     * </pre>
     */
    def endWith(expectedSubstring: String) {
      if ((left endsWith expectedSubstring) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotEndWith" else "endedWith",
            left,
            expectedSubstring
          )
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * string should not contain ('c')
     *                   ^
     * </pre>
     */
    def contain(expectedElement: Char)(implicit holder: Holder[String]) {
      val right = expectedElement
      if (holder.containsElement(left, right) != shouldBeTrue) {
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainExpectedElement" else "containedExpectedElement",
              left,
              right
            )
          )
      }
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForNumeric[T : Numeric](left: T, shouldBeTrue: Boolean)
      extends ResultOfNotWord[T](left, shouldBeTrue) {

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
    def apply(regexString: String): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regexString)

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "eight" should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *                                     ^
     * </pre>
     */
    def apply(regex: Regex): ResultOfRegexWordApplication = new ResultOfRegexWordApplication(regex)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForString(left: String, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * string should have length (12)
     *                    ^
     * </pre>
     */
    def length(expectedLength: Int) {
      val leftLength = left.length
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }
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
  def equal[T](interval: Interval[T]): Matcher[T] = {
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        MatchResult(
          interval.isWithin(left),
          FailureMessages("didNotEqualPlusOrMinus", left, interval.pivot, interval.tolerance),
          FailureMessages("equaledPlusOrMinus", left, interval.pivot, interval.tolerance)
        )
      }
    }
  }

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * result should equal (null)
   *        ^
   * </pre>
   */
  def equal(o: Null): Matcher[AnyRef] = 
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left == null,
          FailureMessages("didNotEqualNull", left),
          FailureMessages("equaledNull"),
          FailureMessages("didNotEqualNull", left),
          FailureMessages("midSentenceEqualedNull")
        )
      }
    }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class LengthWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * "hi" should not have length (3)
     *                             ^
     * </pre>
     */
    def apply(expectedLength: Long): ResultOfLengthWordApplication = new ResultOfLengthWordApplication(expectedLength)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "hi" should not have length (3)
   *                      ^
   * </pre>
   */
  val length = new LengthWord
 
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class SizeWord {

    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * set should not have size (3)
     *                          ^
     * </pre>
     */
    def apply(expectedSize: Long): ResultOfSizeWordApplication = new ResultOfSizeWordApplication(expectedSize)
  }

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should not have size (3)
   *                     ^
   * </pre>
   */
  val size = new SizeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfElementWordApplication[T](val expectedElement: T)

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
    def apply[T](expectedKey: T): ResultOfKeyWordApplication[T] = new ResultOfKeyWordApplication(expectedKey)
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
     * map should not contain value (10)
     *                              ^
     * </pre>
     */
    def apply[T](expectedValue: T): ResultOfValueWordApplication[T] = new ResultOfValueWordApplication(expectedValue)
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
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "eight" should not include substring ("seven")
   *                            ^
   * </pre>
  val substring = new SubstringWord
   */

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForSize[A <: AnyRef : Size](left: A, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

/*  I just added this whole thing in here for completeness when doing SizeShouldWrapper. Write some tests to prove it is needed.
// TODO: This should be for "sizey should not have size (12)" Try that test.
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      val leftLength = left.length
      if ((leftLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
            else
              FailureMessages("hadExpectedLength", left, right)
          )
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfNotWordForLength[A <: AnyRef : Length](left: A, shouldBeTrue: Boolean)
      extends ResultOfNotWordForAnyRef(left, shouldBeTrue) {

/* TODO What's going on? Why can I drop this and still get a compile
// TODO: This should be for "lengthy should not have length (12)" Try that test.
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      val right = resultOfLengthWordApplication.expectedLength
      val leftLength = left.length
      if ((leftLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, right)
            else
              FailureMessages("hadExpectedLength", left, right)
          )
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForExtent[A : Extent](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Int)(implicit len: Length[A]) {
      // val len = implicitly[Length[A]]
      // if ((len.extentOf(left.asInstanceOf[A]) == expectedLength) != shouldBeTrue)
      val leftLength = len.extentOf(left)
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2L)
     *                      ^
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
      // val len = implicitly[Length[A]]
      // if ((len.extentOf(left.asInstanceOf[A]) == expectedLength) != shouldBeTrue)
      val leftLength = len.extentOf(left)
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue) 
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Int)(implicit sz: Size[A]) {
      // val sz = implicitly[Size[T]]
      // if ((sz.extentOf(left.asInstanceOf[T]) == expectedSize) != shouldBeTrue)
      val leftSize = sz.extentOf(left)
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
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
      // val sz = implicitly[Size[T]]
      // if ((sz.extentOf(left.asInstanceOf[T]) == expectedSize) != shouldBeTrue)
      val leftSize = sz.extentOf(left)
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
        )
    }
  }

/*
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForLength[A : Length](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Int) {
      val len = implicitly[Length[A]]
      val leftLength = len.extentOf(left)
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have length (2L)
     *                      ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>length</code> property structure
     * of type <code>Long</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>scala.Seq</code>, <code>java.lang.String</code>, and <code>java.util.List</code>.
     * </p>
     */
    def length(expectedLength: Long) {
      val len = implicitly[Length[A]]
      val leftLength = len.extentOf(left)
      if ((leftLength == expectedLength) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadLengthInsteadOfExpectedLength", left, leftLength, expectedLength)
          else
            FailureMessages("hadExpectedLength", left, expectedLength)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfHaveWordForSize[A : Size](left: A, shouldBeTrue: Boolean) {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should have size (2)
     *                 ^
     * </pre>
     *
     * <p>
     * This method is ultimately invoked for objects that have a <code>size</code> property structure
     * of type <code>Int</code>,
     * but is of a type that is not handled by implicit conversions from nominal types such as
     * <code>Traversable</code> and <code>java.util.Collection</code>.
     * </p>
     */
    def size(expectedSize: Int) {
      val sz = implicitly[Size[A]]
      val leftSize = sz.extentOf(left)
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
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
    def size(expectedSize: Long) {
      val sz = implicitly[Size[A]]
      val leftSize = sz.extentOf(left)
      if ((leftSize == expectedSize) != shouldBeTrue)
        throw newTestFailedException(
          if (shouldBeTrue)
            FailureMessages("hadSizeInsteadOfExpectedSize", left, leftSize, expectedSize)
          else
            FailureMessages("hadExpectedSize", left, expectedSize)
        )
    }
  }
*/

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be < (10) and not be > (17))
   *                    ^
   * </pre>
   */
  def <[T <% Ordered[T]] (right: T): ResultOfLessThanComparison[T] =
    new ResultOfLessThanComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be > (10) and not be < (7))
   *                    ^
   * </pre>
   */
  def >[T <% Ordered[T]] (right: T): ResultOfGreaterThanComparison[T] =
    new ResultOfGreaterThanComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be <= (10) and not be > (17))
   *                    ^
   * </pre>
   */
  def <=[T <% Ordered[T]] (right: T): ResultOfLessThanOrEqualToComparison[T] =
    new ResultOfLessThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * num should (not be >= (10) and not be < (7))
   *                    ^
   * </pre>
   */
  def >=[T <% Ordered[T]] (right: T): ResultOfGreaterThanOrEqualToComparison[T] =
    new ResultOfGreaterThanOrEqualToComparison(right)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * num should not be === (10)
   *                   ^
   * </pre>
   */
/* TODEL
  def === (right: Any): ResultOfTripleEqualsApplication =
    new ResultOfTripleEqualsApplication(right)
*/

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
  }

  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   * ^
   * </pre>
   */
  def evaluating(fun: => Any): ResultOfEvaluatingApplication =
    new ResultOfEvaluatingApplication(fun _)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfProduceInvocation[T](val clazz: Class[T])

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
   * ^
   * </pre>
   */
  def produce[T](implicit manifest: Manifest[T]): ResultOfProduceInvocation[T] =
    new ResultOfProduceInvocation(manifest.erasure.asInstanceOf[Class[T]])

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForTraversable[T](left: GenTraversable[T], shouldBeTrue: Boolean = true) {
  
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain theSameElementsAs anotherTraversable
     *                            ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      matchContainMatcher(left, new TheSameElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain theSameElementsAs array
     *                            ^
     * </pre>
     */
    def theSameElementsAs(right: Array[T])(implicit equality: Equality[T]) {
      matchContainMatcher(left, new TheSameElementsAsContainMatcher(new ArrayWrapper(right), equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain theSameIteratedElementsAs anotherTraversable
     *                            ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      matchContainMatcher(left, new TheSameIteratedElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain theSameIteratedElementsAs array
     *                            ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: Array[T])(implicit equality: Equality[T]) {
      matchContainMatcher(left, new TheSameIteratedElementsAsContainMatcher(new ArrayWrapper(right), equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain allOf (1, 2)
     *                            ^
     * </pre>
     */
    def allOf(right: T*)(implicit equality: Equality[T]) {
      matchContainMatcher(left, new AllOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain inOrder (1, 2)
     *                            ^
     * </pre>
     */
    def inOrder(right: T*)(implicit equality: Equality[T]) {
      matchContainMatcher(left, new InOrderContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain oneOf (1, 2)
     *                            ^
     * </pre>
     */
    def oneOf(right: T*)(implicit equality: Equality[T]) {
      matchContainMatcher(left, new OneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain only (1, 2)
     *                            ^
     * </pre>
     */
    def only(right: T*)(implicit equality: Equality[T]) {
      matchContainMatcher(left, new OnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain inOrderOnly (1, 2)
     *                            ^
     * </pre>
     */
    def inOrderOnly(right: T*)(implicit equality: Equality[T]) {
      matchContainMatcher(left, new InOrderOnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * traversable should contain noneOf (1, 2)
     *                            ^
     * </pre>
     */
    def noneOf(right: T*)(implicit equality: Equality[T]) {
      matchContainMatcher(left, new NoneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax (positiveNumber is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * traversable should contain a positiveNumber
     *                            ^
     * </pre>
     */
    def a(aMatcher: AMatcher[T]) {
      left.find(aMatcher(_).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e)
            throw newTestFailedException(FailureMessages("containedA", left, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", left, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax (oddNumber is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * traversable should contain an oddNumber
     *                            ^
     * </pre>
     */
    def an(anMatcher: AnMatcher[T]) {
      left.find(anMatcher(_).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e)
            throw newTestFailedException(FailureMessages("containedAn", left, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", left, UnquotedString(anMatcher.nounName)))
      }
    }
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class ResultOfContainWordForJavaCollection[E, L[_] <: java.util.Collection[_]](left: L[E], shouldBeTrue: Boolean) {
    // TODO: Chee Seng, why are we casting here to java.util.Collection[E]?
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain theSameElementsAs traversable
     *                            ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[E])(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new TheSameElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain theSameIteratedElementsAs anotherTraversable
     *                        ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[E])(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new TheSameIteratedElementsAsContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain allOf (1, 2)
     *                        ^
     * </pre>
     */
    def allOf(right: E*)(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new AllOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain inOrder (1, 2)
     *                        ^
     * </pre>
     */
    def inOrder(right: E*)(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new InOrderContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain oneOf (1, 2)
     *                        ^
     * </pre>
     */
    def oneOf(right: E*)(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new OneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain only (1, 2)
     *                        ^
     * </pre>
     */
    def only(right: E*)(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new OnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain inOrderOnly (1, 2)
     *                        ^
     * </pre>
     */
    def inOrderOnly(right: E*)(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new InOrderOnlyContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * javaCol should contain noneOf (1, 2)
     *                        ^
     * </pre>
     */
    def noneOf(right: E*)(implicit equality: Equality[E]) {
      matchContainMatcher(left.asInstanceOf[java.util.Collection[E]], new NoneOfContainMatcher(right, equality), shouldBeTrue)
    }
    
    /**
     * This method enables the following syntax (positiveNumber is a <code>AMatcher</code>):
     *
     * <pre class="stHighlight">
     * javaCol should contain a positiveNumber
     *                        ^
     * </pre>
     */
    def a(aMatcher: AMatcher[E]) {
      val leftWrapper = new JavaCollectionWrapper(left.asInstanceOf[java.util.Collection[E]])
      leftWrapper.find(e => aMatcher(e).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = aMatcher(e)
            throw newTestFailedException(FailureMessages("containedA", leftWrapper, UnquotedString(aMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainA", leftWrapper, UnquotedString(aMatcher.nounName)))
      }
    }
    
    /**
     * This method enables the following syntax (oddNumber is a <code>AnMatcher</code>):
     *
     * <pre class="stHighlight">
     * javaCol should contain an oddNumber
     *                        ^
     * </pre>
     */
    def an(anMatcher: AnMatcher[E]) {
      val leftWrapper = new JavaCollectionWrapper(left.asInstanceOf[java.util.Collection[E]])
      leftWrapper.find(e => anMatcher(e).matches) match {
        case Some(e) => 
          if (!shouldBeTrue) {
            val result = anMatcher(e)
            throw newTestFailedException(FailureMessages("containedAn", leftWrapper, UnquotedString(anMatcher.nounName), UnquotedString(result.negatedFailureMessage)))
          }
        case None =>
          if (shouldBeTrue)
            throw newTestFailedException(FailureMessages("didNotContainAn", leftWrapper, UnquotedString(anMatcher.nounName)))
      }
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * traversable should contain (theSameElementsAs(anotherTraversable))
   *                             ^
   * </pre>
   */
  def theSameElementsAs[T](xs: GenTraversable[T])(implicit equality: Equality[T]) = 
    new TheSameElementsAsContainMatcher(xs, equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * traversable should contain (theSameElementsAs(array))
   *                             ^
   * </pre>
   */
  def theSameElementsAs[T](xs: Array[T])(implicit equality: Equality[T]) = 
    new TheSameElementsAsContainMatcher(new ArrayWrapper(xs), equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * traversable should contain (theSameIteratedElementsAs(anotherTraversable))
   *                             ^
   * </pre>
   */
  def theSameIteratedElementsAs[T](xs: GenTraversable[T])(implicit equality: Equality[T]) = 
    new TheSameIteratedElementsAsContainMatcher(xs, equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * traversable should contain (theSameIteratedElementsAs(array))
   *                             ^
   * </pre>
   */
  def theSameIteratedElementsAs[T](xs: Array[T])(implicit equality: Equality[T]) = 
    new TheSameIteratedElementsAsContainMatcher(new ArrayWrapper(xs), equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (allOf(1, 2))
   *                               ^
   * </pre>
   */
  def allOf[T](xs: T*)(implicit equality: Equality[T]) = 
    new AllOfContainMatcher(xs, equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrder(1, 2))
   *                               ^
   * </pre>
   */
  def inOrder[T](xs: T*)(implicit equality: Equality[T]) = 
    new InOrderContainMatcher(xs, equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (oneOf(1, 2))
   *                               ^
   * </pre>
   */
  def oneOf[T](xs: T*)(implicit equality: Equality[T]) = 
    new OneOfContainMatcher(xs, equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (only(1, 2))
   *                               ^
   * </pre>
   */
  def only[T](xs: T*)(implicit equality: Equality[T]) = 
    new OnlyContainMatcher(xs, equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (inOrderOnly(1, 2))
   *                               ^
   * </pre>
   */
  def inOrderOnly[T](xs: T*)(implicit equality: Equality[T]) = 
    new InOrderOnlyContainMatcher(xs, equality)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should contain (noneOf(1, 2))
   *                               ^
   * </pre>
   */
  def noneOf[T](xs: T*)(implicit equality: Equality[T]) = 
    new NoneOfContainMatcher(xs, equality)
  
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
  
  def doCollected[T](collected: Collected, xs: GenTraversable[T], methodName: String, stackDepth: Int)(fun: T => Unit) {
    collected match {
      case AllCollected =>
        doForAll(xs, "allShorthandFailed", "Matchers.scala", methodName, stackDepth) { e => 
          fun(e)
        }
      case AtLeastCollected(num) => 
        doForAtLeast(num, xs, "atLeastShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case EveryCollected => 
        doForEvery(xs, "everyShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case ExactlyCollected(num) => 
        doForExactly(num, xs, "exactlyShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case NoCollected =>
        doForNo(xs, "noShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case BetweenCollected(from, to) =>
        doForBetween(from, to, xs, "betweenShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
          fun(e)
        }
      case AtMostCollected(num) =>
        doForAtMost(num, xs, "atMostShorthandFailed", "Matchers.scala", methodName, stackDepth) { e =>
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
  sealed class ResultOfNotWordForCollectedAny[T](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) {

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
      doCollected(collected, xs, "equal", 1) { e =>
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
      doCollected(collected, xs, "be", 1) { e =>
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
     * all(xs) should not be <= (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanOrEqualToComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
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
     * all(xs) should not be >= (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanOrEqualToComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
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
     * all(xs) should not be < (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfLessThanComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
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
     * all(xs) should not be > (7)
     *                    ^
     * </pre>
     */
    def be(comparison: ResultOfGreaterThanComparison[T]) {
      doCollected(collected, xs, "be", 1) { e => 
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
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be === (7)
     *                    ^
     * </pre>
     */
    def be(comparison: TripleEqualsInvocation[_]) {
      doCollected(collected, xs, "be", 1) { e => 
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
      doCollected(collected, xs, "be", 1) { e => 
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
      doCollected(collected, xs, "be", 1) { e => 
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
      doCollected(collected, xs, "be", 1) { e => 
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
      doCollected(collected, xs, "be", 1) { e => 
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
      doCollected(collected, xs, "be", 1) { e => 
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
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>title ("One Hundred Years of Solitude")</code> results in a <code>HavePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should not have (title ("One Hundred Years of Solitude"))
     *                       ^
     * </pre>
     */
    def have[U >: T](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*) {
      doCollected(collected, xs, "have", 1) { e => 
      
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
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  sealed class ResultOfNotWordForCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
    extends ResultOfNotWordForCollectedAny(collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should not be (null)
     *                    ^
     * </pre>
     */
    def be(o: Null) {
      doCollected(collected, xs, "be", 1) { e => 
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
    def be(symbol: Symbol) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, false, false)
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
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, resultOfAWordApplication.symbol, true, true)
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
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e, resultOfAnWordApplication.symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
              None, 
              6
            )
        }
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
  final class ResultOfNotWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[String](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(string) should not have length (12)
     *                        ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        val eLength = e.length
        if ((eLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, right)
            else
              FailureMessages("hadExpectedLength", e, right), 
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
     * all(string) should not have size (12)
     *                        ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        val eSize = e.size
        if ((eSize == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, right)
            else
              FailureMessages("hadExpectedSize", e, right), 
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
    def startWith(right: String) {
      doCollected(collected, xs, "startWith", 1) { e =>
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
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "startWith", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.pattern.matcher(e).lookingAt != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
              e,
              rightRegex
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
     * all(string) should not endWith ("1.7")
     *                        ^
     * </pre>
     */
    def endWith(expectedSubstring: String) {
      doCollected(collected, xs, "endWith", 1) { e =>
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
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "endWith", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        val allMatches = rightRegex.findAllIn(e)
        if (allMatches.hasNext && (allMatches.end == e.length) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
              e,
              rightRegex
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
     * all(string) should not include regex ("wo.ld")
     *                        ^
     * </pre>
     *
     * <p>
     * The regular expression passed following the <code>regex</code> token can be either a <code>String</code>
     * or a <code>scala.util.matching.Regex</code>.
     * </p>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "include", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.findFirstIn(e).isDefined != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
              e,
              rightRegex
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
     * all(string) should not include ("world")
     *                        ^
     * </pre>
     */
    def include(expectedSubstring: String) {
      doCollected(collected, xs, "include", 1) { e =>
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
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication) {
      doCollected(collected, xs, "fullyMatch", 1) { e =>
        val rightRegex = resultOfRegexWordApplication.regex
        if (rightRegex.pattern.matcher(e).matches != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
              e,
              rightRegex
            ), 
            None, 
            6
          )
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
  sealed class ResultOfNotWordForCollectedGenTraversable[E, T <: GenTraversable[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfTraversable) should not have size (12)
     *                                          ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        val eSize = e.size
        if ((eSize == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, right)
            else
              FailureMessages("hadExpectedSize", e, right), 
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
     * all(traversableOfTraversable) should not have length (12)
     *                                          ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        val eLength = e.size
        if ((eLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, right)
            else
              FailureMessages("hadExpectedLength", e, right), 
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
     * all(traversableOfTraversable) should not contain ("one")
     *                                          ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.exists(_ == right)) != shouldBeTrue) {
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
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">testing
     * all(traversableOfTraversable) should not contain (containMatcher)
     *                                          ^
     * </pre>
     */
    def contain(right: ContainMatcher[E]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val result = right(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
            None, 
            6
          )
        }
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
  final class ResultOfNotWordForCollectedGenSeq[E, T <: GenSeq[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedGenTraversable[E, T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(seqOfSeq) should not have length (12)
     *                          ^
     * </pre>
     */
    override def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        val eLength = e.length
        if ((eLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, right)
            else
              FailureMessages("hadExpectedLength", e, right), 
            None, 
            6
          )
        }
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
  sealed class ResultOfNotWordForCollectedArray[E, T <: Array[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be ('empty)
     *                            ^
     * </pre>
     */
    override def be(symbol: Symbol) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, symbol, false, false)
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
     * all(colOfArray) should not be a ('file)
     *                            ^
     * </pre>
     */
    override def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, resultOfAWordApplication.symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            10
          )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not be an ('actionKey)
     *                            ^
     * </pre>
     */
    override def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication) {
      doCollected(collected, xs, "be", 1) { e => 
        val matcherResult = matchSymbolToPredicateMethod(e.deep, resultOfAnWordApplication.symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
              None, 
              10
            )
        }
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(traversableOfArray) should not have size (12)
     *                                    ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        val eSize = e.size
        if ((eSize == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, right)
            else
              FailureMessages("hadExpectedSize", e, right), 
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
     * all(traversableOfArray) should not contain ("one")
     *                                    ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.exists(_ == right)) != shouldBeTrue) {
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
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">testing
     * all(traversableOfArray) should not contain (containMatcher)
     *                                    ^
     * </pre>
     */
    def contain(right: ContainMatcher[E]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val result = right(e)
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage, 
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
     * all(seqOfArray) should not have length (12)
     *                            ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        val eLength = e.size
        if ((eLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) 
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, right)
            else
              FailureMessages("hadExpectedLength", e, right), 
            None, 
            6
          )
        }
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
  final class ResultOfNotWordForCollectedGenMap[K, V, T <: GenMap[K, V]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedGenTraversable[(K, V), T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not contain key ("three")
     *                          ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfKeyWordApplication.expectedKey
        if ((e.exists(_._1 == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
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
     * all(colOfMap) should not contain value (3)
     *                          ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfValueWordApplication.expectedValue
        if ((e.exists(_._2 == right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              right
            ), 
            None, 
            6
          )
        }
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
  class ResultOfNotWordForCollectedJavaCollection[E, T <: java.util.Collection[E]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCollection) should not have size (3)
     *                                     ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        val eSize = e.size
        if ((eSize == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, right)
            else
              FailureMessages("hadExpectedSize", e, right), 
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
     * all(colOfJavaCollection) should not have length (12)
     *                                     ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        val eLength = e.size
        if ((eLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, right)
            else
              FailureMessages("hadExpectedLength", e, right), 
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
     * all(colOfJavaCollection) should not contain ("elephant")
     *                                     ^
     * </pre>
     */
    def contain(expectedElement: E) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = expectedElement
        if ((e.contains(right)) != shouldBeTrue) {
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
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">testing
     * all(colOfJavaCollection) should not contain (containMatcher)
     *                                     ^
     * </pre>
     */
    def contain(right: ContainMatcher[E]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val result = right(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
        }
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
  final class ResultOfNotWordForCollectedJavaMap[K, V, T <: java.util.Map[K, V]](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) extends 
    ResultOfNotWordForCollectedAnyRef[T](collected, xs, shouldBeTrue){
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have size (3)
     *                              ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfSizeWordApplication.expectedSize
        val eSize = e.size
        if ((eSize == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, right)
            else
              FailureMessages("hadExpectedSize", e, right), 
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
     * all(colOfJavaMap) should not have length (12)
     *                              ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication) {
      doCollected(collected, xs, "have", 1) { e =>
        val right = resultOfLengthWordApplication.expectedLength
        val eLength = e.size
        if ((eLength == right) != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, right)
            else
              FailureMessages("hadExpectedLength", e, right), 
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
     * all(colOfJavaMap) should not contain key ("three")
     *                              ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication[K]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfKeyWordApplication.expectedKey
        if ((e.containsKey(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
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
     * all(colOfJavaMap) should not contain value (3)
     *                              ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication[V]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val right = resultOfValueWordApplication.expectedValue
        if ((e.containsValue(right)) != shouldBeTrue) {
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
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
     * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
     * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
     *
     * <pre class="stHighlight">testing
     * all(colOfJavaMap) should not contain (containMatcher)
     *                              ^
     * </pre>
     */
    def contain(right: ContainMatcher[(K, V)]) {
      doCollected(collected, xs, "contain", 1) { e =>
        val result = right(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
        }
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
  sealed class ResultOfBeWordForCollectedAny[T](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfBeWordForCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T], shouldBeTrue: Boolean) 
    extends ResultOfBeWordForCollectedAny(collected, xs, shouldBeTrue) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def theSameInstanceAs(right: AnyRef) {
      doCollected(collected, xs, "theSameInstanceAs", 1) { e =>
        if ((e eq right) != shouldBeTrue)
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
    def a(symbol: Symbol) {
      doCollected(collected, xs, "a", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, true, true)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            6
          )
        }
      }
    }
    
    // TODO, in both of these, the failure message doesn't have a/an
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(xs) should be an ('orange)
     *                   ^
     * </pre>
     */
    def an(symbol: Symbol) {
      doCollected(collected, xs, "an", 1) { e =>
        val matcherResult = matchSymbolToPredicateMethod(e, symbol, true, false)
        if (matcherResult.matches != shouldBeTrue) {
          throw newTestFailedException(
            if (shouldBeTrue) matcherResult.failureMessage else matcherResult.negatedFailureMessage, 
            None, 
            6
          )
        }
      }
    }
    
    // TODO: Check the shouldBeTrues, are they sometimes always false or true?
    /**
     * This method enables the following syntax, where <code>badBook</code> is, for example, of type <code>Book</code> and
     * <code>goodRead</code> refers to a <code>BePropertyMatcher[Book]</code>:
     *
     * <pre class="stHighlight">
     * all(books) should be a (goodRead)
     *                      ^
     * </pre>
     */
    def a(bePropertyMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "a", 1) { e =>
        val result = bePropertyMatcher(e)
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
    def an(beTrueMatcher: BePropertyMatcher[T]) {
      doCollected(collected, xs, "an", 1) { e =>
        val beTrueMatchResult = beTrueMatcher(e)
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
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfBeWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) 
    extends ResultOfBeWordForCollectedAnyRef(collected, xs, shouldBeTrue) {
  
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
    
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) {
  
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should contain (element)
     *                        ^
     * </pre>
     */
    def apply(expectedElement: T): Matcher[Array[T]] = 
      new Matcher[Array[T]] {
        def apply(left: Array[T]): MatchResult =
          MatchResult(
            left.exists(_ == expectedElement), 
            FailureMessages("didNotContainExpectedElement", left, expectedElement),
            FailureMessages("containedExpectedElement", left, expectedElement)
          )
      }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfArray) should contain theSameElementsAs List(1, 2, 3)
     *                                ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      val containMatcher = new TheSameElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameElementsAs", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfArray) should contain theSameIteratedElementsAs List(1, 2, 3)
     *                                ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      val containMatcher = new TheSameIteratedElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameIteratedElementsAs", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfArray) should contain allOf (1, 2, 3)
     *                                ^
     * </pre>
     */
    def allOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new AllOfContainMatcher(right, equality)
      doCollected(collected, xs, "allOf", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfArray) should contain inOrder (1, 2, 3)
     *                                ^
     * </pre>
     */
    def inOrder(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new InOrderContainMatcher(right, equality)
      doCollected(collected, xs, "inOrder", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfArray) should contain oneOf (1, 2, 3)
     *                                ^
     * </pre>
     */
    def oneOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new OneOfContainMatcher(right, equality)
      doCollected(collected, xs, "oneOf", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfArray) should contain only (1, 2, 3)
     *                                ^
     * </pre>
     */
    def only(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new OnlyContainMatcher(right, equality)
      doCollected(collected, xs, "only", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfArray) should contain inOrderOnly (1, 2, 3)
     *                                ^
     * </pre>
     */
    def inOrderOnly(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new InOrderOnlyContainMatcher(right, equality)
      doCollected(collected, xs, "inOrderOnly", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfArray) should contain noneOf (1, 2, 3)
     *                                ^
     * </pre>
     */
    def noneOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new NoneOfContainMatcher(right, equality)
      doCollected(collected, xs, "noneOf", 1) { e =>
        val result = containMatcher(e)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,  
            None, 
            6
          )
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
  sealed class ResultOfCollectedAny[T](collected: Collected, xs: GenTraversable[T]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be (3)
     *         ^
     * </pre>
     */
    def should(rightMatcher: Matcher[T]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
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
      doCollected(collected, xs, "shouldEqual", 1) { e =>
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
     * all(xs) should equal (3)
     *         ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[T, TYPECLASS1])(implicit typeClass1: TYPECLASS1[T]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
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
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAny[T](collected, xs, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedAny[T] = 
      new ResultOfNotWordForCollectedAny(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  class ResultOfCollectedAnyRef[T <: AnyRef](collected: Collected, xs: GenTraversable[T]) extends ResultOfCollectedAny[T](collected, xs) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    override def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef[T](collected, xs, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(xs) should not equal (3)
     *         ^
     * </pre>
     */
    override def should(notWord: NotWord): ResultOfNotWordForCollectedAnyRef[T] =
      new ResultOfNotWordForCollectedAnyRef(collected, xs, false)
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfCollectedString(collected: Collected, xs: GenTraversable[String]) extends ResultOfCollectedAnyRef(collected, xs) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should have length (3)
     *             ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedString = 
      new ResultOfHaveWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should not have length (3)
     *             ^
     * </pre>
     */
    override def should(notWord: NotWord): ResultOfNotWordForCollectedString = 
      new ResultOfNotWordForCollectedString(collected, xs, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should startWith regex ("Hel*o")
     *             ^
     * </pre>
     */
    def should(startWithWord: StartWithWord): ResultOfStartWithWordForCollectedString = 
      new ResultOfStartWithWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should endWith regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(endWithWord: EndWithWord): ResultOfEndWithWordForCollectedString = 
      new ResultOfEndWithWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should include regex ("wo.ld")
     *             ^
     * </pre>
     */
    def should(includeWord: IncludeWord): ResultOfIncludeWordForCollectedString = 
      new ResultOfIncludeWordForCollectedString(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(string) should fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
     *             ^
     * </pre>
     */
    def should(fullyMatchWord: FullyMatchWord): ResultOfFullyMatchWordForCollectedString = 
      new ResultOfFullyMatchWordForCollectedString(collected, xs, true)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should have length (12)
     *                         ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        val eLength = e.length
        if ((eLength == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, expectedLength)
            else
              FailureMessages("hadExpectedLength", e, expectedLength), 
            None, 
            6
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(string) should have size (12)
     *                         ^
     * </pre>
     */
    def size(expectedSize: Int) {
      doCollected(collected, xs, "size", 1) { e =>
        val eSize = e.size
        if ((eSize == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, expectedSize)
            else
              FailureMessages("hadExpectedSize", e, expectedSize), 
            None, 
            6
          )
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
  final class ResultOfStartWithWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

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
     * all(string) should startWith regex ("Hel*o".r)
     *                              ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.pattern.matcher(e).lookingAt != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotStartWithRegex" else "startedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            7
          )
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
  final class ResultOfIncludeWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

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
     * all(string) should include regex ("wo.ld".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.findFirstIn(e).isDefined != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotIncludeRegex" else "includedRegex",
              e,
              rightRegex
            ), 
            None, 
            7
          )
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
  final class ResultOfEndWithWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

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
     * all(string) should endWith regex ("wor.d".r)
     *                            ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        val allMatches = rightRegex.findAllIn(e)
        if ((allMatches.hasNext && (allMatches.end == e.length)) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotEndWithRegex" else "endedWithRegex",
              e,
              rightRegex
            ), 
            None, 
            7
          )
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
  final class ResultOfFullyMatchWordForCollectedString(collected: Collected, xs: GenTraversable[String], shouldBeTrue: Boolean) {

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
     * all(string) should fullymatch regex ("Hel*o world".r)
     *                               ^
     * </pre>
     */
    def regex(rightRegex: Regex) { checkRegex(rightRegex) }
    
    private def checkRegex(rightRegex: Regex) {
      doCollected(collected, xs, "regex", 2) { e =>
        if (rightRegex.pattern.matcher(e).matches != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotFullyMatchRegex" else "fullyMatchedRegex",
              e,
              rightRegex
            ), 
            None, 
            7
          )
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
  final class ResultOfCollectedGenTraversable[T](collected: Collected, xs: GenTraversable[GenTraversable[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have size (3)
     *                       ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedGenTraversable[T] = 
      new ResultOfHaveWordForCollectedGenTraversable(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should be (Set(1, 2, 3))
     *                       ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenTraversable[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should equal (3)
     *                       ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[GenTraversable[T], TYPECLASS1])(implicit typeClass1: TYPECLASS1[GenTraversable[T]]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[GenTraversable[T], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[GenTraversable[T]], typeClass2: TYPECLASS2[GenTraversable[T]]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should be theSameInstanceAs anotherObject
     *                       ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should not have size (3)
     *                       ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenTraversable[T, GenTraversable[T]] = 
      new ResultOfNotWordForCollectedGenTraversable(collected, xs, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should contain (containMatcher)
     *                       ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedGenTraversable[T] = 
      new ResultOfContainWordForCollectedGenTraversable(collected, xs, true)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedGenTraversable[T](collected: Collected, xs: GenTraversable[GenTraversable[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have size (12)
     *                                   ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        val eSize = e.size
        if ((eSize == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, expectedSize)
            else
              FailureMessages("hadExpectedSize", e, expectedSize), 
            None, 
            6
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should have length (12)
     *                                   ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        val eLength = e.size
        if ((eLength == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, expectedLength)
            else
              FailureMessages("hadExpectedLength", e, expectedLength), 
            None, 
            6
          )
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
  final class ResultOfCollectedGenSeq[T](collected: Collected, xs: GenTraversable[GenSeq[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have length (3)
     *               ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedGenSeq[T] = 
      new ResultOfHaveWordForCollectedGenSeq(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should be (List(1, 2, 3))
     *               ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenSeq[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
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
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[GenSeq[T], TYPECLASS1])(implicit typeClass1: TYPECLASS1[GenSeq[T]]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[GenSeq[T], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[GenSeq[T]], typeClass2: TYPECLASS2[GenSeq[T]]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should be theSameInstanceAs anotherObject
     *               ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should not have length (3)
     *               ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenSeq[T, GenSeq[T]] = 
      new ResultOfNotWordForCollectedGenSeq(collected, xs, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfGenSeq) should contain (containMatcher)
     *                       ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedGenTraversable[T] = 
      new ResultOfContainWordForCollectedGenTraversable(collected, xs, true)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedGenSeq[T](collected: Collected, xs: GenTraversable[GenSeq[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have length (12)
     *                           ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        val eLength = e.length
        if ((eLength == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, expectedLength)
            else
              FailureMessages("hadExpectedLength", e, expectedLength), 
            None, 
            6
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfSeq) should have size (12)
     *                           ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        val eSize = e.size
        if ((eSize == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, expectedSize)
            else
              FailureMessages("hadExpectedSize", e, expectedSize), 
            None, 
            6
          )
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
  final class ResultOfContainWordForCollectedGenTraversable[T](collected: Collected, xs: GenTraversable[GenTraversable[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should contain theSameElementsAs List(1, 2, 3)
     *                                      ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      val containMatcher = new TheSameElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameElementsAs", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfTraversable) should contain theSameIteratedElementsAs List(1, 2, 3)
     *                                      ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      val containMatcher = new TheSameIteratedElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameIteratedElementsAs", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfTraversable) should contain allOf (1, 2, 3)
     *                                      ^
     * </pre>
     */
    def allOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new AllOfContainMatcher(right, equality)
      doCollected(collected, xs, "allOf", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfTraversable) should contain inOrder (1, 2, 3)
     *                                      ^
     * </pre>
     */
    def inOrder(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new InOrderContainMatcher(right, equality)
      doCollected(collected, xs, "inOrder", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfTraversable) should contain oneOf (1, 2, 3)
     *                                      ^
     * </pre>
     */
    def oneOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new OneOfContainMatcher(right, equality)
      doCollected(collected, xs, "oneOf", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfTraversable) should contain only (1, 2, 3)
     *                                      ^
     * </pre>
     */
    def only(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new OnlyContainMatcher(right, equality)
      doCollected(collected, xs, "only", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfTraversable) should contain inOrderOnly (1, 2, 3)
     *                                      ^
     * </pre>
     */
    def inOrderOnly(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new InOrderOnlyContainMatcher(right, equality)
      doCollected(collected, xs, "inOrderOnly", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfTraversable) should contain noneOf (1, 2, 3)
     *                                      ^
     * </pre>
     */
    def noneOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new NoneOfContainMatcher(right, equality)
      doCollected(collected, xs, "noneOf", 1) { e =>
        val result = containMatcher(e)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,  
            None, 
            6
          )
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
  final class ResultOfCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have size (3)
     *                 ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedArray[T] = 
      new ResultOfHaveWordForCollectedArray(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should be (Set(1, 2, 3))
     *                       ^
     * </pre>
     */
    def should[T](rightMatcher: Matcher[GenTraversable[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e.deep.asInstanceOf[IndexedSeq[T]]) match {  // TODO: Ugly but safe cast here because e is Array[T]
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfTraversable) should equal (3)
     *                       ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[GenTraversable[T], TYPECLASS1])(implicit typeClass1: TYPECLASS1[GenTraversable[T]]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e.deep.asInstanceOf[IndexedSeq[T]]) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[GenTraversable[T], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[GenTraversable[T]], typeClass2: TYPECLASS2[GenTraversable[T]]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e.deep.asInstanceOf[IndexedSeq[T]]) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should be theSameInstanceAs anotherObject
     *                 ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedArray(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should not have size (3)
     *                 ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedArray[T, Array[T]] = 
      new ResultOfNotWordForCollectedArray(collected, xs, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfArray) should contain (containMatcher)
     *                 ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedArray[T] = 
      new ResultOfContainWordForCollectedArray(collected, xs, true)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedArray[T](collected: Collected, xs: GenTraversable[Array[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have size (12)
     *                             ^
     * </pre>
     */
    def size(expectedSize: Int) {
      doCollected(collected, xs, "size", 1) { e =>
        val eSize = e.size
        if ((eSize == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, expectedSize)
            else
              FailureMessages("hadExpectedSize", e, expectedSize), 
            None, 
            6
          )
      }
    }
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfArray) should have length (12)
     *                             ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        val eLength = e.length
        if ((eLength == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, expectedLength)
            else
              FailureMessages("hadExpectedLength", e, expectedLength), 
            None, 
            6
          )
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
  final class ResultOfCollectedGenMap[K, V](collected: Collected, xs: GenTraversable[GenMap[K, V]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain key (10)
     *               ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedGenMap[K, V] = 
      new ResultOfContainWordForCollectedGenMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should be (Map(1 -> "one", 2 -> "two"))
     *               ^
     * </pre>
     */
    def should(rightMatcher: Matcher[GenMap[K, V]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should equal (3)
     *               ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[GenMap[K, V], TYPECLASS1])(implicit typeClass1: TYPECLASS1[GenMap[K, V]]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[GenMap[K, V], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[GenMap[K, V]], typeClass2: TYPECLASS2[GenMap[K, V]]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should be theSameInstanceAs (anotherMap)
     *               ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should not have size (3)
     *               ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedGenMap[K, V, GenMap[K, V]] = 
      new ResultOfNotWordForCollectedGenMap(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfContainWordForCollectedGenMap[K, V](collected: Collected, xs: GenTraversable[GenMap[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain key ("one")
     *                              ^
     * </pre>
     */
    def key(expectedKey: K) {
      doCollected(collected, xs, "key", 1) { e =>
        if (e.exists(_._1 == expectedKey) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
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
    def value(expectedValue: V) {
      doCollected(collected, xs, "value", 1) { e =>
        if (e.exists(expectedValue == _._2) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              expectedValue), 
            None, 
            6
          )
      }
    }
    
    
    /**
     * This method enables the following syntax: 
     *
     * <pre class="stHighlight">
     * all(colOfMap) should contain theSameElementsAs List(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      val containMatcher = new TheSameElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameElementsAs", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfMap) should contain theSameElementsAs List(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      val containMatcher = new TheSameIteratedElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameIteratedElementsAs", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfMap) should contain allOf List(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def allOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new AllOfContainMatcher(right, equality)
      doCollected(collected, xs, "allOf", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfMap) should contain inOrder List(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def inOrder(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new InOrderContainMatcher(right, equality)
      doCollected(collected, xs, "inOrder", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfMap) should contain oneOf Map(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def oneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new OneOfContainMatcher(right, equality)
      doCollected(collected, xs, "oneOf", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfMap) should contain only Map(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def only(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new OnlyContainMatcher(right, equality)
      doCollected(collected, xs, "only", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfMap) should contain inOrderOnly Map(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def inOrderOnly(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new InOrderOnlyContainMatcher(right, equality)
      doCollected(collected, xs, "inOrderOnly", 1) { e =>
        val result = containMatcher(e)
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
     * all(colOfMap) should contain noneOf Map(1 -> "one", 2 -> "two", 3 -> "three")
     *                              ^
     * </pre>
     */
    def noneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new NoneOfContainMatcher(right, equality)
      doCollected(collected, xs, "noneOf", 1) { e =>
        val result = containMatcher(e)
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,  
            None, 
            6
          )
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
  final class ResultOfCollectedJavaCollection[T](collected: Collected, xs: GenTraversable[java.util.Collection[T]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have size (3)
     *                   ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedJavaCollection[T] = 
      new ResultOfHaveWordForCollectedJavaCollection(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain theSameElementsAs List(1, 2, 3)
     *                   ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedJavaCollection[T] = 
      new ResultOfContainWordForCollectedJavaCollection(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should be (aJavaSet)
     *                   ^
     * </pre>
     */
    def should(rightMatcher: Matcher[java.util.Collection[T]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should equal (3)
     *                   ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[java.util.Collection[T], TYPECLASS1])(implicit typeClass1: TYPECLASS1[java.util.Collection[T]]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[java.util.Collection[T], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[java.util.Collection[T]], typeClass2: TYPECLASS2[java.util.Collection[T]]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should not have size (3)
     *                   ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedJavaCollection[T, java.util.Collection[T]] = 
      new ResultOfNotWordForCollectedJavaCollection(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedJavaCollection[T](collected: Collected, xs: GenTraversable[java.util.Collection[T]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have size (10)
     *                   ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        val eSize = e.size
        if ((eSize == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, expectedSize)
            else
              FailureMessages("hadExpectedSize", e, expectedSize), 
            None, 
            6
          )
       }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should have length (12)
     *                   ^
     * </pre>
     *
     * <p>
     * This method invokes <code>size</code> on the <code>java.util.List</code> passed as <code>left</code> to
     * determine its length.
     * </p>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        val eLength = e.size
        if ((eLength == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, expectedLength)
            else
              FailureMessages("hadExpectedLength", e, expectedLength), 
            None, 
            6
          )
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
  final class ResultOfContainWordForCollectedJavaCollection[T](collected: Collected, xs: GenTraversable[java.util.Collection[T]], shouldBeTrue: Boolean) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain theSameElementsAs List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      val containMatcher = new TheSameElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameElementsAs", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,  
            None, 
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain theSameIteratedElementsAs List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[T])(implicit equality: Equality[T]) {
      val containMatcher = new TheSameIteratedElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameIteratedElementsAs", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain allOf List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def allOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new AllOfContainMatcher(right, equality)
      doCollected(collected, xs, "allOf", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain inOrder List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def inOrder(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new InOrderContainMatcher(right, equality)
      doCollected(collected, xs, "inOrder", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain oneOf List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def oneOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new OneOfContainMatcher(right, equality)
      doCollected(collected, xs, "oneOf", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain only List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def only(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new OnlyContainMatcher(right, equality)
      doCollected(collected, xs, "only", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain inOrderOnly List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def inOrderOnly(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new InOrderOnlyContainMatcher(right, equality)
      doCollected(collected, xs, "inOrderOnly", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaCol) should contain noneOf List(1, 2, 3)
     *                                  ^
     * </pre>
     */
    def noneOf(right: T*)(implicit equality: Equality[T]) {
      val containMatcher = new NoneOfContainMatcher(right, equality)
      doCollected(collected, xs, "noneOf", 1) { e =>
        val result = containMatcher(new JavaCollectionWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
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
  final class ResultOfCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]]) {
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have size (3)
     *                   ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForCollectedJavaMap[K, V] = 
      new ResultOfHaveWordForCollectedJavaMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain value (3)
     *                   ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForCollectedJavaMap[K, V] = 
      new ResultOfContainWordForCollectedJavaMap(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should be (someJavaMap)
     *                   ^
     * </pre>
     */
    def should(rightMatcher: Matcher[java.util.Map[K, V]]) {
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 6)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should equal (3)
     *                   ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[java.util.Map[K, V], TYPECLASS1])(implicit typeClass1: TYPECLASS1[java.util.Map[K, V]]) {
      val rightMatcher = rightMatcherFactory1.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[java.util.Map[K, V], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[java.util.Map[K, V]], typeClass2: TYPECLASS2[java.util.Map[K, V]]) {
      val rightMatcher = rightMatcherFactory2.matcher
      doCollected(collected, xs, "should", 1) { e =>
        rightMatcher(e) match {
          case MatchResult(false, failureMessage, _, _, _) => 
            throw newTestFailedException(failureMessage, None, 10)
          case _ => ()
        }
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should be theSameInstanceAs anotherObject
     *                   ^
     * </pre>
     */
    def should(beWord: BeWord) = new ResultOfBeWordForCollectedAnyRef(collected, xs, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should not have length (3)
     *                   ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForCollectedJavaMap[K, V, java.util.Map[K, V]] = 
      new ResultOfNotWordForCollectedJavaMap(collected, xs, false)
  }
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   * @author Chee Seng
   */
  final class ResultOfHaveWordForCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have size (10)
     *                               ^
     * </pre>
     */
    def size(expectedSize: Long) {
      doCollected(collected, xs, "size", 1) { e =>
        val eSize = e.size
        if ((eSize == expectedSize) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadSizeInsteadOfExpectedSize", e, eSize, expectedSize)
            else
              FailureMessages("hadExpectedSize", e, expectedSize), 
            None, 
            6
          )
      }
    }
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should have length (10)
     *                               ^
     * </pre>
     */
    def length(expectedLength: Long) {
      doCollected(collected, xs, "length", 1) { e =>
        val eLength = e.size
        if ((eLength == expectedLength) != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue)
              FailureMessages("hadLengthInsteadOfExpectedLength", e, eLength, expectedLength)
            else
              FailureMessages("hadExpectedLength", e, expectedLength), 
            None, 
            6
          )
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
  final class ResultOfContainWordForCollectedJavaMap[K, V](collected: Collected, xs: GenTraversable[java.util.Map[K, V]], shouldBeTrue: Boolean) {
    
    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain key ("two")
     *                        ^
     * </pre>
     */
    def key(expectedKey: K) {
      doCollected(collected, xs, "key", 1) { e =>
        if (e.containsKey(expectedKey) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainKey" else "containedKey",
              e,
              expectedKey), 
            None, 
            6
          )
      }
    }

    /**
     * This method enables the following syntax (<code>javaMap</code> is a <code>java.util.Map</code>):
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain value ("2")
     *                        ^
     * </pre>
     */
    def value(expectedValue: V) {
      doCollected(collected, xs, "value", 1) { e =>
        if (e.containsValue(expectedValue) != shouldBeTrue)
          throw newTestFailedException(
            FailureMessages(
              if (shouldBeTrue) "didNotContainValue" else "containedValue",
              e,
              expectedValue), 
              None, 
              6
          )
      }
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain theSameElementsAs List(1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      val containMatcher = new TheSameElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameElementsAs", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,  
            None, 
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain theSameIteratedElementsAs List(1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def theSameIteratedElementsAs(right: GenTraversable[(K, V)])(implicit equality: Equality[(K, V)]) {
      val containMatcher = new TheSameIteratedElementsAsContainMatcher(right, equality)
      doCollected(collected, xs, "theSameIteratedElementsAs", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain allOf (1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def allOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new AllOfContainMatcher(right, equality)
      doCollected(collected, xs, "allOf", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain inOrder (1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def inOrder(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new InOrderContainMatcher(right, equality)
      doCollected(collected, xs, "inOrder", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain oneOf (1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def oneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new OneOfContainMatcher(right, equality)
      doCollected(collected, xs, "oneOf", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain only (1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def only(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new OnlyContainMatcher(right, equality)
      doCollected(collected, xs, "only", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain inOrderOnly (1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def inOrderOnly(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new InOrderOnlyContainMatcher(right, equality)
      doCollected(collected, xs, "inOrderOnly", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * all(colOfJavaMap) should contain noneOf (1 -> "one", 2 - > "two", 3 -> "three")
     *                                  ^
     * </pre>
     */
    def noneOf(right: (K, V)*)(implicit equality: Equality[(K, V)]) {
      val containMatcher = new NoneOfContainMatcher(right, equality)
      doCollected(collected, xs, "noneOf", 1) { e =>
        val result = containMatcher(new JavaMapWrapper(e))
        if (result.matches != shouldBeTrue)
          throw newTestFailedException(
            if (shouldBeTrue) result.failureMessage else result.negatedFailureMessage,
            None,
            6
          )
      }
    }
    
  }

  def all[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(AllCollected, xs)
  
  def all(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(AllCollected, xs)
  
  def all(xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(AllCollected, xs)
  
  def all[T](xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(AllCollected, xs)
  
  def all[T](xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(AllCollected, xs)
  
  def all[T](xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(AllCollected, xs)
  
  def all[K, V](xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(AllCollected, xs)
  
  def all[T](xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(AllCollected, xs)
  
  def all[K, V](xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(AllCollected, xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(AtLeastCollected(num), xs)
  
  def atLeast(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(AtLeastCollected(num), xs)
  
  def atLeast(num: Int, xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(AtLeastCollected(num), xs)
  
  def atLeast[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(AtLeastCollected(num), xs)
  
  def atLeast[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(AtLeastCollected(num), xs)
  
  def atLeast[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(AtLeastCollected(num), xs)
  
  def every[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(EveryCollected, xs)
  
  def every(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(EveryCollected, xs)
  
  def every(xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(EveryCollected, xs)
  
  def every[K, V](xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(EveryCollected, xs)
  
  def every[T](xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(EveryCollected, xs)
  
  def every[K, V](xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(EveryCollected, xs)
  
  def exactly[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] = 
    new ResultOfCollectedAny(ExactlyCollected(num), xs)
  
  def exactly(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] = 
    new ResultOfCollectedAnyRef(ExactlyCollected(num), xs)
  
  def exactly(num: Int, xs: GenTraversable[String]): ResultOfCollectedString = 
    new ResultOfCollectedString(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[GenTraversable[T]]) = 
    new ResultOfCollectedGenTraversable(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[GenSeq[T]]) = 
    new ResultOfCollectedGenSeq(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[Array[T]]) = 
    new ResultOfCollectedArray(ExactlyCollected(num), xs)
  
  def exactly[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) = 
    new ResultOfCollectedGenMap(ExactlyCollected(num), xs)
  
  def exactly[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) = 
    new ResultOfCollectedJavaCollection(ExactlyCollected(num), xs)
  
  def exactly[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) = 
    new ResultOfCollectedJavaMap(ExactlyCollected(num), xs)

  def no[T](xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(NoCollected, xs)

  def no(xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(NoCollected, xs)

  def no(xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(NoCollected, xs)

  def no[T](xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(NoCollected, xs)

  def no[T](xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(NoCollected, xs)

  def no[T](xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(NoCollected, xs)

  def no[K, V](xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(NoCollected, xs)

  def no[T](xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(NoCollected, xs)

  def no[K, V](xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(NoCollected, xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(BetweenCollected(from, upTo), xs)

  def between(from: Int, upTo:Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(BetweenCollected(from, upTo), xs)

  def between(from: Int, upTo:Int, xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(BetweenCollected(from, upTo), xs)

  def between[K, V](from: Int, upTo:Int, xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(BetweenCollected(from, upTo), xs)

  def between[T](from: Int, upTo:Int, xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(BetweenCollected(from, upTo), xs)

  def between[K, V](from: Int, upTo:Int, xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(BetweenCollected(from, upTo), xs)

  def atMost[T](num: Int, xs: GenTraversable[T]): ResultOfCollectedAny[T] =
    new ResultOfCollectedAny(AtMostCollected(num), xs)

  def atMost(num: Int, xs: GenTraversable[AnyRef]): ResultOfCollectedAnyRef[AnyRef] =
    new ResultOfCollectedAnyRef(AtMostCollected(num), xs)

  def atMost(num: Int, xs: GenTraversable[String]): ResultOfCollectedString =
    new ResultOfCollectedString(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[GenTraversable[T]]) =
    new ResultOfCollectedGenTraversable(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[GenSeq[T]]) =
    new ResultOfCollectedGenSeq(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[Array[T]]) =
    new ResultOfCollectedArray(AtMostCollected(num), xs)

  def atMost[K, V](num: Int, xs: GenTraversable[GenMap[K, V]]) =
    new ResultOfCollectedGenMap(AtMostCollected(num), xs)

  def atMost[T](num: Int, xs: GenTraversable[java.util.Collection[T]]) =
    new ResultOfCollectedJavaCollection(AtMostCollected(num), xs)

  def atMost[K, V](num: Int, xs: GenTraversable[java.util.Map[K, V]]) =
    new ResultOfCollectedJavaMap(AtMostCollected(num), xs)
  // This is where ShouldMatchers.scala started 

  // Turn off this implicit conversion, becase asAny method is added via AnyShouldWrapper
  // override def convertToAsAnyWrapper(o: Any): AsAnyWrapper = new AsAnyWrapper(o)

  private object ShouldMethodHelper {
    def shouldMatcher[T](left: T, rightMatcher: Matcher[T], stackDepthAdjustment: Int = 0) {
      rightMatcher(left) match {
        case MatchResult(false, failureMessage, _, _, _) => throw newTestFailedException(failureMessage, None, stackDepthAdjustment)
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
  class AnyShouldWrapper[T](left: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should be (3)
     *        ^
     * </pre>
     */
    def should(rightMatcherX1: Matcher[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX1)
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
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory1.matcher)
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[T, TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[T], typeClass2: TYPECLASS2[T]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory2.matcher)
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
      if (!equality.areEqual(left, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    // TODO: Ensure on inspector shorthands. Moved this here from NumericShouldWrapper.
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual 7.1 +- 0.2
     *        ^
     * </pre>
     */
    def shouldEqual(interval: Interval[T]) {
      if (!interval.isWithin(left)) {
        throw newTestFailedException(FailureMessages("didNotEqualPlusOrMinus", left, interval.pivot, interval.tolerance))
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
    def should(notWord: NotWord): ResultOfNotWord[T] = new ResultOfNotWord[T](left, false)

    // In 2.10, will work with AnyVals. TODO: Also, Need to ensure Char works
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * a should === (b)
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[T, U]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }

    // TODO: Need to make sure this works in inspector shorthands. I moved this
    // up here from NumericShouldWrapper.
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (100 +- 1)
     *        ^
     * </pre>
     */
    def should(inv: TripleEqualsInvocationOnInterval[T])(implicit ev: Numeric[T]) {
      if ((inv.interval.isWithin(left)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
            if (inv.expectingEqual) "didNotEqualPlusOrMinus" else "equaledPlusOrMinus",
            left,
            inv.interval.pivot,
            inv.interval.tolerance
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
    def should(beWord: BeWord): ResultOfBeWordForAny[T] = new ResultOfBeWordForAny(left, true)

    /* *
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * aDouble shouldBe 8.8
     *         ^
     * </pre>
    def shouldBe(right: T) {
      if (left != right) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("wasNotEqualTo", leftee, rightee))
      }
    }

    def shouldBe(beMatcher: BeMatcher[T]) { // TODO: This looks like a bug to me. Investigate. - bv
      beMatcher.apply(left).matches
    }
*/

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
    def should(haveWord: HaveWord)(implicit ev: Extent[T]): ResultOfHaveWordForExtent[T] =
      new ResultOfHaveWordForExtent(left, true)

    // TODO: Scaladoc, and decide whether or not to actually even support this here. It may be best
    // to let this one always be imported from ScalaUtils.
    def asAny: Any = left
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
  final class StringShouldWrapper(val left: String) extends AnyRefShouldWrapper(left) with StringShouldWrapperForVerb {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should have length (3)
     *        ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForString = {
      new ResultOfHaveWordForString(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * string should include regex ("hi")
     *        ^
     * </pre>
     */
    def should(includeWord: IncludeWord): ResultOfIncludeWordForString = {
      new ResultOfIncludeWordForString(left, true)
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
      new ResultOfStartWithWordForString(left, true)
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
      new ResultOfEndWithWordForString(left, true)
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
      new ResultOfFullyMatchWordForString(left, true)
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
      new ResultOfNotWordForString(left, false)
    }
  }

// TODO: Am I doing conversions on immutable.GenTraversable and immutable.GenSeq? If so, write a test that fails and make it general.
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>scala.collection.GenMap[K, V]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class MapShouldWrapper[K, V, L[_, _] <: scala.collection.GenMap[_, _]](left: L[K, V]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should be (Map(1 -&gt; "one", 2 -&gt; "two"))
     *     ^
     * </pre>
     */
    def should(rightMatcherX4: Matcher[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX4)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should equal (Map(1 -&gt; "one", 2 -&gt; "two"))
     *     ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[L[K, V], TYPECLASS1])(implicit typeClass1: TYPECLASS1[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory1.matcher)
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[L[K, V], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[L[K, V]], typeClass2: TYPECLASS2[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory2.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map shouldEqual Map(1 -&gt; "one", 2 -&gt; "two")
     *     ^
     * </pre>
     */
    def shouldEqual(right: L[K, V])(implicit equality: Equality[L[K, V]]) {
      if (!equality.areEqual(left, right)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("didNotEqual", leftee, rightee))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should be theSameInstanceAs (anotherMap)
     *     ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[L[K, V]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[L[K, V]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should have size (3)
     *     ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForTraversable[(K, V)] = {
      new ResultOfHaveWordForTraversable(left.asInstanceOf[GenMap[K,V]], true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should contain key (10)
     *     ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForMap[K, V] = {
      new ResultOfContainWordForMap(left.asInstanceOf[GenMap[K, V]], true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * map should not have size (3)
     *     ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForMap[K, V, L] = {
      new ResultOfNotWordForMap(left.asInstanceOf[L[K, V]], false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (Map("I" -&gt; 1, "II" -&gt; 2))
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[K, V], R]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on <code>AnyRef</code>s.
   * </p>
   *
   * @author Bill Venners
   */
  class AnyRefShouldWrapper[T <: AnyRef](left: T) extends AnyShouldWrapper(left) {

    // TODO: Ensure on inspector shorthands. Had to add this here after moving another shouldEqual method from NumericShouldWrapper.
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result shouldEqual null
     *        ^
     * </pre>
     */
    def shouldEqual(right: Null) { 
      if (left != null) {
        throw newTestFailedException(FailureMessages("didNotEqualNull", left))
      }
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should not have length (3)
     *        ^
     * </pre>
     */
    override def should(notWord: NotWord): ResultOfNotWordForAnyRef[T] =
      new ResultOfNotWordForAnyRef(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should be theSameInstanceAs anotherObject
     *        ^
     * </pre>
     */
    override def should(beWord: BeWord): ResultOfBeWordForAnyRef[T] = new ResultOfBeWordForAnyRef(left, true)
    
/*
    def shouldBe: ResultOfBeWordForAnyRef[T] = new ResultOfBeWordForAnyRef(left, true)
    
    def shouldBe(right: Null) {
      if (left != null) {
        throw newTestFailedException(FailureMessages("wasNotNull", left))
      }
    }
*/

/*
    def shouldBe[U](right: AType[U]) {
      if (!right.isAssignableFromClassOf(left)) {
        throw newTestFailedException(FailureMessages("wasNotAnInstanceOf", left, UnquotedString(right.className)))
      }
    }
*/

/*
    def shouldBe(right: AnyRef) {

      def shouldBeEqual(right: AnyRef): Boolean = {
        if (right.isInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]) {
          // need to put in if because NoSuchMethodError when pattern match ResultOfAWordToBePropertyMatcherApplication
          val app = right.asInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]
          app.bePropertyMatcher.apply(left).matches
        }
        else if (right.isInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]) {
          val app = right.asInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]
          app.bePropertyMatcher.apply(left).matches
        }
        else {
          val beWord = new BeWord
          right match {
            case rightSymbol: ResultOfAWordToSymbolApplication => 
              beWord.a[AnyRef](rightSymbol.symbol)(left).matches
            case rightSymbol: ResultOfAnWordToSymbolApplication => 
              beWord.an[AnyRef](rightSymbol.symbol)(left).matches
            case beMatcher: BeMatcher[AnyRef] => 
              beMatcher.apply(left).matches
            case bePropertyMatcher: BePropertyMatcher[AnyRef] => 
              bePropertyMatcher.apply(left).matches
            case _ => 
              left == right
          }
        }
      }

      if (!shouldBeEqual(right)) {
        val (resourceName, leftee, rightee) = 
          if (right.isInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]) {
            val app = right.asInstanceOf[ResultOfAWordToBePropertyMatcherApplication[AnyRef]]
            val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, UnquotedString(app.bePropertyMatcher.apply(left).propertyName))
            ("wasNotA", leftee, rightee)
          }
          else if (right.isInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]) {
            val app = right.asInstanceOf[ResultOfAnWordToBePropertyMatcherApplication[AnyRef]]
            val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, UnquotedString(app.bePropertyMatcher.apply(left).propertyName))
            ("wasNotAn", leftee, rightee)
          }
          else {
            right match {
              case bePropertyMatcher: BePropertyMatcher[AnyRef] => 
                val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, UnquotedString(bePropertyMatcher.apply(left).propertyName))
                ("wasNot", leftee, rightee)
              case _ => 
                val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
                ("wasNotEqualTo", leftee, rightee)
            }
          }
        throw newTestFailedException(FailureMessages(resourceName, leftee, rightee))
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>scala.Collection[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class TraversableShouldWrapper[E, L[_] <: GenTraversable[_]](left: L[E]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should be (Set(1, 2, 3))
     *             ^
     * </pre>
     */
    def should(rightMatcherX6: Matcher[GenTraversable[E]]) {
      ShouldMethodHelper.shouldMatcher(left.asInstanceOf[GenTraversable[E]], rightMatcherX6)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should equal (Set(1, 2, 3))
     *             ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[L[E], TYPECLASS1])(implicit typeClass1: TYPECLASS1[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory1.matcher)
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[L[E], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[L[E]], typeClass2: TYPECLASS2[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory2.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should have size (3)
     *             ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForTraversable[E] = 
      new ResultOfHaveWordForTraversable(left.asInstanceOf[GenTraversable[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should contain theSameElementsAs anotherTraversable
     *             ^
     * </pre>
     */
    def should(containWord: ContainWord) = 
      new ResultOfContainWordForTraversable(left.asInstanceOf[GenTraversable[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should be theSameInstanceAs anotherObject
     *             ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[L[E]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[L[E]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * traversable should not have size (3)
     *             ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForTraversable[E, L] =
      new ResultOfNotWordForTraversable(left, false)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (Set(1, 2, 3))
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[E], R]) {
      // if ((left == inv.right) != inv.expectingEqual)
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * xs.loneElement should be > 9
     *    ^
     * </pre>
     */
    def loneElement: E = {
      if (left.size == 1)
        left.head.asInstanceOf[E] // Why do I need to cast?
      else
        throw newTestFailedException(
          FailureMessages(
            "notLoneElement",
            left,
            left.size)
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>java.util.Collection[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  // final class JavaCollectionShouldWrapper[T](left: java.util.Collection[T]) {
  final class JavaCollectionShouldWrapper[E, L[_] <: java.util.Collection[_]](left: L[E]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should be (aJavaSet)
     *                ^
     * </pre>
     */
    def should(rightMatcherX7: Matcher[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX7)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should equal (aJavaSet)
     *                ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[L[E], TYPECLASS1])(implicit typeClass1: TYPECLASS1[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory1.matcher)
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[L[E], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[L[E]], typeClass2: TYPECLASS2[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory2.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should have size (3)
     *                ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForJavaCollection[E, L] =
      new ResultOfHaveWordForJavaCollection(left, true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should contain theSameElementsAs anotherSeq
     *                ^
     * </pre>
     */
    def should(containWord: ContainWord) = 
      new ResultOfContainWordForJavaCollection(left.asInstanceOf[java.util.Collection[E]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should be theSameInstanceAs anotherObject
     *                ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[L[E]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[L[E]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaCollection should not have size (3)
     *                ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForJavaCollection[E, L] =
      new ResultOfNotWordForJavaCollection(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (jSet)
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[E], R]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>java.util.Map[K, V]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class JavaMapShouldWrapper[K, V, L[_, _] <: java.util.Map[_, _]](left: L[K, V]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should be (someJavaMap)
     *         ^
     * </pre>
     */
    def should(rightMatcherX8: Matcher[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX8)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should equal (someJavaMap)
     *         ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[L[K, V], TYPECLASS1])(implicit typeClass1: TYPECLASS1[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory1.matcher)
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[L[K, V], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[L[K, V]], typeClass2: TYPECLASS2[L[K, V]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory2.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should contain value (3)
     *         ^
     * </pre>
     */
    def should(containWord: ContainWord): ResultOfContainWordForJavaMap[K, V] = {
      new ResultOfContainWordForJavaMap(left.asInstanceOf[java.util.Map[K, V]], true)
    }
 
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should have size (3)
     *         ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForJavaMap = {
      new ResultOfHaveWordForJavaMap(left, true)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should not have length (3)
     *         ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForJavaMap[K, V, L] = {
      new ResultOfNotWordForJavaMap[K, V, L](left, false)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaMap should be theSameInstanceAs anotherObject
     *         ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[java.util.Map[K, V]] = new ResultOfBeWordForAnyRef(left.asInstanceOf[java.util.Map[K, V]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (javaMap)
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[K, V], R]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>GenSeq[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class SeqShouldWrapper[E, L[_] <: GenSeq[_]](left: L[E]) {
 
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should be (List(1, 2, 3))
     *     ^
     * </pre>
     */
    def should(rightMatcherX9: Matcher[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX9)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should equal (List(1, 2, 3))
     *     ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[L[E], TYPECLASS1])(implicit typeClass1: TYPECLASS1[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory1.matcher)
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[L[E], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[L[E]], typeClass2: TYPECLASS2[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory2.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should have length (3)
     *     ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForSeq[E] =
      new ResultOfHaveWordForSeq(left.asInstanceOf[GenSeq[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should contain theSameElementsAs anotherSeq
     *     ^
     * </pre>
     */
    def should(containWord: ContainWord) = 
      new ResultOfContainWordForTraversable(left.asInstanceOf[GenTraversable[E]], true)
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should not have length (3)
     *     ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForSeq[E, L] =
      new ResultOfNotWordForSeq(left, false)
    // def should(notWord: NotWord): ResultOfNotWordForAnyRef[GenSeq[E]] =
      // new ResultOfNotWordForAnyRef(left, false)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should be theSameInstanceAs List(1, 2, 3)
     *     ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[L[E]] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (List(1, 2, 3))
     *        ^
     * </pre>
     */
    def should[R](inv: TripleEqualsInvocation[R])(implicit constraint: EqualityConstraint[L[E], R]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>scala.Array[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class ArrayShouldWrapper[E](left: Array[E]) extends AnyRefShouldWrapper(left) {

     /**
     * This method enables syntax such as the following, where <code>positiveNumber</code> is a <code>AMatcher</code>:
     *
     * <pre class="stHighlight">
     * array should contain a positiveNumber
     *       ^
     * </pre>
     */
    def should(containWord: ContainWord) = 
      new ResultOfContainWordForTraversable(new ArrayWrapper(left), true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * array should not have length (3)
     *       ^
     * </pre>
     */
    override def should(notWord: NotWord): ResultOfNotWordForArray[E] =
      new ResultOfNotWordForArray(left, false)
    
/*
    def shouldBe(right: Array[E]) {
      if (!left.deep.equals(right.deep)) {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
        throw newTestFailedException(FailureMessages("wasNotEqualTo", leftee, rightee))
      }
    }
*/
  }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>should</code> methods to
   * be invoked on objects of type <code>java.util.List[T]</code>.
   * </p>
   *
   * @author Bill Venners
   */
  final class JavaListShouldWrapper[E, L[_] <: java.util.List[_]](left: L[E]) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should be (someOtherJavaList)
     *          ^
     * </pre>
     */
    def should(rightMatcherX12: Matcher[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherX12)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should equal (someOtherJavaList)
     *          ^
     * </pre>
     */
    def should[TYPECLASS1[_]](rightMatcherFactory1: MatcherFactory1[L[E], TYPECLASS1])(implicit typeClass1: TYPECLASS1[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory1.matcher)
    }

    def should[TYPECLASS1[_], TYPECLASS2[_]](rightMatcherFactory2: MatcherFactory2[L[E], TYPECLASS1, TYPECLASS2])(implicit typeClass1: TYPECLASS1[L[E]], typeClass2: TYPECLASS2[L[E]]) {
      ShouldMethodHelper.shouldMatcher(left, rightMatcherFactory2.matcher)
    }

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should have length (3)
     *          ^
     * </pre>
     */
    def should(haveWord: HaveWord): ResultOfHaveWordForJavaList[E, L] = {
      new ResultOfHaveWordForJavaList(left, true)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should contain theSameElementsAs anotherSeq
     *          ^
     * </pre>
     */
    def should(containWord: ContainWord) = 
      new ResultOfContainWordForJavaCollection(left.asInstanceOf[java.util.Collection[E]], true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * javaList should not have length (3)
     *          ^
     * </pre>
     */
    def should(notWord: NotWord): ResultOfNotWordForJavaList[E, L] = {
      new ResultOfNotWordForJavaList(left, false)
    }
    
    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * seq should be theSameInstanceAs List(1, 2, 3)
     *     ^
     * </pre>
     */
    def should(beWord: BeWord): ResultOfBeWordForAnyRef[L[E]] = new ResultOfBeWordForAnyRef(left, true)

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * result should === (jList)
     *        ^
     * </pre>
     */
    def should[U](inv: TripleEqualsInvocation[U])(implicit constraint: EqualityConstraint[L[E], U]) {
      if ((constraint.areEqual(left, inv.right)) != inv.expectingEqual)
        throw newTestFailedException(
          FailureMessages(
           if (inv.expectingEqual) "didNotEqual" else "equaled",
            left,
            inv.right
          )
        )
    }
  }

  /**
   * Implicitly converts an object of type <code>T</code> to a <code>AnyShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToAnyShouldWrapper[T](o: T): AnyShouldWrapper[T] = new AnyShouldWrapper(o)

  /**
   * Implicitly converts a <code>scala.AnyRef</code> of type <code>T</code> to an <code>AnyRefShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToAnyRefShouldWrapper[T <: AnyRef](o: T): AnyRefShouldWrapper[T] = new AnyRefShouldWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.Collection[T]</code> to a <code>CollectionShouldWrapper</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToTraversableShouldWrapper[E, L[_] <: GenTraversable[_]](o: L[E]): TraversableShouldWrapper[E, L] = new TraversableShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>GenSeq[T]</code> to a <code>SeqShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToSeqShouldWrapper[E, L[_] <: GenSeq[_]](o: L[E]): SeqShouldWrapper[E, L] = new SeqShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>scala.Array[T]</code> to a <code>ArrayShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToArrayShouldWrapper[T](o: Array[T]): ArrayShouldWrapper[T] = new ArrayShouldWrapper[T](o)

  /**
   * Implicitly converts an object of type <code>scala.collection.GenMap[K, V]</code> to a <code>MapShouldWrapper[K, V]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToMapShouldWrapper[K, V, L[_, _] <: scala.collection.GenMap[_, _]](o: L[K, V]): MapShouldWrapper[K, V, L] = new MapShouldWrapper[K, V, L](o)

  /**
   * Implicitly converts an object of type <code>java.lang.String</code> to a <code>StringShouldWrapper</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit override def convertToStringShouldWrapper(o: String): StringShouldWrapper = new StringShouldWrapper(o)

  /**
   * Implicitly converts an object of type <code>java.util.Collection[T]</code> to a <code>JavaCollectionShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToJavaCollectionShouldWrapper[E, L[_] <: java.util.Collection[_]](o: L[E]): JavaCollectionShouldWrapper[E, L] = new JavaCollectionShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>java.util.List[T]</code> to a <code>JavaListShouldWrapper[T]</code>,
   * to enable <code>should</code> methods to be invokable on that object. This conversion is necessary to enable
   * <code>length</code> to be used on Java <code>List</code>s.
   */
  // implicit def convertToJavaListShouldWrapper[T](o: java.util.List[T]): JavaListShouldWrapper[T] = new JavaListShouldWrapper[T](o)
  implicit def convertToJavaListShouldWrapper[E, L[_] <: java.util.List[_]](o: L[E]): JavaListShouldWrapper[E, L] = new JavaListShouldWrapper[E, L](o)

  /**
   * Implicitly converts an object of type <code>java.util.Map[K, V]</code> to a <code>JavaMapShouldWrapper[K, V]</code>,
   * to enable <code>should</code> methods to be invokable on that object.
   */
  implicit def convertToJavaMapShouldWrapper[K, V, L[_, _] <: java.util.Map[_, _]](o: L[K, V]): JavaMapShouldWrapper[K, V, L] = new JavaMapShouldWrapper[K, V, L](o)
  
  /**
   * Turn off implicit conversion of LoneElement, so that if user accidentally mixin LoneElement it does conflict with convertToTraversableShouldWrapper
   */
  override def convertToTraversableLoneElementWrapper[T](xs: GenTraversable[T]): LoneElementTraversableWrapper[T] = new LoneElementTraversableWrapper[T](xs)

  // This one doesn't include Holder in its result type because that would conflict with the
  // one returned by enablersForJavaCollection.
  implicit def enablersForJavaList[E, JLIST[_] <: java.util.List[_]]: Length[JLIST[E]] = 
    new Length[JLIST[E]] {
      def extentOf(javaList: JLIST[E]): Long = javaList.size
    }

  // This one doesn't include Holder in its result type because that would conflict with the
  // one returned by enablersForTraversable.
  implicit def enablersForSeq[E, SEQ[_] <: scala.collection.GenSeq[_]]: Length[SEQ[E]] = 
    new Length[SEQ[E]] {
      def extentOf(seq: SEQ[E]): Long = seq.length
    }

  implicit def enablersForJavaCollection[E, JCOL[_] <: java.util.Collection[_]]: Size[JCOL[E]] = 
    new Size[JCOL[E]] {
      def extentOf(javaColl: JCOL[E]): Long = javaColl.size
    }

  implicit def equalityEnablersForJavaCollection[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Holder[JCOL[E]] = 
    decidedForJavaCollection by equality

  object decidedForJavaCollection {
    def by[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Holder[JCOL[E]] = 
      new Holder[JCOL[E]] {
        def containsElement(javaColl: JCOL[E], ele: Any): Boolean = {
          val it: java.util.Iterator[E] = javaColl.iterator.asInstanceOf[java.util.Iterator[E]]
          var found = false
          while (!found && it.hasNext) {
            found = equality.areEqual(it.next , ele)
          }
          found
        }
      }
  }

  // I think Java Maps aren't Holders, because they don't have an element type. The only
  // thing close is the stupid Entry<K, V> type, which is mutable!
  implicit def enablersForJavaMap[K, V, JMAP[_, _] <: java.util.Map[_, _]]: Size[JMAP[K, V]] = 
    new Size[JMAP[K, V]] {
      def extentOf(javaMap: JMAP[K, V]): Long = javaMap.size
    }

  // This one could also mix in DefaultHolder. Wait, no, a Holder with an explicit equality.
  // ExplicitEqualityHolder. That guy would have a method like:
  // def containsElement(trav: TRAV[E], ele: Any, equality: Equality[E]): Boolean = {
  implicit def enablersForTraversable[E, TRAV[_] <: scala.collection.GenTraversable[_]]: Size[TRAV[E]] = 
    new Size[TRAV[E]] {
      def extentOf(trav: TRAV[E]): Long = trav.size
    }

  object decidedForTraversable {
    def by[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Holder[TRAV[E]] = 
      new Holder[TRAV[E]] {
        def containsElement(trav: TRAV[E], ele: Any): Boolean = {
          trav.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler thinks e is Any. Should be E. Compiler bug?
        }
      }
  }

  implicit def equalityEnablersForTraversable[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Holder[TRAV[E]] = 
    new Holder[TRAV[E]] {
      def containsElement(trav: TRAV[E], ele: Any): Boolean = {
        trav.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler thinks e is Any. Should be E. Compiler bug?
      }
    }

  implicit def enablersForMap[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]]: Size[MAP[K, V]] with Holder[MAP[K, V]] = 
    new Size[MAP[K, V]] with Holder[MAP[K, V]] {
      def extentOf(map: MAP[K, V]): Long = map.size
      def containsElement(map: MAP[K, V], ele: Any): Boolean = map.exists(_ == ele)
    }

  implicit def enablersForArray[E]: Length[Array[E]] with Size[Array[E]] = 
    new Length[Array[E]] with Size[Array[E]] {
      def extentOf(arr: Array[E]): Long = arr.length
    }

  implicit def equalityEnablersForArray[E](implicit equality: Equality[E]): Holder[Array[E]] = 
    new Holder[Array[E]] {
      def containsElement(arr: Array[E], ele: Any): Boolean =
        arr.exists((e: E) => equality.areEqual(e, ele))
    }

  object decidedForArray {
    def by[E](equality: Equality[E]): Holder[Array[E]] = 
      new Holder[Array[E]] {
        def containsElement(arr: Array[E], ele: Any): Boolean =
          arr.exists((e: E) => equality.areEqual(e, ele))
      }
  }

  implicit val enablersForString: Length[String] with Size[String] = 
    new Length[String] with Size[String] {
      def extentOf(str: String): Long = str.length
    }

  implicit def equalityEnablersForString(implicit equality: Equality[Char]): Holder[String] = 
    new Holder[String] {
      def containsElement(str: String, ele: Any): Boolean =
          str.exists((e: Char) => equality.areEqual(e, ele))
    }

  object decidedForString {
    def by(equality: Equality[Char]): Holder[String] = 
      new Holder[String] {
        def containsElement(str: String, ele: Any): Boolean =
          str.exists((e: Char) => equality.areEqual(e, ele))
      }
  }

  implicit def convertResultOfLengthWordApplicationToHavePropertyMatcher[T](resultOfLengthWordApplication: ResultOfLengthWordApplication)(implicit length: Length[T]): HavePropertyMatcher[T, Long] =
    new HavePropertyMatcher[T, Long] {
      def apply(objectWithProperty: T): HavePropertyMatchResult[Long] = {
        val expectedLength = resultOfLengthWordApplication.expectedLength
        val result = length.extentOf(objectWithProperty)
        new HavePropertyMatchResult[Long](
          result == expectedLength,
          "length",
          expectedLength,
          result
        )
      }
    } 

  implicit def convertResultOfSizeWordApplicationToHavePropertyMatcher[T](resultOfSizeWordApplication: ResultOfSizeWordApplication)(implicit size: Size[T]): HavePropertyMatcher[T, Long] =
    new HavePropertyMatcher[T, Long] {
      def apply(objectWithProperty: T): HavePropertyMatchResult[Long] = {
        val expectedSize = resultOfSizeWordApplication.expectedSize
        val result = size.extentOf(objectWithProperty)
        new HavePropertyMatchResult[Long](
          result == expectedSize,
          "size",
          expectedSize,
          result
        )
      }
    } 
}

/**
 * Companion object that facilitates the importing of <code>Matchers</code> members as 
  an alternative to mixing it the trait. One use case is to import <code>Matchers</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.7.3.final (Java HotSpot(TM) Client VM, Java 1.5.0_16).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * 
 * scala&gt; import org.scalatest.Matchers._
 * import org.scalatest.Matchers._
 * 
 * scala&gt; 1 should equal (2)
 * org.scalatest.TestFailedException: 1 did not equal 2
 * 	at org.scalatest.matchers.Helper$.newTestFailedException(Matchers.template:40)
 * 	at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.shouldMatcher(ShouldMatchers.scala:826)
 * 	at org.scalatest.matchers.ShouldMatchers$IntShouldWrapper.should(ShouldMatchers.scala:1123)
 * 	at .&lt;init&gt;(&lt;console&gt;:9)
 * 	at .&lt;clinit&gt;(&lt;console&gt;)
 * 	at RequestR...
 *
 * scala&gt; "hello, world" should startWith ("hello")
 * 
 * scala&gt; 7 should (be &gt;= (3) and not be &lt;= (7))
 * org.scalatest.TestFailedException: 7 was greater than or equal to 3, but 7 was less than or equal to 7
 * 	at org.scalatest.matchers.Helper$.newTestFailedException(Matchers.template:40)
 * 	at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.shouldMatcher(ShouldMatchers.scala:826)
 * 	at org.scalatest.matchers.ShouldMatchers$IntShouldWrapper.should(ShouldMatchers.scala:1123)
 * 	at .&lt;init&gt;(...
 * </pre>
 *
 * @author Bill Venners
 */
object Matchers extends Matchers {
  private[scalatest] def andMatchersAndApply[T](left: T, leftMatcher: Matcher[T], rightMatcher: Matcher[T]): MatchResult = {
    val leftMatchResult = leftMatcher(left)
    val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
    if (!leftMatchResult.matches)
      MatchResult(
        false,
        leftMatchResult.failureMessage,
        leftMatchResult.negatedFailureMessage,
        leftMatchResult.midSentenceFailureMessage,
        leftMatchResult.midSentenceNegatedFailureMessage
      )
    else {
      MatchResult(
        rightMatchResult.matches,
        Resources("commaBut", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.negatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
        Resources("commaBut", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.midSentenceNegatedFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
      )
    }
  }

  private[scalatest] def orMatchersAndApply[T](left: T, leftMatcher: Matcher[T], rightMatcher: Matcher[T]): MatchResult = {
    val leftMatchResult = leftMatcher(left)
    val rightMatchResult = rightMatcher(left) // Not short circuiting anymore
    if (leftMatchResult.matches)
      MatchResult(
        true,
        leftMatchResult.negatedFailureMessage,
        leftMatchResult.failureMessage,
        leftMatchResult.midSentenceNegatedFailureMessage,
        leftMatchResult.midSentenceFailureMessage
      )
    else {
      MatchResult(
        rightMatchResult.matches,
        Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.failureMessage, rightMatchResult.midSentenceNegatedFailureMessage),
        Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceFailureMessage),
        Resources("commaAnd", leftMatchResult.midSentenceFailureMessage, rightMatchResult.midSentenceNegatedFailureMessage)
      )
    }
  }
  private[scalatest] def matchSymbolToPredicateMethod[S <: AnyRef](left: S, right: Symbol, hasArticle: Boolean, articleIsA: Boolean): MatchResult = {

    // If 'empty passed, rightNoTick would be "empty"
    val propertyName = right.name

    accessProperty(left, right, true) match {

      case None =>

        // if propertyName is '>, mangledPropertyName would be "$greater"
        val mangledPropertyName = transformOperatorChars(propertyName)

        // methodNameToInvoke would also be "empty"
        val methodNameToInvoke = mangledPropertyName

        // methodNameToInvokeWithIs would be "isEmpty"
        val methodNameToInvokeWithIs = "is"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

        val firstChar = propertyName(0).toLower
        val methodNameStartsWithVowel = firstChar == 'a' || firstChar == 'e' || firstChar == 'i' ||
          firstChar == 'o' || firstChar == 'u'

        throw newTestFailedException(
          FailureMessages(
            if (methodNameStartsWithVowel) "hasNeitherAnOrAnMethod" else "hasNeitherAOrAnMethod",
            left,
            UnquotedString(methodNameToInvoke),
            UnquotedString(methodNameToInvokeWithIs)
          )
        )

      case Some(result) =>

        val (wasNot, was) =
          if (hasArticle) {
            if (articleIsA) ("wasNotA", "wasA") else ("wasNotAn", "wasAn")
          }
          else ("wasNot", "was")

        MatchResult(
          result == true, // Right now I just leave the return value of accessProperty as Any
          FailureMessages(wasNot, left, UnquotedString(propertyName)),
          FailureMessages(was, left, UnquotedString(propertyName))
        )
    }
  }
}
