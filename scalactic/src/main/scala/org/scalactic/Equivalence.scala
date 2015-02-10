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
package org.scalactic

/**
 * Defines a custom way to determine equality for a type when compared with another value of the same type.
 *
 * <p>
 * <code>Equivalence</code> enables you to define alternate notions of equality for types that can be used
 * with ScalaUtil's <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> and
 * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>
 * traits. These traits can be used to perform equality comparisons with type constraints enforced at
 * compile time using ScalaUtil's <code>===</code> and <code>!==</code> syntax
 * and ScalaTest's <code>should</code> <code>===</code> syntax of <code>Matchers</code> trait. 
 * </p>
 *
 * <a name="equivalanceLaws"></a>
 * <h2>Equivalence laws</h2>
 *
 * <p>
 * Instances of <code>Equivalence[T]</code> define an equivalence relation on non-<code>null</code> instance of type <code>T</code>
 * that must adhere to the following laws (which are consistent with those of <code>equals</code> on <code>java.lang.Object</code>):
 * </p>
 *
 * <ul>
 * <li><em>reflexivity</em>: for any non-<code>null</code> value <code>x</code>, <code>areEqual(x, x)</code> must return <code>true.</code></li>
 * <li><em>symmetry</em>: for any non-<code>null</code> values <code>x</code> and <code>y</code>, <code>areEqual(x, y)</code> must return <code>true</code> if and only if <code>areEqual(y, x)</code> returns <code>true</code>.</li>
 * <li><em>transitivity</em>: for any non-<code>null</code> values <code>x</code>, <code>y</code>, and <code>z</code>, if <code>areEqual(x, y)</code> returns <code>true</code> and <code>areEqual(y, z)</code> returns <code>true</code>, then <code>areEqual(x, z)</code> must return <code>true</code>.</li>
 * <li><em>consistency</em>: for any non-<code>null</code> values <code>x</code> and <code>y</code>, multiple invocations of <code>areEqual(x, y)</code> consistently return <code>true</code> or consistently return <code>false</code>, provided no information used in the equality comparison on the objects is modified.</li>
 * <li><em>right-null</em>: For any non-<code>null</code> value <code>x</code>, <code>areEqual(x, null)</code> must return <code>false</code>.</li>
 * <li><em>left-null</em>: For any non-<code>null</code> value <code>x</code>, <code>areEqual(null, x)</code> must return <code>false</code>.</li>
 * <li><em>both-null</em>: <code>areEqual(null, null)</code> must return <code>true</code>.</li>
 * </ul>
 *
 * <a name="equivalenceAndEquality"></a>
 * <h2>Equivalence and equality</h2>
 *
 * <p>
 * Because <a href="Equality.html"><code>Equality</code></a> extends <code>Equivalence</code>, you automatically
 * define an <code>Equivalence[T]</code> when you define an <code>Equality[T]</code>. Most often you will usually
 * want to define custom <code>Equality</code>s, because they will be more generally useful: they are also
 * used by Scalactic's <a href="TripleEquals.html"><code>TripleEquals</code></a> trait and ScalaTest's
 * <code>equal</code>, <code>be</code>, and <code>contain</code> matcher syntax. However, if you really want
 * just an <code>Equivalence</code>, and writing an <code>Equality</code> is inconvenient, you can write
 * an <code>Equivalence</code> directly for a type.
 * </p>
 *
 * <p>
 * For example, say you have a case class that includes a <code>Double</code> value:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; case class Person(name: String, age: Double)
 * defined class Person
 * </pre>
 * 
 * <p>
 * Imagine you are calculating the <code>age</code> values in such as way that occasionally tests
 * are failing because of rounding differences that you actually don't care about. For example, you 
 * expect an age of 29.0, but you're sometimes seeing 29.0001:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; Person("Joe", 29.0001) === Person("Joe", 29.0)
 * res0: Boolean = false
 * </pre>
 *
 * <p>
 * The <code>===</code> operator of <code>TypeCheckedTripleEquals</code> looks for an implicit
 * <code>Equivalence[SUPER]</code>, where <code>SUPER</code> is either the left-hand or right-hand type, whichever
 * one is a supertype of the other. In this case, both sides are <code>Person</code> (which is considered a supertype of
 * itself), so the compiler will look for an <code>Equivalence[Person]</code>.
 * Because you didn't specifically provide an implicit <code>Equivalence[Person]</code>, <code>===</code> will fall back on
 * <a href="Equality.html#defaultEquality">default equality</a>, because an <code>Equality[Person]</code> <em>is-an</em> 
 * <code>Equivalence[Person]</code>. The default <code>Equality[Person]</code> will call <code>Person</code>'s
 * <code>equals</code> method. That <code>equals</code> method, provided by the Scala compiler
 * because <code>Person</code> is a case class, will declare these two objects unequal because 29.001 does not
 * exactly equal 29.0.
 * </p>
 * 
 * <p>
 * To make the equality check more forgiving, you could define an implicit <code>Equivalence[Person]</code> that compares
 * the <code>age</code> <code>Double</code>s with a tolerance, like this:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import Tolerance._
 * import Tolerance._
 * 
 * scala&gt; implicit val personEq = 
 *      |   new Equivalence[Person] {
 *      |     def areEquivalent(a: Person, b: Person): Boolean =
 *      |       a.name == b.name &amp;&amp; a.age === b.age +- 0.0002
 *      |   }
 * personEq: org.scalactic.Equivalence[Person] = $anon$1@7892bd8
 * </pre>
 *
 * <p>
 * Now the <code>===</code> operator will use your more forgiving <code>Equivalence[Person]</code> for the
 * equality check instead of default equality:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; Person("Joe", 29.0001) === Person("Joe", 29.0)
 * res1: Boolean = true
 * </pre>
 *
 */
trait Equivalence[T] {

  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal.
   *
   * <p>
   * Note: this <code>areEquivalent</code> method means essentially the same thing as the <code>areEqual</code> method
   * of trait <a href="Equality.html"><code>Equality</code></a>, the difference only being the static type of the
   * right-hand value. This method is named <code>areEquivalent</code> instead
   * of <code>areEqual</code> so that it can be implemented in terms of <code>areEqual</code> in trait
   * <code>Equality</code> (which extends <code>Equivalence</code>).
   * </p>
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @return true if the passed objects are "equal," as defined by this <code>Equivalence</code> instance
   */
  def areEquivalent(a: T, b: T): Boolean
} 

/**
 * Companion object for trait <code>Equivalence</code> that provides a factory method for producing 
 * default <code>Equivalence</code> instances.
 */ 
object Equivalence {

  /**
   * Provides default <code>Equivalence</code> implementations for the specified type whose
   * <code>areEqual</code> method first calls <code>.deep</code> on any <code>Array</code> (on either the left or right side),
   * then compares the resulting objects with <code>==</code>.
   *
   * @return a default <code>Equivalence[T]</code>
   */
  implicit def default[T]: Equivalence[T] = Equality.default[T]
}
