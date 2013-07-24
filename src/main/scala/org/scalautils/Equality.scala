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
package org.scalautils

/**
 * Defines a custom way to determine equality for a type.
 *
 * <p>
 * <code>Equality</code> enables you to define alternate notions of equality for types that can be used
 * with ScalaUtil's <code>===</code> and <code>!==</code> syntax and ScalaTest's matcher syntax. 
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
 * scala&gt; import org.scalautils._
 * import org.scalautils._
 *
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 *
 * scala&gt; Person("Joe", 29.0001) === Person("Joe", 29.0)
 * res0: Boolean = false
 * </pre>
 *
 * <p>
 * The <code>===</code> operator looks for an implicit <code>Equality[L]</code>, where <code>L</code> is the left-hand type: in this
 * case, <code>Person</code>. Because you didn't specifically provide an implicit <code>Equality[Person]</code>, <code>===</code> will fall back on
 * <a href="#defaultEquality">default equality</a>, which will call <code>Person</code>'s <code>equals</code> method. That <code>equals</code> method, provided by the Scala compiler
 * because <code>Person</code> is a case class, will declare these two objects unequal because 29.001 does not exactly equal 29.0.
 * </p>
 * 
 * <p>
 * To make the equality check more forgiving, you could define an implicit <code>Equality[Person]</code> that compares
 * the <code>age</code> <code>Double</code>s with a tolerance, like this:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import Tolerance._
 * import Tolerance._
 * 
 * scala&gt; implicit val personEq = 
 *      |   new Equality[Person] {
 *      |     def areEqual(a: Person, b: Any): Boolean =
 *      |       b match {
 *      |         case p: Person =&gt; a.name == p.name &amp;&amp; a.age === p.age +- 0.0002
 *      |         case _ =&gt; false
 *      |       }
 *      |   }
 * personEq: org.scalautils.Equality[Person] = $anon$1@2b29f6e7
 * </pre>
 *
 * <p>
 * Now the <code>===</code> operator will use your more forgiving <code>Equality[Person]</code> for the equality check instead
 * of default equality:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; Person("Joe", 29.0001) === Person("Joe", 29.0)
 * res1: Boolean = true
 * </pre>
 *
 * <a name="defaultEquality"></a>
 * <h2>Default equality</h2>
 *
 * <p>
 * ScalaUtils defines a default <code>Equality[T]</code> for all types <code>T</code> whose <code>areEqual</code> method works by first
 * calling <code>.deep</code> on any passed array, then calling <code>==</code> on the left-hand object, passing in the right-hand object.
 * You can obtain a default equality via the <code>default</code> method of the <a href="Equality$.html">Equality companion object</a>,
 * or from the <code>defaultEquality</code> method defined in <a href="TripleEqualsSupport.html"</a><code>TripleEqualsSupport</code></a>.
 * </p>
 *
 * <a name="aboutEquality"></a>
 * <h2>About equality and equivalence</h2>
 *
 * <p>
 * The <code>Equality</code> trait represents the Java Platform's native notion of equality, as expressed in the signature and contract of
 * the <code>equals</code> method of <code>java.lang.Object</code>. Essentially, trait <code>Equality</code> enables you to write alternate
 * <code>equals</code> method implementations for a type outside its defining class.
 * </p>
 *
 * <p>
 * In an <code>equals</code> method, the left-hand type is known to be the type of <code>this</code>, but
 * the right-hand type is <code>Any</code>.
 * As a result, you would normally perform a runtime type test to determine whether the right-hand object is of an appropriate type for equality,
 * and if so, compare it structurally for equality with the left-hand (<code>this</code>) object.
 * An an illustration, here's a possible <code>equals</code>
 * implementation for the <code>Person</code> case class shown in the earlier example:
 * </p>
 *
 * <pre class="stHighlight">
 * override def equals(other: Any): Boolean = 
 *   other match {
 *     case p: Person =&gt; name = p.name &amp;&amp; age = p.age
 *     case _ =&gt; false
 *   }
 * </pre>
 *
 * <p>
 * The <code>areEquals</code> method of <code>Equality[T]</code> is similar. The left-hand type is known to be <code>T</code>, but the right-hand type is <code>Any</code>, so
 * normally you'd need to do a runtime type test in your <code>areEqual</code> implementation.
 * Here's the <code>areEqual</code> method implementation from the earlier <code>Equality[Person]</code> example:
 * </p>
 *
 * <pre class="stHighlight">
 * def areEqual(a: Person, b: Any): Boolean =
 *   b match {
 *     case p: Person =&gt; a.name == p.name &amp;&amp; a.age === p.age +- 0.0002
 *     case _ =&gt; false
 *   }
 * </pre>
 *
 * <p>
 * </p>
 *
 * <em>Note: The <code>Equality</code> type class was inspired in part by the <code>Equal</code> type class of the 
 * <a href="http://code.google.com/p/scalaz/" target="_blank"><code>scalaz</code></a> project.</em>
 * </p>
 *
 * @tparam A the type whose equality is being customized
 */
trait Equality[A] extends Equivalence[A] {

  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal.
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @return true if the passed objects are "equal," as defined by this <code>Equality</code> instance
   */
  def areEqual(a: A, b: Any): Boolean

  final def areEquivalent(a: A, b: A): Boolean = areEqual(a, b)
} 

object Equality {

  def apply[A](uniformity: Uniformity[A]): Equality[A] = {
    new NormalizingEquality[A] {
      def normalized(a: A): A = uniformity.normalized(a)
      def normalizedCanHandle(b: Any): Boolean = uniformity.normalizedCanHandle(b)
      def normalizedOrSame(b: Any): Any = uniformity.normalizedOrSame(b)
    }
  }

  /**
   * A default <code>Equality</code> implementation (which can be used for any type) whose
   * <code>areEqual</code> method first calls <code>.deep</code> on any array (on either the left or right side),
   * then compares the resulting objects with <code>==</code>.
   */
  implicit def default[A]: Equality[A] = new DefaultEquality[A]
}

