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
 * Provides <code>===</code> and <code>!==</code> operators that return <code>Boolean</code>, delegate the equality determination
 * to an <code>Equality</code> type class, and require no relationship between the types of the two values compared. 
 * 
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>TripleEquals</code> is useful (in both production and test code) when you need determine equality for a type of object differently than its
 * <code>equals</code> method: either you can't change the <code>equals</code> method, or the <code>equals</code> method is sensible generally, but
 * you are in a special situation where you need something else. If you also want a compile-time type check, however, you should use one
 * of <code>TripleEquals</code> sibling traits: 
 * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> or <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a>.
 * </td></tr></table>
 *
 * <p>
 * This trait will override or hide implicit methods defined by its sibling traits,
 * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> or <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a>,
 * and can therefore be used to temporarily turn of type checking in a limited scope. Here's an example, in which <code>TypeCheckedTripleEquals</code> will
 * cause a compiler error:
 * </p>
 * 
 * <pre class=stHighlight>
 * import org.scalautils._
 * import TypeCheckedTripleEquals._
 *
 * object Example {
 *
 *   def cmp(a: Int, b: Long): Int = {
 *     if (a === b) 0       // This line won't compile
 *     else if (a &lt; b) -1
 *     else 1
 *   }
 *
 *  def cmp(s: String, t: String): Int = {
 *    if (s === t) 0
 *    else if (s &lt; t) -1
 *    else 1
 *  }
 * }
 * </pre>
 *
 * Because <code>Int</code> and <code>Long</code> are not in a subtype/supertype relationship, comparing <code>1</code> and <code>1L</code> in the context
 * of <code>TypeCheckedTripleEquals</code> will generate a compiler error:
 * </p>
 *
 * <pre>
 * Example.scala:9: error: types Int and Long do not adhere to the equality constraint selected for
 * the === and !== operators; they must either be in a subtype/supertype relationship, or, if
 * ConversionCheckedTripleEquals is in force, implicitly convertible in one direction or the other;
 * the missing implicit parameter is of type org.scalautils.EqualityConstraint[Int,Long]
 *     if (a === b) 0      // This line won't compile
 *           ^
 * one error found
 * </pre>
 * 
 * <p>
 * You can &ldquo;turn off&rdquo; the type checking locally by importing the members of <code>TripleEquals</code> in
 * a limited scope:
 * </p>
 * 
 * <pre>
 * package org.scalautils.examples.tripleequals
 * 
 * import org.scalautils._
 * import TypeCheckedTripleEquals._
 * 
 * object Example {
 * 
 *   def cmp(a: Int, b: Long): Int = {
 *     import TripleEquals._
 *     if (a === b) 0
 *     else if (a &lt; b) -1
 *     else 1
 *   }
 *
 *  def cmp(s: String, t: String): Int = {
 *    if (s === t) 0
 *    else if (s &lt; t) -1
 *    else 1
 *  }
 * }
 * </pre>
 *
 * <p>
 * With the above change, the <code>Example.scala</code> file compiles fine. Type checking is turned off only inside the first <code>cmp</code> method that
 * takes an <code>Int</code> and a <code>Long</code>. <code>TypeCheckedTripleEquals</code> is still enforcing its type constraint, for example, for the <code>s === t</code>
 * expression in the other overloaded <code>cmp</code> method that takes strings.
 * </p>
 * 
 * <p>
 * Because the methods in <code>TripleEquals</code> (and its siblings)<em>override</em> all the methods defined in
 * supertype <a href="EqualityConstraints.html"><code>EqualityConstraints</code></a>, you can achieve the same
 * kind of nested tuning of equality constraints whether you mix in traits, import from companion objects, or use some combination of both.
 * </p>
 *
 * <p>
 * In short, you should be able to select a primary constraint level via either a mixin or import, then change that in nested scopes
 * however you want, again either through a mixin or import, without getting any implicit conversion ambiguity. The innermost constraint level in scope
 * will always be in force.
 * <p>
 *
 * @author Bill Venners
 */
trait TripleEquals extends EqualityConstraints {

  // Inherit the Scaladoc for these methods

  implicit override def defaultEquality[A]: Equality[A] = new DefaultEquality[A]

  implicit override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def convertToLegacyEqualizer[T](left: T): LegacyEqualizer[T] = new LegacyEqualizer(left)
  override def convertToLegacyCheckingEqualizer[T](left: T): LegacyCheckingEqualizer[T] = new LegacyCheckingEqualizer(left)

  implicit override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  override def lowPriorityTypeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: A <:< B): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)
  override def typeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: B <:< A): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  override def lowPriorityConversionCheckedEqualityConstraint[A, B](implicit equalityOfB: Equality[B], cnv: A => B): EqualityConstraint[A, B] = new AToBEqualityConstraint[A, B](equalityOfB, cnv)
  override def conversionCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], cnv: B => A): EqualityConstraint[A, B] = new BToAEqualityConstraint[A, B](equalityOfA, cnv)
}

/**
 * Companion object to trait <code>TripleEquals</code> that facilitates the importing of <code>TripleEquals</code> members as 
 * an alternative to mixing it in. One use case is to import <code>TripleEquals</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalatest.jar
 * Welcome to Scala version 2.10.0
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalautils._
 * import org.scalautils._
 *
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 *
 * scala&gt; 1 + 1 === 2
 * res0: Boolean = true
 * </pre>
 */
object TripleEquals extends TripleEquals

