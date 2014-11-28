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

import EqualityPolicy._

/**
 * Provides <code>===</code> and <code>!==</code> operators that return <code>Boolean</code>, delegate the equality determination
 * to an <code>Equality</code> type class, and require no relationship between the types of the two values compared. 
 * 
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>UncheckedEquality</code> is useful (in both production and test code) when you need determine equality for a type of object differently than its
 * <code>equals</code> method: either you can't change the <code>equals</code> method, or the <code>equals</code> method is sensible generally, but
 * you are in a special situation where you need something else. If you also want a compile-time type check, however, you should use one
 * of <code>UncheckedEquality</code> sibling traits: 
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
 * <pre class="stHighlight">
 * import org.scalactic._
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
 * the missing implicit parameter is of type org.scalactic.Constraint[Int,Long]
 *     if (a === b) 0      // This line won't compile
 *           ^
 * one error found
 * </pre>
 * 
 * <p>
 * You can &ldquo;turn off&rdquo; the type checking locally by importing the members of <code>UncheckedEquality</code> in
 * a limited scope:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalactic.examples.tripleequals
 * 
 * import org.scalactic._
 * import TypeCheckedTripleEquals._
 * 
 * object Example {
 * 
 *   def cmp(a: Int, b: Long): Int = {
 *     import UncheckedEquality._
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
 * Because the methods in <code>UncheckedEquality</code> (and its siblings)<em>override</em> all the methods defined in
 * supertype <a href="EqualityPolicy.html"><code>EqualityPolicy</code></a>, you can achieve the same
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
trait UncheckedEquality extends EqualityPolicy {

  import scala.language.implicitConversions

  // Inherit the Scaladoc for these methods

  implicit override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)
  override def convertToFreshCheckingEqualizer[T](left: T): FreshCheckingEqualizer[T] = new FreshCheckingEqualizer(left)

  override def numericEqualityConstraint[A, B](implicit equalityOfA: Equality[A], numA: CooperatingNumeric[A], numB: CooperatingNumeric[B]): EqualityConstraint[A, B] with NativeSupport = new BasicEqualityConstraint[A, B](equalityOfA)
  override def booleanEqualityConstraint[A, B](implicit equalityOfA: Equality[A], boolA: CooperatingBoolean[A], boolB: CooperatingBoolean[B]): EqualityConstraint[A, B] with NativeSupport = new BasicEqualityConstraint[A, B](equalityOfA)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): Constraint[A, B] = new BasicConstraint[A, B](equalityOfA)
  implicit override def unconstrainedFreshEquality[A, B](implicit equalityOfA: Equality[A]): EqualityConstraint[A, B] with NativeSupport = new BasicEqualityConstraint[A, B](equalityOfA)

  override def lowPriorityTypeCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], ev: A <:< B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def convertEquivalenceToAToBConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A <:< B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def typeCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
  override def convertEquivalenceToBToAConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)

  override def lowPriorityCheckedEqualityConstraint[A, B](implicit equivalenceOfB: Equivalence[B], ev: A <:< B): EqualityConstraint[A, B] with NativeSupport = new ASubtypeOfBEqualityConstraint[A, B](equivalenceOfB, ev)
  override def convertEquivalenceToASubtypeOfBEqualityConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A <:< B): EqualityConstraint[A, B] with NativeSupport = new ASubtypeOfBEqualityConstraint[A, B](equivalenceOfB, ev)
  override def checkedEqualityConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): EqualityConstraint[A, B] with NativeSupport = new BSubtypeOfAEqualityConstraint[A, B](equivalenceOfA, ev)
  override def convertEquivalenceToBSubtypeOfAEqualityConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): EqualityConstraint[A, B] with NativeSupport = new BSubtypeOfAEqualityConstraint[A, B](equivalenceOfA, ev)

  override def lowPriorityConversionCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], cnv: A => B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, cnv)
  override def convertEquivalenceToAToBConversionConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A => B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def conversionCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], cnv: B => A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, cnv)
  override def convertEquivalenceToBToAConversionConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B => A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)

  // For EnabledEquality
  override def enabledEqualityConstraintFor[A](implicit equivalenceOfA: Equivalence[A], ev: EnabledEqualityFor[A]): EqualityConstraint[A, A] with NativeSupport = new EnabledEqualityConstraint[A](equivalenceOfA)
  override def lowPriorityEnabledEqualityConstraintBetween[B, A](implicit equalityOfB: Equality[B], ev: EnabledEqualityBetween[A, B]): EqualityConstraint[B, A] = new BasicEqualityConstraint[B, A](equalityOfB)
  override def enabledEqualityConstraintBetween[A, B](implicit equalityOfA: Equality[A], ev: EnabledEqualityBetween[A, B]): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)
  override def lowPriorityEnabledEqualityConstraintConverting[A, B](implicit equivalenceOfB: Equivalence[B], cnv: EnabledEqualityConverting[A, B]): EqualityConstraint[A, B] = new AToBEnabledEqualityConstraint[A, B](equivalenceOfB, cnv)
  override def enabledEqualityConstraintConverting[A, B](implicit equivalenceOfA: Equivalence[A], cnv: EnabledEqualityConverting[B, A]): EqualityConstraint[A, B] = new BToAEnabledEqualityConstraint[A, B](equivalenceOfA, cnv)
}

/**
 * Companion object to trait <code>UncheckedEquality</code> that facilitates the importing of <code>UncheckedEquality</code> members as 
 * an alternative to mixing it in. One use case is to import <code>UncheckedEquality</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalatest.jar
 * Welcome to Scala version 2.10.0
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import UncheckedEquality._
 * import UncheckedEquality._
 *
 * scala&gt; 1 + 1 === 2
 * res0: Boolean = true
 * </pre>
 */
object UncheckedEquality extends UncheckedEquality

