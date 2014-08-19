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

import org.scalactic._

import EqualityPolicy._

/**
 * Trait that can be mixed into a <code>Suite</code> to disable the lone implicit conversion provided by default in trait
 * <a href="Assertions.html"><code>Assertions</code></a>, which trait <code>Suite</code> extends.
 * 
 * <p>
 * Currently there is just one implicit conversion provided by default in <a href="Suite.html"><code>Suite</code></a>, the one that adds a <code>===</code> method to
 * anything. If more default implicits are added to <code>Suite</code> in future versions of ScalaTest, they will be added here as well so that
 * this trait will disable all of them.
 * </p>
 *
 * <p>
 * This trait can be used to quickly solve a problem in which ScalaTest's default implicit conversion is clashing with those of some other library
 * you need to use in your tests. After mixing in this trait, like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * class MySuite extends FunSuite with NonImplicitAssertions { 
 *   // ... 
 * } 
 * </pre>
 *
 * <p>
 * You can write tests using <code>assert</code> (without triple equals), <code>expect</code>, and <code>intercept</code>:
 * </p>
 *
 * <pre class="stHighlight">
 *   assert(a &lt; 7)
 *
 *   expect(2) { 1 + 1 }
 *
 *   intercept[IndexOutOfBoundsException] {
 *     "hi".charAt(-1)
 *   }
 * </pre>
 *
 * @author Chua Chee Seng
 * @author Bill Venners
 */
trait NonImplicitAssertions extends Assertions {

/*
  override def numericEqualityConstraint[A, B](implicit equalityOfA: Equality[A], numA: CooperatingNumeric[A], numB: CooperatingNumeric[B]): EqualityConstraint[A, B] with NativeSupport = new BasicEqualityConstraint[A, B](equalityOfA)

  // Turns out I need these to get matchers to work, because looking for EvidenceThat...
  implicit override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): Constraint[A, B] = new BasicConstraint[A, B](equalityOfA)
  implicit override def unconstrainedFreshEquality[A, B](implicit equalityOfA: Equality[A]): EqualityConstraint[A, B] with NativeSupport = new BasicEqualityConstraint[A, B](equalityOfA)
*/

  /**
   * Overrides the <code>super</code> implementation of <code>convertToEqualizer</code>, turning off the implicit 
   * modifier (if present) to remove the method from the space of implicit conversions.
   *
   * @param left the object whose type to convert to <code>Equalizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)
  override def convertToFreshCheckingEqualizer[T](left: T): FreshCheckingEqualizer[T] = new FreshCheckingEqualizer(left)

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

/*
  // For EnabledEquality
  override def enabledEqualityConstraintFor[A](implicit equivalenceOfA: Equivalence[A], ev: EnabledEqualityFor[A]): EqualityConstraint[A, A] with NativeSupport = new EnabledEqualityConstraint[A](equivalenceOfA)
  override def lowPriorityEnabledEqualityConstraintBetween[B, A](implicit equalityOfB: Equality[B], ev: EnabledEqualityBetween[A, B]): EqualityConstraint[B, A] = new BasicEqualityConstraint[B, A](equalityOfB)
  override def enabledEqualityConstraintBetween[A, B](implicit equalityOfA: Equality[A], ev: EnabledEqualityBetween[A, B]): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)
  override def lowPriorityEnabledEqualityConstraintConverting[A, B](implicit equivalenceOfB: Equivalence[B], cnv: EnabledEqualityConverting[A, B]): EqualityConstraint[A, B] = new AToBEnabledEqualityConstraint[A, B](equivalenceOfB, cnv)
  override def enabledEqualityConstraintConverting[A, B](implicit equivalenceOfA: Equivalence[A], cnv: EnabledEqualityConverting[B, A]): EqualityConstraint[A, B] = new BToAEnabledEqualityConstraint[A, B](equivalenceOfA, cnv)
*/
}

/**
 * Companion object that facilitates the importing of the members of trait <code>Assertions</code> without importing the implicit conversions
 * it provides by default.  One use case for this object is to import the non-implicit <code>Assertions</code> members so you can use
 * them in the Scala interpreter along with another library whose implicits conflict with those provided by <code>Assertions</code>:
 *
 * <pre class="stREPL">
 * $ scala -cp scalatest-1.7.jar
 * Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_29).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * 
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 * 
 * scala&gt; import NonImplicitAssertions._
 * import NonImplicitAssertions._
 * 
 * scala&gt; assert(1 + 1 === 2)
 * &lt;console&gt;:14: error: value === is not a member of Int
 *              assert(1 + 1 === 2)
 *                            ^
 * 
 * scala&gt; assert(1 + 1 == 2)
 *
 * scala&gt; expect(2) { 1 + 1 }
 * 
 * scala&gt; expect(2) { 1 + 1 + 1 }
 * org.scalatest.TestFailedException: Expected 2, but got 3
 *   at org.scalatest.Assertions$class.newAssertionFailedException(Assertions.scala:318)
 *   at org.scalatest.NonImplicitAssertions$.newAssertionFailedException(NonImplicitAssertions.scala:73)
 *   ...
 * 
 * scala&gt; intercept[IndexOutOfBoundsException] { "hi".charAt(-1) }
 * res3: IndexOutOfBoundsException = java.lang.StringIndexOutOfBoundsException: String index out of range: -1
 * </pre>
 */
object NonImplicitAssertions extends NonImplicitAssertions

