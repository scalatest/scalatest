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

import TripleEqualsSupport._

/**
 * Trait that can be mixed into a <code>Suite</code> to disable the implicit conversions provided by default in trait
 * <a href="Assertions.html"><code>Assertions</code></a>, which trait <code>Suite</code> extends.
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
 * You can write tests using <code>assert</code> (without triple equals), <code>assertResult</code>, <code>assertThrows</code>,
 * <code>intercept</code>, <code>assertCompiles</code>, <code>assertDoesNotCompile</code>, and <code>assertTypeError</code>:
 * </p>
 *
 * <pre class="stHighlight">
 *   assert(a &lt; 7)
 *
 *   assertResult(2) { 1 + 1 }
 *
 *   assertThrows[IndexOutOfBoundsException] {
 *     "hi".charAt(-1)
 *   }
 *
 *   val caught =
 *     intercept[IndexOutOfBoundsException] {
 *       "hi".charAt(-1)
 *     }
 *
 *   assertDoesNotCompile("val a: String = 1")
 *
 *   assertTypeError("val a: String = 1")
 *
 *   assertCompiles("val a: Int = 1")
 * </pre>
 *
 * @author Chua Chee Seng
 * @author Bill Venners
 */
trait NonImplicitAssertions extends Assertions {

  /**
   * Overrides the <code>super</code> implementation of <code>convertToEqualizer</code>, turning off the implicit 
   * modifier (if present) to remove the method from the space of implicit conversions.
   *
   * @param left the object whose type to convert to <code>Equalizer</code>.
   * @throws NullArgumentException if <code>left</code> is <code>null</code>.
   */
  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def lowPriorityTypeCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], ev: A <:< B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def convertEquivalenceToAToBConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A <:< B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def typeCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
  override def convertEquivalenceToBToAConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)

  /**
   * <strong>The <code>lowPriorityConversionCheckedConstraint</code> method has been deprecated and will be removed in a future version of Scalactic. It
   * is no longer needed now that the deprecation period of <code>ConversionCheckedTripleEquals</code> has expired. It will not be replaced.</strong>
   *
   * <p>
   * Provides an <code>A CanEqual B</code> instance for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>A</code> is
   * implicitly convertible to <code>B</code>, given an implicit <code>Equivalence[B]</code>.
   * </p>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[B]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="LowPriorityConversionCheckedConstraint.html"><code>LowPriorityConversionCheckedConstraint</code></a> (extended by
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>), and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equalityOfB an <code>Equivalence[B]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>A</code> to </code>B</code>
   * @return an <code>A CanEqual B</code> instance whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[B]</code>.
   */
  @deprecated("The lowPriorityConversionCheckedConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def lowPriorityConversionCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], cnv: A => B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, cnv)

  /**
   * <strong>The <code>convertEquivalenceToAToBConversionConstraint</code> method has been deprecated and will be removed in a future version of Scalactic.
   * It is no longer needed now that the deprecation period of <code>ConversionCheckedTripleEquals</code> has expired. It will not be replaced.</strong>
   *
   * <p>
   * Provides an <code>A CanEqual B</code> instance for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>A</code> is
   * implicitly convertible to <code>B</code>, given an <em>explicit</em> <code>Equivalence[B]</code>.
   * </p>
   *
   * <p>
   * This method is used to enable the <a href="Explicitly.html"><code>Explicitly</code></a> DSL for
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> by requiring an explicit <code>Equivalance[B]</code>, but
   * taking an implicit function that converts from <code>A</code> to </code>B</code>.
   * </p>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[B]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="LowPriorityConversionCheckedConstraint.html"><code>LowPriorityConversionCheckedConstraint</code></a> (extended by
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>), and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equalityOfB an <code>Equivalence[B]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>A</code> to </code>B</code>
   * @return an <code>A CanEqual B</code> instance whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[B]</code>.
   */
  @deprecated("The convertEquivalenceToAToBConversionConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def convertEquivalenceToAToBConversionConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A => B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)

  /**
   * <strong>The <code>conversionCheckedConstraint</code> method has been deprecated and will be removed in a future version of Scalactic. It
   * is no longer needed now that the deprecation period of <code>ConversionCheckedTripleEquals</code> has expired. It will not be replaced.</strong>
   *
   * <p>
   * Provides an <code>A CanEqual B</code> instance for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>B</code> is
   * implicitly convertible to <code>A</code>, given an implicit <code>Equivalence[A]</code>.
   * </p>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[A]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>) and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equivalenceOfA an <code>Equivalence[A]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>B</code> to </code>A</code>
   * @return an <code>A CanEqual B</code> instance whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[A]</code>.
   */
  @deprecated("The conversionCheckedConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def conversionCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], cnv: B => A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, cnv)

  /**
   * <strong>The <code>convertEquivalenceToBToAConversionConstraint</code> method has been deprecated and will be removed in a future version of Scalactic.
   * It is no longer needed now that the deprecation period of <code>ConversionCheckedTripleEquals</code> has expired. It will not be replaced.</strong>
   *
   * <p>
   * Provides an <code>A CanEqual B</code> instance for any two types <code>A</code> and <code>B</code>, enforcing the type constraint that <code>B</code> is
   * implicitly convertible to <code>A</code>, given an <em>explicit</em> <code>Equivalence[A]</code>.
   * </p>
   *
   * <p>
   * This method is used to enable the <a href="Explicitly.html"><code>Explicitly</code></a> DSL for
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> by requiring an explicit <code>Equivalance[A]</code>, but
   * taking an implicit function that converts from <code>B</code> to </code>A</code>. For example, under <code>ConversionCheckedTripleEquals</code>,
   * this method (as an implicit method), would be used to compile this statement:
   * </p>
   *
   * <pre class="stHighlight">
   * def closeEnoughTo1(num: Double): Boolean =
   *   (num === 1.0)(decided by forgivingEquality)
   * </pre>
   *
   * <p>
   * The returned <code>Constraint</code>'s <code>areEqual</code> method uses the implicitly passed <code>Equivalence[A]</code>'s
   * <code>areEquivalent</code> method to determine equality.
   * </p>
   *
   * <p>
   * This method is overridden and made implicit by subtraits
   * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>) and
   * overriden as non-implicit by the other subtraits in this package.
   * </p>
   *
   * @param equivalenceOfA an <code>Equivalence[A]</code> type class to which the <code>Constraint.areEqual</code> method will delegate to determine equality.
   * @param cnv an implicit conversion from <code>B</code> to </code>A</code>
   * @return an <code>A CanEqual B</code> instance whose <code>areEqual</code> method delegates to the <code>areEquivalent</code> method of
   *     the passed <code>Equivalence[A]</code>.
   */
  @deprecated("The convertEquivalenceToBToAConversionConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def convertEquivalenceToBToAConversionConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B => A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
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

