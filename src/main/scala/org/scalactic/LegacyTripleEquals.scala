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

import TripleEqualsSupport._

/**
 * Provides <code>===</code> and <code>!==</code> operators that return <code>Boolean</code>, delegate the equality determination
 * to an <code>Equality</code> type class, and require no relationship between the types of the two values compared. 
 * 
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>LegacyTripleEquals</code> is useful (in test, not production, code) when you need to determine equality for a type of object differently than its
 * <code>equals</code> method: either you can't change the <code>equals</code> method, or the <code>equals</code> method is sensible generally, but
 * you are in a special situation where you need something else. If you also want a compile-time type check, however, you should use one
 * of <code>LegacyTripleEquals</code> sibling traits: 
 * <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a> or <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>.
 * </td></tr></table>
 *
 * <p>
 * <em>
 * Note: This trait is extended by <code>org.scalatest.Assertions</code>, because it provides the same kind of <code>===</code> operator that was
 * historically provided by <code>Assertions</code>.
 * The purpose of this trait is to maintain compatibility with existing ScalaTest code that uses the original <code>===</code> operator. After
 * ScalaTest no longer supports Scala 2.9, the &ldquo;legacy&rdquo; triple equals traits will be deprecated and eventually removed. Good error messages will
 * be obtained for both <code>==</code> and <code>===</code> through assert macros. In the transition phase, you can in production code use regular triple equals traits, 
 * whose <code>===</code> operators return <code>Boolean</code>, and in test code use "legacy" triple equals traits, whose <code>===</code>
 * operators return <code>Option[String]</code>.
 * </em>
 * </p>
 *
 * <p>
 * This trait will override or hide implicit methods defined by its sibling traits,
 * <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a> or <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>,
 * and can therefore be used to temporarily turn off type checking in a limited scope.
 * Because the methods in <code>LegacyTripleEquals</code> (and its siblings) <em>override</em> all the methods defined in
 * supertype <a href="TripleEqualsSupport.html"><code>TripleEqualsSupport</code></a>, you can achieve the same
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
@deprecated("org.scalactic.LegacyTripleEquals has been deprecated and will be removed in a future version of ScalaTest. If you need this, please copy the source code into your own trait instead.")
trait LegacyTripleEquals extends TripleEqualsSupport {

  import scala.language.implicitConversions

  implicit override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): Constraint[A, B] = new EqualityConstraint[A, B](equalityOfA)

  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  implicit override def convertToLegacyEqualizer[T](left: T): LegacyEqualizer[T] = new LegacyEqualizer(left)
  override def convertToLegacyCheckingEqualizer[T](left: T): LegacyCheckingEqualizer[T] = new LegacyCheckingEqualizer(left)

  override def lowPriorityTypeCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], ev: A <:< B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def convertEquivalenceToAToBConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A <:< B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def typeCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
  override def convertEquivalenceToBToAConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)

  override def lowPriorityConversionCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], cnv: A => B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, cnv)
  override def convertEquivalenceToAToBConversionConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A => B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def conversionCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], cnv: B => A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, cnv)
  override def convertEquivalenceToBToAConversionConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B => A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)


  /**
   * Implicit conversion from <code>Any</code> to <code>Equalizer</code>, used to enable
   * assertions with <code>===</code> comparisons.
   *
   * <p>
   * For more information
   * on this mechanism, see the <a href="Suite.Equalizer.html">documentation for </code>Equalizer</code></a>.
   * </p>
   *
   * <p>
   * Because trait <code>Suite</code> mixes in <code>Assertions</code>, this implicit conversion will always be
   * available by default in ScalaTest <code>Suite</code>s. This is the only implicit conversion that is in scope by default in every
   * ScalaTest <code>Suite</code>. Other implicit conversions offered by ScalaTest, such as those that support the matchers DSL
   * or <code>invokePrivate</code>, must be explicitly invited into your test code, either by mixing in a trait or importing the
   * members of its companion object. The reason ScalaTest requires you to invite in implicit conversions (with the exception of the
   * implicit conversion for <code>===</code> operator)  is because if one of ScalaTest's implicit conversions clashes with an
   * implicit conversion used in the code you are trying to test, your program won't compile. Thus there is a chance that if you
   * are ever trying to use a library or test some code that also offers an implicit conversion involving a <code>===</code> operator,
   * you could run into the problem of a compiler error due to an ambiguous implicit conversion. If that happens, you can turn off
   * the implicit conversion offered by this <code>convertToEqualizer</code> method simply by overriding the method in your
   * <code>Suite</code> subclass, but not marking it as implicit:
   * </p>
   *
   * <pre class="stHighlight">
   * // In your Suite subclass
   * override def convertToEqualizer(left: Any) = new Equalizer(left)
   * </pre>
   * 
   * @param left the object whose type to convert to <code>Equalizer</code>.
   * @throws NullPointerException if <code>left</code> is <code>null</code>.
   */
  // implicit override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
}

/**
 * Companion object to trait <code>LegacyTripleEquals</code> that facilitates the importing of <code>LegacyTripleEquals</code> members as 
 * an alternative to mixing it in. One use case is to import <code>LegacyTripleEquals</code> members so you can use
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
 * scala&gt; import LegacyTripleEquals._
 * import LegacyTripleEquals._
 *
 * scala&gt; 1 + 1 === 2
 * res0: Option[String] = None
 * </pre>
 */
object LegacyTripleEquals extends LegacyTripleEquals
