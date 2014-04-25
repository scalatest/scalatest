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
 * to an <code>Equality</code> type class, and require that either the types of the two values compared are in a subtype/supertype
 * relationship, or that an implicit conversion is available that can convert from one type to the other.
 * 
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>ConversionCheckedLegacyTripleEquals</code> is useful (in test, not production, code) when you need determine equality for a type of object differently than
 * its <code>equals</code>
 * method&#8212;either you can't change the <code>equals</code> method, or the <code>equals</code> method is sensible generally, but you're in a special situation where you
 * need something else&#8212;and/or you want a compile-time type check that allows types that are implicitly convertable in either (or both) directions.
 * </td></tr></table>
 *
 * <p>
 * <em>
 * Note: The purpose of this trait is to maintain compatibility with existing ScalaTest code that uses the original <code>===</code> operator. After
 * ScalaTest no longer supports Scala 2.9, the &ldquo;legacy&rdquo; triple equals traits will be deprecated and eventually removed. Good error messages will
 * be obtained for both <code>==</code> and <code>===</code> through assert macros. In the transition phase, you can in production code use regular triple equals traits, 
 * whose <code>===</code> operators return <code>Boolean</code>, and in test code use "legacy" triple equals traits, whose <code>===</code>
 * operators return <code>Option[String]</code>.
 * </em>
 * </p>
 *
 * <p>
 * This trait is the middle ground of the three <em>legacy triple equals</em> traits, in between
 * <code>LegacyTripleEquals</code>, the most lenient, and <code>TypeCheckedLegacyTripleEquals</code>, the most strict.
 * If <code>LegacyTripleEquals</code> is mixed in or imported, the <code>===</code> can be used with any two types
 * and still compile. If <code>TypeCheckedLegacyTripleEquals</code> is mixed in or imported, however, only types in 
 * a subtype or supertype relationship with each other (including when both types are exactly the same) will compile.
 * <code>ConversionCheckedLegacyTripleEquals</code> is slightly more accomodating, because in addition to compiling any
 * use of <code>===</code> that will compile under </code>TypeCheckedLegacyTripleEquals</code>, it will also compile
 * type types that would be rejected by <code>TypeCheckedLegacyTripleEquals</code>, so long as an implicit
 * conversion (in either direction) from one type to another is available.
 * </p>
 *
 * <p>
 * For example, under <code>TypeCheckedLegacyTripleEquals</code>, the following use of <code>===</code> will not compile,
 * because <code>Int</code> and <code>Long</code> are not in a subtype/supertype relationship. (<em>I.e.</em>, <code>Int</code>
 * is not a subtype or supertype of <code>Long</code>):
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 * 
 * scala&gt; import TypeCheckedLegacyTripleEquals._
 * import TypeCheckedLegacyTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * &lt;console&gt;:14: error: types Int and Long do not adhere to the equality constraint selected for
 * the === and !== operators; they must either be in a subtype/supertype relationship, or, if
 * ConversionCheckedLegacyTripleEquals is in force, implicitly convertible in one direction or the other;
 * the missing implicit parameter is of type org.scalactic.Constraint[Int,Long]
 *               1 === 1L
 *                 ^
 * </pre>
 *
 * <p>
 * Trait <code>TypeCheckedLegacyTripleEquals</code> rejects types <code>Int</code> and <code>Long</code> because they are not directly related via
 * subtyping. However, an implicit widening conversion from <code>Int</code> to </code>Long</code> does exist (imported implicitly from
 * <code>scala.Predef</code>), so <code>ConversionCheckedLegacyTripleEquals</code>
 * will allow it:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import ConversionCheckedLegacyTripleEquals._
 * import ConversionCheckedLegacyTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * res1: Option[String = None
 * </pre>
 * 
 * <p>
 * The implicit conversion can go in either direction: from the left type to the right type, or vice versa. In the above expression the 
 * implicit conversion goes from left to right (the <code>Int</code> on the left to the <code>Long</code> on the right). It also works
 * the other way:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; 1L === 1
 * res2: Option[String] = None
 * </pre>
 * 
 * <p>
 * This trait will override or hide implicit methods defined by its sibling traits,
 * <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a> or <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>,
 * and can therefore be used to temporarily turn on or off conversion checking in a limited scope.
 * Because the methods in <code>ConversionCheckedLegacyTripleEquals</code> (and its siblings)
 * <em>override</em> all the methods defined in supertype <a href="TripleEqualsSupport.html"><code>TripleEqualsSupport</code></a>, you can achieve the same
 * kind of nested tuning of equality constraints whether you mix in traits, import from companion objects, or use some combination of both.
 * </p>
 *
 * <p>
 * In short, you should be able to select a primary constraint level via either a mixin or import, then change that in nested scopes
 * however you want, again either through a mixin or import, without getting any implicit conversion ambiguity. The innermost constraint level in scope
 * will always be in force.
 * <p>
 *
 * <p>
 * An alternative way to solve an unwanted compiler error caused by an over-zealous type constraint is with a <em>widening type ascription</em>. Here
 * are some examples:
 * </p>
 *
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import ConversionCheckedLegacyTripleEquals._
 * import ConversionCheckedLegacyTripleEquals._
 *
 * scala&gt; List(1, 2, 3) === Vector(1, 2, 3)
 * &lt;console&gt;:14: error: types List[Int] and scala.collection.immutable.Vector[Int] do not adhere to the equality constraint selected for the === and !== operators; the missing implicit parameter is of type org.scalactic.Constraint[List[Int],scala.collection.immutable.Vector[Int]]
 *               List(1, 2, 3) === Vector(1, 2, 3)
 *                             ^
 * </pre>
 *
 * <p>
 * Although you could solve the above type error with <a href="TraversableEqualityConstraints.html"><code>TraversableEqualityConstraints</code></a>, you could also
 * simply widen the type of one side or the other to <code>Any</code>. Because <code>Any</code> is a supertype of everything, the
 * type constraint will be satisfied:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; List(1, 2, 3) === (Vector(1, 2, 3): Any)
 * res1: Option[String] = None
 *
 * scala&gt; (List(1, 2, 3): Any) === Vector(1, 2, 3)
 * res2: Option[String] = None
 * </pre>
 *
 * <p>
 * You could alternatively widen a type to a more specific common supertype than <code>Any</code>. For example, since <code>List[Int]</code> and
 * <code>Vector[Int]</code> are both subtypes of <code>Seq[Int]</code>, so you could widen either type to <code>Seq[Int]</code> to satisfy
 * the type checker:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; List(1, 2, 3) === (Vector(1, 2, 3): Seq[Int])
 * res3: Option[String] = None
 *
 * scala&gt; (List(1, 2, 3): Seq[Int]) === Vector(1, 2, 3)
 * res4: Option[String] = None
 * </pre>
 * 
 * @author Bill Venners
 */
@deprecated("org.scalactic.ConversionCheckedLegacyTripleEquals has been deprecated and will be removed in a future version of ScalaTest. If you need this, please copy the source code into your own trait instead.")
trait ConversionCheckedLegacyTripleEquals extends LowPriorityConversionCheckedConstraint {

  import scala.language.implicitConversions

  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  override def convertToLegacyEqualizer[T](left: T): LegacyEqualizer[T] = new LegacyEqualizer(left)
  override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)
  implicit override def convertToLegacyCheckingEqualizer[T](left: T): LegacyCheckingEqualizer[T] = new LegacyCheckingEqualizer(left)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): Constraint[A, B] = new EqualityConstraint[A, B](equalityOfA)

  override def lowPriorityTypeCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], ev: A <:< B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def convertEquivalenceToAToBConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A <:< B): Constraint[A, B] = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def typeCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
  override def convertEquivalenceToBToAConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)

  implicit override def conversionCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], cnv: B => A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, cnv)
  implicit override def convertEquivalenceToBToAConversionConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B => A): Constraint[A, B] = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
}

/**
 * Companion object to trait <code>ConversionCheckedLegacyTripleEquals</code> that facilitates the importing of <code>ConversionCheckedLegacyTripleEquals</code> members as 
 * an alternative to mixing it in. One use case is to import <code>ConversionCheckedLegacyTripleEquals</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalactic.jar
 * Welcome to Scala version 2.10.0
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 * 
 * scala&gt; import ConversionCheckedLegacyTripleEquals._
 * import ConversionCheckedLegacyTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * res0: Option[String] = None
 * </pre>
 */
object ConversionCheckedLegacyTripleEquals extends ConversionCheckedLegacyTripleEquals

