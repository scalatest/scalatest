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

/*
trait LowPriorityTypeCheckedLegacyConstraint extends EqualityConstraints {
  implicit override def lowPriorityTypeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: A <:< B): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)
}
*/

/**
 * Provides <code>===</code> and <code>!==</code> operators that return <code>Boolean</code>, delegate the equality determination
 * to an <code>Equality</code> type class, and require the types of the two values compared to be in a subtype/supertype
 * relationship.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>TypeCheckedLegacyTripleEquals</code> is useful (in test, not production, code) when you need determine equality for a type of object differently
 * than its <code>equals</code>
 * method&#8212;either you can't change the <code>equals</code> method, or the <code>equals</code> method is sensible generally, but you're in a special situation where you
 * need something else&#8212;and/or you want to enforce at compile-time that the types of the compared values are in a subtype/supertype relationship.
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
 * This trait is the strictest of the three <em>legacy triple equals</em> traits, enforcing a stronger constraint than
 * both <code>LegacyTripleEquals</code> (the most lenient) and <code>ConversionCheckedLegacyTripleEquals</code> (the middle ground).
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
 * <pre class="stHighlight">
 * scala&gt; import org.scalautils._
 * import org.scalautils._
 * 
 * scala&gt; import TypeCheckedLegacyTripleEquals._
 * import TypeCheckedLegacyTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * &lt;console&gt;:14: error: types Int and Long do not adhere to the equality constraint selected for
 * the === and !== operators; they must either be in a subtype/supertype relationship, or, if
 * ConversionCheckedLegacyTripleEquals is in force, implicitly convertible in one direction or the other;
 * the missing implicit parameter is of type org.scalautils.EqualityConstraint[Int,Long]
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
 * <pre class="stHighlight">
 * scala&gt; import ConversionCheckedLegacyTripleEquals._
 * import ConversionCheckedLegacyTripleEquals._
 * 
 * scala&gt; 1 === 1L
 * res1: Option[String] = None
 * </pre>
 * 
 * <p>
 * The implicit conversion can go in either direction: from the left type to the right type, or vice versa. In the above expression the 
 * implicit conversion goes from left to right (the <code>Int</code> on the left to the <code>Long</code> on the right). It also works
 * the other way:
 * </p>
 * 
 * <pre class="stHighlight">
 * scala&gt; 1L === 1
 * res2: Option[String] = None
 * </pre>
 * 
 * <p>
 * This trait will override or hide implicit methods defined by its sibling traits,
 * <a href="LegacyTripleEquals.html"><code>LegacyTripleEquals</code></a> or <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>,
 * and can therefore be used to temporarily turn on or off conversion checking in a limited scope.
 * Because the methods in <code>ConversionCheckedLegacyTripleEquals</code> (and its siblings)
 * <em>override</em> all the methods defined in supertype <a href="EqualityConstraints.html"><code>EqualityConstraints</code></a>, you can achieve the same
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
 * An alternative way to solve an unwanted compiler error caused by an over-zealous equality type constraint is to convert one side or the other to type
 * <code>Any</code>. Because <code>Any</code> is a supertype of everything, any level of equality type constraint will be satisfied. The <code>AsAny</code>
 * trait offers a convenient syntax, the <code>asAny</code> method, for this purpose:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; import org.scalautils._
 * import org.scalautils._
 *
 * scala&gt; import TypeCheckedLegacyTripleEquals._
 * import TypeCheckedLegacyTripleEquals._
 *
 * scala&gt; import AsAny._
 * import AsAny._
 *
 * scala&gt; 1 === 1L
 * &gt;console&gt;:17: error: types Int and Long do not adhere to the equality constraint selected for
 * the === and !== operators; they must either be in a subtype/supertype relationship, or, if
 * ConversionCheckedLegacyTripleEquals is in force, implicitly convertible in one direction or the other;
 * the missing implicit parameter is of type org.scalautils.EqualityConstraint[Int,Long]
 *               1 === 1L
 *                 ^
 *
 * scala&gt; 1 === 1L.asAny
 * res1: Option[String] = None
 *
 * </pre>
 * 
 * @author Bill Venners
 */
trait TypeCheckedLegacyTripleEquals extends LowPriorityTypeCheckedConstraint {

  // override def convertToAsAnyWrapper(o: Any): AsAnyWrapper = super.convertToAsAnyWrapper(o)

  implicit override def defaultEquality[A]: Equality[A] = new DefaultEquality[A]

  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  override def convertToLegacyEqualizer[T](left: T): LegacyEqualizer[T] = new LegacyEqualizer(left)
  implicit override def convertToLegacyCheckingEqualizer[T](left: T): LegacyCheckingEqualizer[T] = new LegacyCheckingEqualizer(left)

  implicit override def typeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: B <:< A): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)

  override def lowPriorityConversionCheckedEqualityConstraint[A, B](implicit equalityOfB: Equality[B], cnv: A => B): EqualityConstraint[A, B] = new AToBEqualityConstraint[A, B](equalityOfB, cnv)
  override def conversionCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], cnv: B => A): EqualityConstraint[A, B] = new BToAEqualityConstraint[A, B](equalityOfA, cnv)
}

/**
 * Companion object to trait <code>TypeCheckedLegacyTripleEquals</code> that facilitates the importing of <code>TypeCheckedLegacyTripleEquals</code> members as 
 * an alternative to mixing it in. One use case is to import <code>TypeCheckedLegacyTripleEquals</code> members so you can use
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
 * scala&gt; import TypeCheckedLegacyTripleEquals._
 * import TypeCheckedLegacyTripleEquals._
 *
 * scala&gt; 1 + 1 === 2
 * res0: Option[String] = None
 * </pre>
 */
object TypeCheckedLegacyTripleEquals extends TypeCheckedLegacyTripleEquals

