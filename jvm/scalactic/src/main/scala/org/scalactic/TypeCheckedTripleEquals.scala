/*
 * Copyright 2001-2024 Artima, Inc.
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
 * to an <code>Equality</code> type class, and require the types of the two values compared to be in a subtype/supertype
 * relationship.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Trait <code>TypeCheckedTripleEquals</code> is useful (in both production and test code) when you need a stricter type check
 * than is provided by the <a href="http://www.scalactic.org/supersafe">SuperSafe Community Edition</a> compiler plugin for
 * <a href="TripleEquals.html"><code>TripleEquals</code></a>. For example, if you are developing a library that uses advanced
 * features of Scala's type system, you may want to enforce in your tests that the types appearing
 * in equality comparisons match exactly.
 * </td></tr></table>
 *
 * <p>
 * By default under <code>TripleEquals</code>, any use of <code>===</code> will compile, just like the <code>==</code> operator:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 * 
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 * 
 * scala&gt; 1L === 1 // A Long can equal an Int
 * res0: Boolean = true
 *
 * scala&gt; List(1, 2, 3) === Vector(1, 2, 3) // A List can equal a Vector
 * res1: Boolean = true
 *
 * scala&gt; "hi" === 1 // Likely a bug, because a String can never equal an Int
 * res2: Boolean = false
 * </pre>
 *
 * <p>
 * With <a href="http://www.scalactic.org/supersafe">SuperSafe Community Edition</a> installed, the first two expressions
 * above will be allowed to compile, but the third (which represents a likely bug) will not: 
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 * 
 * scala&gt; import TripleEquals._
 * import TripleEquals._
 *
 * scala&gt; 1L === 1
 * res0: Boolean = true
 *
 * scala&gt; List(1, 2, 3) === Vector(1, 2, 3)
 * res1: Boolean = true
 *
 * scala&gt; "hi" === 1 // SuperSafe catches the bug at compile time
 * &lt;console&gt;:17: error: [Artima SuperSafe] Values of type String and Int may not be compared with
 * the === operator. If you really want to compare them for equality, configure Artima SuperSafe to allow
 * those types to be compared for equality.  For more information on this kind of error, see:
 * http://www.artima.com/supersafe_user_guide.html#safer-equality
 *        "hi" === 1
 *             ^
 * </pre>
 *
 * <p>
 * By contrast, <code>TypeCheckedTripleEquals</code> will prevent any of the above three expressions from compiling:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; 1L === 1
 * &lt;console&gt;:17: error: types Long and Int do not adhere to the type constraint selected for
 *     the === and !== operators; the missing implicit parameter is of type org.scalactic.CanEqual[Long,Int]
 *        1L === 1
 *           ^
 *
 * scala&gt; List(1, 2, 3) === Vector(1, 2, 3)
 * &lt;console&gt;:17: error: types List[Int] and scala.collection.immutable.Vector[Int] do not adhere
 *     to the type constraint selected for the === and !== operators; the missing implicit parameter is
 *     of type org.scalactic.CanEqual[List[Int],scala.collection.immutable.Vector[Int]]
 *        List(1, 2, 3) === Vector(1, 2, 3)
 *                      ^
 *
 * scala&gt; "hi" === 1
 * &lt;console&gt;:17: error: types String and Int do not adhere to the type constraint selected for
 *     the === and !== operators; the missing implicit parameter is of type org.scalactic.CanEqual[String,Int]
 *        "hi" === 1
 *             ^
 * </pre>
 *
 * <p>
 * Trait <code>TypeCheckedTripleEquals</code> rejects comparisons of types <code>Int</code> and <code>Long</code>, <code>List[Int]</code>
 * and <code>Vector[Int]</code>, and <code>String</code> and <code>Int</code>, because none have a direct subtype/supertype relationship.
 * To compare two types that are unrelated by inheritance under <code>TypeCheckedTripleEquals</code>, you could
 * convert one of them to the other type (because a type is both a subtype and supertype of itself). Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; 1L === 1.toLong // Now both sides are Long
 * res0: Boolean = true
 *
 * scala&gt; List(1, 2, 3) === Vector(1, 2, 3).toList // Now both sides are List[Int]
 * res1: Boolean = true
 * </pre>
 *
 * <p>
 * Nevertheless, a better (and the recommended) way to deal with this situation is to use a <em>widening type ascription</em>.
 * A type ascription is simply a colon and a type placed next to a variable, usually surrounded by parentheses.
 * For example, because <code>AnyVal</code> is a common supertype of <code>Int</code> and <code>Long</code>, 
 * you could solve the type error by widening the type of one side or the other to <code>AnyVal</code>.
 * Because <code>AnyVal</code> is a supertype of both <code>Int</code> and <code>Long</code>, the
 * type constraint will be satisfied:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; 1 === (1L: AnyVal)
 * res2: Boolean = true
 * 
 * scala&gt; (1: AnyVal) === 1L
 * res3: Boolean = true
 * </pre>
 * 
 * <p>
 * Similarly, since <code>Seq[Int]</code> is a common supertype of both
 * <code>Vector[Int]</code> and <code>List[Int]</code>, the type constraint can be
 * satisfied by widening either to their common supertype, <code>Seq[Int]</code>:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; List(1, 2, 3) === (Vector(1, 2, 3): Seq[Int])
 * res4: Boolean = true
 *
 * scala&gt; (List(1, 2, 3): Seq[Int]) === Vector(1, 2, 3)
 * res5: Boolean = true
 * </pre>
 *
 * <p>
 * The primary intended use case for <code>TypeCheckedTripleEquals</code> is to enable tests to be very strict
 * about which types can compared for equality, but it can also be used with production code where this level of
 * strictness is desired. 
 * </p>
 *
 * <h2>Forcing implicit conversions before equality checks</h2>
 *
 * <p>
 * You can also use a type ascription to force an implicit conversion before a value participates
 * in an equality comparison. For example, although <code>JavaConversions</code> provides
 * an implicit conversion between <code>java.util.Set</code> and <code>scala.collection.mutable.Set</code>,
 * under <code>TypeCheckedTripleEquals</code> an equality comparison between those two types
 * will not be allowed:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import collection.JavaConversions._
 * import collection.JavaConversions._
 *
 * scala&gt; import collection.mutable
 * import collection.mutable
 *
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; mutable.Set.empty[String] === new java.util.HashSet[String]
 * &lt;console&gt;:36: error: types scala.collection.mutable.Set[String] and java.util.HashSet[String] do not
 *     adhere to the type constraint selected for the === and !== operators; the missing implicit parameter
 *     is of type org.scalactic.CanEqual[scala.collection.mutable.Set[String],java.util.HashSet[String]]
 *        mutable.Set.empty[String] === new java.util.HashSet[String]
 *                                  ^
 * </pre>
 *
 * <p>
 * To force an implicit conversion of the Java <code>HashSet</code> to a Scala <code>mutable.Set</code>, after which the
 * type constraint will be satisfied, you can use a type ascription:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; mutable.Set.empty[String] === (new java.util.HashSet[String]: mutable.Set[String])
 * res0: Boolean = true
 * </pre>
 * 
 * <h2>Scoping equality policies</h2>
 *
 * <p>
 * This trait will override or hide implicit methods defined by 
 * <a href="TripleEquals.html"><code>TripleEquals</code></a> 
 * and can therefore be used to temporarily turn on or off type checking in a limited scope. Here's an example, in which <code>TypeCheckedTripleEquals</code> will
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
 * <p>
 * Because <code>Int</code> and <code>Long</code> are not in a subtype/supertype relationship, comparing <code>1</code> and <code>1L</code> in the context
 * of <code>TypeCheckedTripleEquals</code> will generate a compiler error:
 * </p>
 *
 * <pre>
 * Example.scala:9: error: types Int and Long do not adhere to the type constraint selected
 *   for the === and !== operators; the missing implicit parameter is of
 *   type org.scalactic.CanEqual[Int,Long]
 *         if (a === b) 0       // This line won't compile
 *               ^
 * one error found
 * </pre>
 * 
 * <p>
 * You can &ldquo;relax&rdquo; the type checking locally by importing
 * the members of <code>TripleEquals</code> in a limited scope:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalactic.examples.conversioncheckedtripleequals
 * 
 * import org.scalactic._
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
 * With the above change, the <code>Example.scala</code> file compiles fine. The strict checking is disabled only inside the first <code>cmp</code> method that
 * takes an <code>Int</code> and a <code>Long</code>. <code>TypeCheckedTripleEquals</code> is still enforcing its type constraint, for example, for the <code>s === t</code>
 * expression in the other overloaded <code>cmp</code> method that takes strings.
 * </p>
 * 
 * <p>
 * Because the methods <code>TripleEquals</code> and <code>TypeCheckedTripleEquals</code>
 * <em>override</em> all the methods defined in supertype <a href="TripleEqualsSupport.html"><code>TripleEqualsSupport</code></a>, you can achieve the same
 * kind of nested tuning of equality constraints whether you mix in traits, import from companion objects, or use some combination of both.
 * </p>
 *
 * <p>
 * In short, you should be able to select a primary constraint level via either a mixin or import, then change that in nested scopes
 * however you want, again either through a mixin or import, without getting any implicit conversion ambiguity. The innermost constraint level in scope
 * will always be in force.
 * </p>
 *
 * @author Bill Venners
 */
trait TypeCheckedTripleEquals extends LowPriorityTypeCheckedConstraint {

  import scala.language.implicitConversions

  // Inherit the Scaladoc for these methods

  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  implicit override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): A CanEqual B = new EqualityConstraint[A, B](equalityOfA)

  implicit override def typeCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
  implicit override def convertEquivalenceToBToAConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)

  @deprecated("The lowPriorityConversionCheckedConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def lowPriorityConversionCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], cnv: A => B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, cnv)
  @deprecated("The convertEquivalenceToAToBConversionConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def convertEquivalenceToAToBConversionConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A => B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  @deprecated("The conversionCheckedConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def conversionCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], cnv: B => A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, cnv)
  @deprecated("The convertEquivalenceToBToAConversionConstraint method has been deprecated and will be removed in a future version of ScalaTest. It is no longer needed now that the deprecation period of ConversionCheckedTripleEquals has expired. It will not be replaced.", "3.1.0")
  override def convertEquivalenceToBToAConversionConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B => A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
}

/**
 * Companion object to trait <code>TypeCheckedTripleEquals</code> that facilitates the importing of <code>TypeCheckedTripleEquals</code> members as 
 * an alternative to mixing it in. One use case is to import <code>TypeCheckedTripleEquals</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalatest.jar
 * Welcome to Scala 2.13.6 (OpenJDK 64-Bit Server VM, Java yyy).
 * Type in expressions for evaluation. Or try :help.
 *
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; 1 + 1 === 2
 * res0: Boolean = true
 * </pre>
 */
object TypeCheckedTripleEquals extends TypeCheckedTripleEquals

