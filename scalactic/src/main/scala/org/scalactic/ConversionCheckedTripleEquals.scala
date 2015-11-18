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
 * <strong>Trait <code>ConversionCheckedTripleEquals</code> has been deprecated and will be removed in a future version of Scalactic. Please use <code>TypeCheckedTripleEquals</code> with a type annotation instead.</strong>
 *
 * <p>
 * Trait <code>ConversionCheckedTripleEquals</code> has been deprecated because code that uses it can break if you 
 * change the equality policy to <code>TripleEquals</code>. For example, because <code>JavaConversions</code> provides
 * an implicit conversion between <code>java.util.Set</code> and <code>scala.collection.mutable.Set</code>,
 * an equality comparison under <code>ConversionCheckedTripleEquals</code> can yield <code>true</code>:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import collection.JavaConversions._
 * import collection.JavaConversions._
 *
 * scala&gt; import collection.mutable
 * import collection.mutable
 *
 * scala&gt; import ConversionCheckedTripleEquals._
 * import ConversionCheckedTripleEquals._
 *
 * scala&gt; mutable.Set.empty[String] === new java.util.HashSet[String]
 * res0: Boolean = true
 * </pre>
 *
 * <p>
 * If code written under <code>ConversionCheckedTripleEquals</code> is left unchanged, but the policy
 * is changed to <code>TripleEquals</code>, the equality comparison will now yield <code>false</code>:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt;  import TripleEquals._
 * import TripleEquals._
 *
 * scala&gt; mutable.Set.empty[String] === (new java.util.HashSet[String])
 * res1: Boolean = false
 * </pre>
 *
 * <p>
 * The above change from <code>true</code> to <code>false</code> happens without any warning
 * or complaint from the compiler. Thus it is quite error prone. A better way to achieve equality
 * comparisons after an implicit conversion is to do so <code>explicitly</code>, by forcing
 * the implicit conversion via a type annotation (following an expression with a colon and
 * the desired type). Here's an example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; mutable.Set.empty[String] === (new java.util.HashSet[String]: mutable.Set[String])
 * res3: Boolean = true
 * </pre>
 *
 * <p>
 * To get rid of the deprecation warning, you can use <code>TypeCheckedTripleEquals</code> instead of
 * <code>ConversionCheckedTripleEquals</code>, and add explicit type annotations where needed:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt;  import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; mutable.Set.empty[String] === new java.util.HashSet[String]
 * &lt;console&gt;:27: error: types scala.collection.mutable.Set[String] and java.util.HashSet[String] do not adhere to the type constraint selected for the === and !== operators; the missing implicit parameter is of type org.scalactic.CanEqual[scala.collection.mutable.Set[String],java.util.HashSet[String]]
 *        mutable.Set.empty[String] === (new java.util.HashSet[String])
 *                                  ^
 *
 * scala&gt; mutable.Set.empty[String] === (new java.util.HashSet[String]: mutable.Set[String])
 * res4: Boolean = true
 * </pre>
 * 
 * @author Bill Venners
 */
@deprecated("ConversionCheckedTripleEquals has been deprecated and will be removed in a future version of Scalactic. Please use TypeCheckedTripleEquals with a type annotation instead")
trait ConversionCheckedTripleEquals extends LowPriorityConversionCheckedConstraint {

  import scala.language.implicitConversions

  // Inherit the Scaladoc for these methods

  override def convertToEqualizer[T](left: T): Equalizer[T] = new Equalizer(left)
  implicit override def convertToCheckingEqualizer[T](left: T): CheckingEqualizer[T] = new CheckingEqualizer(left)

  override def unconstrainedEquality[A, B](implicit equalityOfA: Equality[A]): A CanEqual B = new EqualityConstraint[A, B](equalityOfA)

  override def lowPriorityTypeCheckedConstraint[A, B](implicit equivalenceOfB: Equivalence[B], ev: A <:< B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def convertEquivalenceToAToBConstraint[A, B](equivalenceOfB: Equivalence[B])(implicit ev: A <:< B): A CanEqual B = new AToBEquivalenceConstraint[A, B](equivalenceOfB, ev)
  override def typeCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], ev: B <:< A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
  override def convertEquivalenceToBToAConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B <:< A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)

  implicit override def conversionCheckedConstraint[A, B](implicit equivalenceOfA: Equivalence[A], cnv: B => A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, cnv)
  implicit override def convertEquivalenceToBToAConversionConstraint[A, B](equivalenceOfA: Equivalence[A])(implicit ev: B => A): A CanEqual B = new BToAEquivalenceConstraint[A, B](equivalenceOfA, ev)
}

/**
 * <strong>Object <code>ConversionCheckedTripleEquals</code> has been deprecated and will be removed in a future version of Scalactic. Please use <code>TypeCheckedTripleEquals</code> with a type annotation instead.</strong>
 *
 * <p>
 * For more information and examples, please see the documentation for the <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEqals</code></a> companion trait.
 * </p>
 */
@deprecated("ConversionCheckedTripleEquals has been deprecated and will be removed in a future version of Scalactic. Please use TypeCheckedTripleEquals with a type annotation instead")
object ConversionCheckedTripleEquals extends ConversionCheckedTripleEquals

