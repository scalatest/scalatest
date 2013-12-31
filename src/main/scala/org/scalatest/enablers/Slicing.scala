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
package org.scalatest.enablers

import org.scalautils.{Equality, Every}
import org.scalatest.words.ArrayWrapper
import scala.collection.GenTraversable
import org.scalatest.FailureMessages
import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
 * Typeclass that enables for aggregations certain <code>contain</code> syntax in the ScalaTest matchers DSL.
 *
 * <p>
 * An <code>Aggregating[A]</code> provides access to the "aggregating nature" of type <code>A</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>A</code>. An <code>A</code>
 * can be any type of <em>aggregation</em>&#8212;an object that in some way aggregates or brings together other objects. ScalaTest provides
 * implicit implementations for several types out of the box in the
 * <a href="Aggregating$.html"><code>Aggregating</code> companion object</a>:
 * </p>
 * 
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 * 
 * <p>
 * The <code>contain</code> syntax enabled by this trait is:
 * <p>
 * 
 * <ul>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>atLeastOneOf</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>atMostOneOf</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>only</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>allOf</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>theSameElementsAs</code> <code>(List(1, 2, 3))</code></li>
 * </ul>
 * 
 * <p>
 * You can enable the <code>contain</code> matcher syntax enabled by <code>Aggregating</code> on your own
 * type <code>U</code> by defining an <code>Aggregating[U]</code> for the type and making it available implicitly.
 * </p>
 *
 * <p>
 * Note, for an explanation of the difference between <code>Containing</code> and <code>Aggregating</code>, both of which
 * enable <code>contain</code> matcher syntax, see the <a href="Containing.html#containingVersusAggregating">Containing
 * versus Aggregating</a> section of the main documentation for trait <code>Containing</code>.
 * </p>
 */
private[scalatest] trait Slicing[-A] {

// TODO: Write tests that a NotAllowedException is thrown when no elements are passed, maybe if only one element is passed, and 
// likely if an object is repeated in the list.
  /**
   * Implements <code>contain</code> <code>atLeastOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at least one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at least one of the passed elements
   */
  def includes(sequence: A, subSequence: A): Boolean

  /**
   * Implements <code>contain</code> <code>theSameElementsAs</code> syntax for aggregations of type <code>A</code>.
   *
   * @param leftAggregation an aggregation about which an assertion is being made
   * @param rightAggregation an aggregation that should contain the same elements as the passed <code>leftAggregation</code>
   * @return true if the passed <code>leftAggregation</code> contains the same elements as the passed <code>rightAggregation</code>
   */
  def startsWith(sequence: A, prefix: A): Boolean

  /**
   * Implements <code>contain</code> <code>only</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles the only elements that should be contained in the passed aggregation
   * @return true if the passed aggregation contains only the passed elements
   */
  def endsWith(sequence: A, suffix: A): Boolean
}

/**
 * Companion object for <code>Aggregating</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 */
private[scalatest] object Slicing {

  /**
   * Implicit to support <code>Aggregating</code> nature of <code>String</code>.
   *
   * @param equality <a href="../../scalautils/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
   * @return <code>Aggregating[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
   */
  implicit def slicingNatureOfString: Slicing[String] = 
    new Slicing[String] {
      def includes(string: String, subString: String): Boolean = string.indexOf(subString) >= 0
      def startsWith(string: String, prefix: String): Boolean = string.startsWith(prefix)
      def endsWith(string: String, suffix: String): Boolean = string.endsWith(suffix)
    }
}

