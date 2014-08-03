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
 * Provides an implicit method that loosens the equality constraint defined by <code>TypeCheckedTripleEquals</code> or <code>ConversionCheckedTripleEquals</code>
 * for Scala <code>Map</code>s to one that more closely matches Scala's approach to <code>Map</code> equality.
 *
 * <p>
 * Scala's approach to <code>Map</code> equality is that if both objects being compared are <code>Map</code>s, the elements are compared to determine equality.
 * This means you could compare an immutable <code>TreeMap</code> and a mutable <code>HashMap</code> for equality, for instance, and get true so long as the two maps
 * contained the same key-value mappings. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import scala.collection.immutable.TreeMap
 * import scala.collection.immutable.TreeMap
 *
 * scala&gt; import scala.collection.mutable.HashMap
 * import scala.collection.mutable.HashMap
 *
 * scala&gt; TreeMap("one" -&gt; 1, "two" -&gt; 2) == HashMap("one" -&gt; 1, "two" -&gt; 2)
 * res0: Boolean = true
 * </pre>
 *
 * <p>
 * Such a comparison would not, however, compile if you used <code>===</code> under either <code>TypeCheckedTripleEquals</code> or <code>ConversionCheckedTripleEquals</code>,
 * because <code>TreeMap</code> and <code>HashMap</code> are not in a subtype/supertype relationship, nor does an implicit conversion by default exist between them:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; TreeMap("one" -&gt; 1, "two" -&gt; 2) === HashMap("one" -&gt; 1, "two" -&gt; 2)
 * &lt;console&gt;:16: error: types scala.collection.immutable.TreeMap[String,Int] and
 *   scala.collection.mutable.HashMap[String,Int] do not adhere to the equality constraint selected for
 *   the === and !== operators; the missing implicit parameter is of type
 *   org.scalactic.EqualityConstraint[scala.collection.immutable.TreeMap[String,Int],
 *   scala.collection.mutable.HashMap[String,Int]]
 *               TreeMap("one" -&gt; 1, "two" -&gt; 2) === HashMap("one" -&gt; 1, "two" -&gt; 2)
 *                                               ^
 * </pre>
 *
 * <p>
 * If you mix or import the implicit conversion provided by <code>MapEqualityConstraint</code>, however, the comparison will be allowed:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import MapEqualityConstraints._
 * import MapEqualityConstraints._
 *
 * scala&gt; TreeMap("one" -&gt; 1, "two" -&gt; 2) === HashMap("one" -&gt; 1, "two" -&gt; 2)
 * res2: Boolean = true
 * </pre>
 *
 * <p>
 * The equality constraint provided by this trait requires that both left and right sides are subclasses of <code>scala.collection.GenMap</code> and that
 * an <code>EqualityConstraint</code> can be found for both key types and both value types. In the example above, both the <code>TreeMap</code> and
 * <code>HashMap</code> are subclasses of <code>scala.collection.GenMap</code>, and the regular <code>TypeCheckedTripleEquals</code> provides equality
 * constraints for the key types, both of which are <code>String</code>, and value types, both of which are <code>Int</code>. By contrast, this
 * trait would not allow a <code>TreeMap[String, Int]</code> to be compared against a <code>HashMap[String, java.util.Date]</code>, because no equality constraint
 * will exist between the value types <code>Int</code> and <code>Date</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import java.util.Date
 * import java.util.Date
 *
 * scala&gt; TreeMap("one" -&gt; 1, "two" -&gt; 2) === HashMap("one" -&gt; new Date, "two" -&gt; new Date)
 * &lt;console&gt;:20: error: types scala.collection.immutable.TreeMap[String,Int] and
 *   scala.collection.mutable.HashMap[String,java.util.Date] do not adhere to the equality constraint selected for
 *   the === and !== operators; the missing implicit parameter is of type
 *   org.scalactic.EqualityConstraint[scala.collection.immutable.TreeMap[String,Int],
 *   scala.collection.mutable.HashMap[String,java.util.Date]]
 *               TreeMap("one" -&gt; 1, "two" -&gt; 2) === HashMap("one" -&gt; new Date, "two" -&gt; new Date)
 *                                               ^
 * </pre>
 * 
 * @author Bill Venners
 */
trait MapEqualityConstraints {

  import scala.language.higherKinds

  /**
   * Provides an equality constraint that allows two subtypes of <code>scala.collection.GenMap</code>s to be compared for equality with <code>===</code> so long
   * as an <code>EqualityConstraint</code> is available for both key types and both value types.
   */
  implicit def mapEqualityConstraint[KA, VA, CA[ka, kb] <: collection.GenMap[ka, kb], KB, VB, CB[kb, vb] <: collection.GenMap[kb, vb]](implicit equalityOfA: Equality[CA[KA, VA]], evKey: Constraint[KA, KB], evValue: Constraint[VA, VB]): Constraint[CA[KA, VA], CB[KB, VB]] = new BasicConstraint[CA[KA, VA], CB[KB, VB]](equalityOfA)
}

/**
 * Companion object that facilitates the importing of <code>MapEqualityConstraints</code> members as 
 * an alternative to mixing it in. One use case is to import <code>MapEqualityConstraints</code> members so you can use
 * them in the Scala interpreter.
 */
object MapEqualityConstraints extends MapEqualityConstraints
