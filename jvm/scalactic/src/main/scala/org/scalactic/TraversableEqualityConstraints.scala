/*
 * Copyright 2001-2025 Artima, Inc.
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

/**
 * Provides three implicit methods that loosen the equality constraint defined by <code>TypeCheckedTripleEquals</code> 
 * for Scala <code>Traversable</code>s to one that more closely matches Scala's approach to <code>Traversable</code> equality.
 *
 * <p>
 * Scala's approach to <code>Traversable</code> equality is that if the objects being compared are ether both <code>Seq</code>s, both <code>Set</code>s,
 * or both <code>Map</code>s, the elements are compared to determine equality.
 * This means you could compare an immutable <code>Vector</code> and a mutable <code>ListBuffer</code> for equality, for instance, and get true so long as the
 * two <code>Seq</code>s contained the same elements in the same order. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import scala.collection.mutable.ListBuffer
 * import scala.collection.mutable.ListBuffer
 *
 * scala&gt; Vector(1, 2) == ListBuffer(1, 2)
 * res0: Boolean = true
 * </pre>
 *
 * <p>
 * Such a comparison would not, however, compile if you used <code>===</code> under <code>TypeCheckedTripleEquals</code>,
 * because <code>Vector</code> and <code>ListBuffer</code> are not in a subtype/supertype relationship:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; Vector(1, 2) === ListBuffer(1, 2)
 * &lt;console&gt;:16: error: types scala.collection.immutable.Vector[Int] and
 *   scala.collection.mutable.ListBuffer[Int] do not adhere to the equality constraint selected for
 *   the === and !== operators; the missing implicit parameter is of type
 *   org.scalactic.CanEqual[scala.collection.immutable.Vector[Int],
 *   scala.collection.mutable.ListBuffer[Int]]
 *               Vector(1, 2) === ListBuffer(1, 2)
 *                            ^
 * </pre>
 *
 * <p>
 * If you mix or import the implicit conversion provided by <code>TraversableEqualityConstraint</code>, however, the comparison will be allowed:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import TraversableEqualityConstraints._
 * import TraversableEqualityConstraints._
 *
 * scala&gt; Vector(1, 2) === ListBuffer(1, 2)
 * res2: Boolean = true
 * </pre>
 *
 * <p>
 * The equality constraints provided by this trait require that left and right sides are both subclasses of either <code>scala.collection.GenSeq</code>,
 * <code>scala.collection.GenSet</code>, or <code>scala.collection.GenMap</code>, and that
 * an <code>CanEqual</code> can be found for the element types for <code>Seq</code> and <code>Set</code>, or the key and value types for <code>Map</code>s. In
 * the example above, both the <code>Vector</code> and
 * <code>ListBuffer</code> are subclasses of <code>scala.collection.GenSeq</code>, and the regular <code>TypeCheckedTripleEquals</code> provides equality
 * constraints for the element types, both of which are <code>Int</code>. By contrast, this
 * trait would not allow a <code>Vector[Int]</code> to be compared against a <code>ListBuffer[java.util.Date]</code>, because no equality constraint
 * will exist between the element types <code>Int</code> and <code>Date</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import java.util.Date
 * import java.util.Date
 *
 * scala&gt; Vector(1, 2) === ListBuffer(new Date, new Date)
 * &lt;console&gt;:20: error: types scala.collection.immutable.Vector[Int] and
 *   scala.collection.mutable.ListBuffer[java.util.Date] do not adhere to the equality constraint selected for
 *   the === and !== operators; the missing implicit parameter is of type
 *   org.scalactic.CanEqual[scala.collection.immutable.Vector[Int],
 *   scala.collection.mutable.ListBuffer[java.util.Date]]
 *               Vector(1, 2) === ListBuffer(new Date, new Date)
 *                            ^
 * </pre>
 *
 * <p>
 * This trait simply mixes together <a href="SeqEqualityConstraints.html"><code>SeqEqualityConstraints</code></a>,
 * <a href="SetEqualityConstraints.html"><code>SetEqualityConstraints</code></a>,
 * and <a href="MapEqualityConstraints.html"><code>MapEqualityConstraints</code></a>.
 * </p>
 * 
 * @author Bill Venners
 */
trait TraversableEqualityConstraints extends SeqEqualityConstraints with SetEqualityConstraints with MapEqualityConstraints

/**
 * Companion object that facilitates the importing of <code>TraversableEqualityConstraints</code> members as 
 * an alternative to mixing it in. One use case is to import <code>TraversableEqualityConstraints</code> members so you can use
 * them in the Scala interpreter.
 */
object TraversableEqualityConstraints extends TraversableEqualityConstraints
