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
 * Provides an implicit method that loosens the equality constraint defined by <code>TypeCheckedTripleEquals</code> or <code>ConversionCheckedTripleEquals</code>
 * for Scala <code>Set</code>s to one that more closely matches Scala's approach to <code>Set</code> equality.
 *
 * <p>
 * Scala's approach to <code>Set</code> equality is that if both objects being compared are <code>Set</code>s, the elements are compared to determine equality.
 * This means you could compare an immutable <code>TreeSet</code> and a mutable <code>HashSet</code> for equality, for instance, and get true so long as the
 * two <code>Set</code>s contained the same elements in the same order. Here's an example:
 * </p>
 *
 * <pre>
 * scala&gt; import scala.collection.immutable.TreeSet
 * import scala.collection.immutable.TreeSet
 *
 * scala&gt; import scala.collection.mutable.HashSet
 * import scala.collection.mutable.HashSet
 *
 * scala&gt; TreeSet(1, 2) == HashSet(1, 2)
 * res0: Boolean = true
 * </pre>
 *
 * <p>
 * Such a comparison would not, however, compile if you used <code>===</code> under either <code>TypeCheckedTripleEquals</code> or <code>ConversionCheckedTripleEquals</code>,
 * because <code>TreeSet</code> and <code>HashSet</code> are not in a subtype/supertype relationship, nor does an implicit conversion by default exist between them:
 * </p>
 *
 * <pre>
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; import TypeCheckedTripleEquals._
 * import TypeCheckedTripleEquals._
 *
 * scala&gt; TreeSet(1, 2) === HashSet(1, 2)
 * &lt;console&gt;:16: error: types scala.collection.immutable.TreeSet[Int] and
 *   scala.collection.mutable.HashSet[Int] do not adhere to the equality constraint selected for
 *   the === and !== operators; the missing implicit parameter is of type
 *   org.scalactic.EqualityConstraint[scala.collection.immutable.TreeSet[Int],
 *   scala.collection.mutable.HashSet[Int]]
 *               TreeSet(1, 2) === HashSet(1, 2)
 *                             ^
 * </pre>
 *
 * <p>
 * If you mix or import the implicit conversion provided by <code>SetEqualityConstraint</code>, however, the comparison will be allowed:
 * </p>
 *
 * <pre>
 * scala&gt; import SetEqualityConstraints._
 * import SetEqualityConstraints._
 *
 * scala&gt; TreeSet(1, 2) === HashSet(1, 2)
 * res2: Boolean = true
 * </pre>
 *
 * <p>
 * The equality constraint provided by this trait requires that both left and right sides are subclasses of <code>scala.collection.GenSet</code> and that
 * an <code>EqualityConstraint</code> can be found for the element types. In the example above, both the <code>TreeSet</code> and
 * <code>HashSet</code> are subclasses of <code>scala.collection.GenSet</code>, and the regular <code>TypeCheckedTripleEquals</code> provides equality
 * constraints for the element types, both of which are <code>Int</code>. By contrast, this
 * trait would not allow a <code>TreeSet[Int]</code> to be compared against a <code>HashSet[java.util.Date]</code>, because no equality constraint
 * will exist between the element types <code>Int</code> and <code>Date</code>:
 * </p>
 *
 * <pre>
 * scala&gt; import java.util.Date
 * import java.util.Date
 *
 * scala&gt; TreeSet(1, 2) === HashSet(new Date, new Date)
 * &lt;console&gt;:20: error: types scala.collection.immutable.TreeSet[Int] and
 *   scala.collection.mutable.HashSet[java.util.Date] do not adhere to the equality constraint selected for
 *   the === and !== operators; the missing implicit parameter is of type
 *   org.scalactic.EqualityConstraint[scala.collection.immutable.TreeSet[Int],
 *   scala.collection.mutable.HashSet[java.util.Date]]
 *               TreeSet(1, 2) === HashSet(new Date, new Date)
 *                             ^
 * </pre>
 * 
 * @author Bill Venners
 */
trait SetEqualityConstraints {

  import scala.language.higherKinds

  /**
   * Provides an equality constraint that allows two subtypes of <code>scala.collection.GenSet</code>s to be compared for equality with <code>===</code> so long
   * as an <code>EqualityConstraint</code> is available for the element types.
   */
  implicit def setEqualityConstraint[EA, CA[ea] <: collection.GenSet[ea], EB, CB[eb] <: collection.GenSet[eb]](implicit equalityOfA: Equality[CA[EA]], ev: Constraint[EA, EB]): Constraint[CA[EA], CB[EB]] = new EqualityConstraint[CA[EA], CB[EB]](equalityOfA)
}

/**
 * Companion object that facilitates the importing of <code>SetEqualityConstraints</code> members as 
 * an alternative to mixing it in. One use case is to import <code>SetEqualityConstraints</code> members so you can use
 * them in the Scala interpreter.
 */
object SetEqualityConstraints extends SetEqualityConstraints
