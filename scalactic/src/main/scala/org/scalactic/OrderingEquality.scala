/*
 * Copyright 2001-2014 Artima, Inc.
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
 * A <code>HashingEquality[T]</code> that offers a <code>compare</code> method that indicates 
 * whether two objects of type <code>T</code> are greater than, less than, or equal to each other.
 *
 * <p>
 * Instances of this trait must define a <em>total ordering</em> on objects of type <code>T</code> that 
 * is consistent with <code>areEqual</code> according to the following:
 * </p>
 *
 * <ul>
 * <li><em>consistency</em>: for any non-<code>null</code> values <code>x</code> and <code>y</code>, <code>(compare(a, b) == 0) == areEqual(x, y)</code> must return <code>true</code>.</li>
 * <li><em>right-null</em>: For any non-<code>null</code> value <code>x</code>, <code>compare(x, null)</code> must throw <code>NullPointerException</code>.</li>
 * <li><em>left-null</em>: For any non-<code>null</code> value <code>x</code>, <code>compare(null, x)</code> must throw <code>NullPointerException</code>.</li>
 * <li><em>both-null</em>: <code>compare(null, null)</code> must throw <code>NullPointerException</code>.</li>
 * </ul>
 */
trait OrderingEquality[A] extends HashingEquality[A] {

  /**
   * Returns an integer whose sign indicates how x compares to y.
   *
   * <p>
   * The result sign has the following meaning:
   * </p>
   *
   * <ul>
   * <li>negative if x < y</li>
   * <li>positive if x > y</li>
   * <li>zero otherwise (if <code>areEqual(x, y)</code>)</li>
   * </ul>
   *
   * <p>
   * For more detail on the contract of this method, see the main documentation for this trait.
   * </p>
   */
  def compare(a: A, b: A): Int

// Actually the only way to do this is to make them implement areEqual in
// each case and ask them to do it right. Give them the template, but problem
// is that even optionOfA: Option[A] method is problematic. This means they
// need to implement compare and areEqual, and that's a pain. But otherwise
// I can't give them an OrdBox. Well I could make them write compareNotEqual.
// So compare always calls areEqual, which it can do, or areEquivalent, and if
// that's not the case, then it calls compareNotEqual, which actually does
// the deed. This way compare and areEqual will always be in sync. Or weirder still
// I could have one method that both areEqual and compare call? And you implement that?
// Something that takes an A and an Any, then pattern matches, and if it is ... but
// that was compare(a: A, b: Any), which caused me to run into trouble. Oh, yes, because
// if it is not an A, I can't give a value for comparing.

  // Yes, need both areEqual and compare. Can't implement one in terms of
  // the other because of the Any in areEqual, but you can't do a compare
  // with an Any even if you wanted to, which you wouldn't. Could do something
  // funky to try and reduce errors, like a optionOfA method that you implement
  // areEqual in terms of, but that'll just slow down areEqual. I think it is better
  // to just require users to implement both, and say they need to be consistent with
  // each other, like equals and hashCode, or like Ordering's equiv and ==. That way
  // people can implement each in the most efficient manner.
  // Oh yes, the reason we need the Any is so we can provide an OrdBox that doesn't
  // cast and hope what comes back is a ClassCastException.
  // override def areEqual(a: A, b: Any): Boolean

  /**
   * Returns true if `a` <= `b` in the total order defined by this <code>OrderingEquality</code>.
  */
  final def lteq(a: A, b: A): Boolean = compare(a, b) <= 0

  /**
   * Returns true if `a` >= `b` in the total order defined by this <code>OrderingEquality</code>.
  */
  final def gteq(a: A, b: A): Boolean = compare(a, b) >= 0

  /**
   * Returns true if `a` < `b` in the total order defined by this <code>OrderingEquality</code>.
  */
  final def lt(a: A, b: A): Boolean = compare(a, b) < 0

  /**
   * Returns true if `a` > `b` in the total order defined by this <code>OrderingEquality</code>.
  */
  final def gt(a: A, b: A): Boolean = compare(a, b) > 0

  /**
   * Returns true if `a` == `b` in the total order defined by this <code>OrderingEquality</code>.
  */
  // final def equiv(a: A, b: A): Boolean = compare(a, b) == 0 // Silly. Don't need this as it is redundant with areEquivalent.

  /**
   * Returns `a` if `a` >= `b`, otherwise `b`, according to the total order defined by this <code>OrderingEquality</code>.
  */
  final def max(a: A, b: A): A = if (gteq(a, b)) a else b

  /**
   * Returns `a` if `a` <= `b`, otherwise `b`, according to the total order defined by this <code>OrderingEquality</code>.
  */
  final def min(a: A, b: A): A = if (lteq(a, b)) a else b
}

/**
 * Companion object for trait <code>HashingEquality</code> that provides a factory method for producing <code>HashingEquality</code>
 * instances.
 */ 
object OrderingEquality {

  /**
   * Provides default <code>HashingEquality</code> implementations for the specified type whose
   * <code>areEqual</code> method first calls <code>.deep</code> on any <code>Array</code> (on either the left or right side),
   * then compares the resulting objects with <code>==</code>, and whose <code>hashCodeFor</code> method first calls
   * .deep if the passed object is an <code>array</code>, then calls <code>##</code>. The compare method is defined
   * by the <code>compare</code> method of the implicitly provided <code>Ordering[A]</code>.
   *
   * <p>
   * Note, the implicitly provided <code>Ordering</code> must be consistent with the <code>areEqual</code> and <code>hashCodeFor</code>
   * methods. This will be true by default because the <code>Ordering</code>s provided by Scala are consistent with <code>==</code>
   * and <code>##</code>, and Scala provides no implicit <code>Ordering</code> for arrays.
   * </p>
   *
   * @return a default <code>OrderingEquivalence[A]</code>
   */
  implicit def default[A](implicit ordering: Ordering[A]): OrderingEquality[A] = new DefaultOrderingEquality[A]
}

