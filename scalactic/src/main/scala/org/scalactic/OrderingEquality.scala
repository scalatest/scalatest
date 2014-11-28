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

trait OrderingEquality[A] extends HashingEquality[A] {

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
  override def areEqual(a: A, b: Any): Boolean

  /**
   * Return true if `a` <= `b` in the ordering.
  */
  def lteq(a: A, b: A): Boolean = compare(a, b) <= 0

  /**
   * Return true if `a` >= `b` in the ordering.
  */
  def gteq(a: A, b: A): Boolean = compare(a, b) >= 0

  /**
   * Return true if `a` < `b` in the ordering.
  */
  def lt(a: A, b: A): Boolean = compare(a, b) < 0

  /**
   * Return true if `a` > `b` in the ordering.
  */
  def gt(a: A, b: A): Boolean = compare(a, b) > 0

  /**
   * Return true if `a` == `b` in the ordering.
  */
  def equiv(a: A, b: A): Boolean = compare(a, b) == 0

  /**
   * Return `a` if `a` >= `b`, otherwise `b`.
  */
  def max(a: A, b: A): A = if (gteq(a, b)) a else b

  /**
   * Return `a` if `a` <= `b`, otherwise `b`.
  */
  def min(a: A, b: A): A = if (lteq(a, b)) a else b
}

object OrderingEquality {
  implicit def defaultOrderingEqualityForString: OrderingEquality[String] =
    new OrderingEquality[String] {
      def compare(a: String, b: String): Int = a.compareTo(b)
      def hashCodeFor(a: String): Int = a.hashCode
      def areEqual(a: String, b: Any): Boolean =
        b match {
          case s: String => a == s
          case _ => false
        }
    }
}

