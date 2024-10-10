/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.prop

/**
  * A nullary (zero-parameter) "function" that is a bit friendlier to testing.
  *
  * This is a variant of `() => A` -- that is, a function that takes no parameters
  * and returns an [[A]]. In practice, that isn't quite true: where `() => A` is
  * lazy (it is not evaluated until you call it), this one is strict (you pass the
  * result in as a parameter).
  *
  * In exchange, this is more usable, and reproducible for test environments. Its
  * `hashCode` and `equals` are based on those of the passed-in value (so they are
  * consistent and reproducible), and its `toString` nicely displays the result
  * that will always be returned.
  *
  * @param result the value that will be returned by this function
  * @tparam A the type that is returned by this function
  */
class PrettyFunction0[A](private val result: A) extends (() => A) {
  /**
  * Applies this function and returns the stored result.
  *
  * @return the result that was stored in this function
  */
  def apply(): A = result
  /**
  * Returns a string representation of this function.
  *
  * @return a string that represents this function in the form "() => result"
  */
  override def toString = s"() => $result"
  /**
  * Computes the hash code based on the stored result.
  *
  * @return the hash code of the stored result
  */
  override def hashCode: Int = result.hashCode
  /**
  * Checks if this function is equal to another object.
  *
  * @param o the object to compare against
  * @return true if the given object is also a `PrettyFunction0`
  *         with an equivalent stored result, otherwise false
  */
  override def equals(o: Any): Boolean = {
    o match {
      case that: PrettyFunction0[_] => that.result == this.result
      case _ => false
    }
  }
}

object PrettyFunction0 {
  /**
    * Create a [[PrettyFunction0]].
    *
    * See [[PrettyFunction0]] for details.
    *
    * @param a the value that will be returned when you evaluate the resulting function
    * @tparam A the type returned by the resulting function
    * @return a function that always returns the passed-in value
    */
  def apply[A](a: A): PrettyFunction0[A] = new PrettyFunction0[A](a)
}
