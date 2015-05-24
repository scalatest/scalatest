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
 * An <code>Equality[T]</code> that offers a <code>hashCodeFor</code> method that
 * can provide an <code>Int</code> hash code for a given <code>T</code> instance, whose
 * contract is constrainted by that of <code>areEqual</code>.
 *
 * <p>
 * The general contract of <code>hashCodeFor</code> is:
 * </p>
 *
 * <ul>
 * <li>
 * Whenever <code>hashCodeFor</code> is passed on the same object more than once during an execution an application,
 * <code>hashCodeFor</code> must consistently return the same integer, provided no information used in <code>areEqual</code>
 * comparisons on the object is modified. This integer need not remain consistent from one execution of an application
 * to another execution of the same application.
 * </li>
 * <li>
 * If two objects are equal according to the <code>areEqual</code> method, then passing either of those objects to
 * the <code>hashCodeFor</code> method must produce the same integer result.
 * </li>
 * <li>
 * It is not required that if two objects are unequal according to the <code>areEqual</code> method, then calling the
 * <code>hashCodeFor</code> method on each of the two objects must produce distinct integer results. However, you should
 * be aware that producing distinct integer results for unequal objects may improve the performance of hashtables. 
 * </p>
 * 
 * <p>
 * Trait <code>HashingEquality</code> is used by instances of <a href="EquaPath$EquaSet.html"><code>EquaPath#EquaSet</code></a> to implement hash sets based
 * on custom equalities.
 * </p>
 */
trait HashingEquality[T] extends Equality[T] {

  /**
   * Returns a hash code for the specified object that is consistent with <code>areEqual</code>.
   *
   * <p>
   * See the main documentation of this trait for more detail on the contract of <code>hashCodeFor</code>.
   * </p>
   */
  def hashCodeFor(a: T): Int
} 

/**
 * Companion object for trait <code>HashingEquality</code> that provides a factory method for producing <code>HashingEquality</code>
 * instances.
 */ 
object HashingEquality {

  /**
   * Provides default <code>HashingEquality</code> implementations for the specified type whose
   * <code>areEqual</code> method first calls <code>.deep</code> on any <code>Array</code> (on either the left or right side),
   * then compares the resulting objects with <code>==</code>, and whose <code>hashCodeFor</code> method first calls
   * .deep if the passed object is an <code>array</code>, then calls <code>##</code>.
   *
   * @return a default <code>HashingEquality[A]</code>
   */
  implicit def default[A]: HashingEquality[A] = new DefaultHashingEquality[A]
}
