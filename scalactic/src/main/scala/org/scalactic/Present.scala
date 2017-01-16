/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic

/**
  * A wrapper class to allow any value to be used with <code>for</code>
  *
  * @param value the underlying value
  * @tparam A the type of the underlying value
  */
final class Present[A] private (val value: A) extends AnyVal {

  /**
    * the underlying value
    *
    * @return the underlying value
    */
  def get: A = value

  /**
    * Builds a new <code>Present</code> by applying a function to the underlying value of this <code>Present</code> that returns type <code>B</code>.
    *
    * @param f the function to transfrom type <code>A</code> to type <code>B</code>
    * @return a <code>Present</code> of <code>B</code>
    */
  def map[B](f: A => B): Present[B] = new Present(f(value))

  /**
    * Builds a new <code>Present</code> by applying a function to underlying value of this <code>Present</code> that returns type <code>Present[B]</code>.
    *
    * @param f the function that takes type <code>A</code> and returns a type <code>Present[B]</code>
    * @return a <code>Present</code> of <code>B</code>
    */
  def flatMap[B](f: A => Present[B]): Present[B] = f(value)

  /**
    * Applies a function f to the underlying value of this <code>Present</code>.
    *
    * @param f the function that takes <code>A</code>
    */
  def foreach(f: A => Unit): Unit = f(value)

  /**
   * A string that includes the underlying value of this <code>Present</code>.
   *
   * @return a string representation of this object
   */
  override def toString: String = s"Present($value)"
}

object Present {

  /**
    * Factory method to create a new instance of <code>Present</code> using the passed in value as underlying value.
    *
    * @param value the underlying value for the <code>Present</code>
    * @return an instance of <code>Present</code> wrapping the passed in <code>value</code>
    */
  def apply[A](value: A) = new Present(value)

}
