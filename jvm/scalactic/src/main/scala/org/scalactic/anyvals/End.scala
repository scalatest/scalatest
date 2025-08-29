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
package org.scalactic.anyvals

/**
  * Object that can be used as an endpoint for <code>NonEmptyList</code> construction expressions
  * that use the cons (<code>::</code>) operator.
  *
  * <p>
  * Here's an example:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; 1 :: 2 :: 3 :: End
  * res0: org.scalactic.NonEmptyList[Int] = NonEmptyList(1, 2, 3)
  * </pre>
  *
  * <p>
  * Note that unlike <code>Nil</code>, which is an instance of <code>List[Nothing]</code>,
  * <code>End</code> is not an instance of <code>NonEmptyList[Nothing]</code>, because there is
  * no empty <code>NonEmptyList</code>:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; Nil.isInstanceOf[List[_]]
  * res0: Boolean = true
  *
  * scala&gt; End.isInstanceOf[NonEmptyList[_]]
  * res1: Boolean = false
  * </pre>
  */
object End {
  /**
    * A <code>::</code> operator that serves to start a <code>NonEmptyList</code> construction
    * expression.
    *
    * <p>
    * The result of calling this method will always be a <code>NonEmptyList</code> of length 1.
    * Here's an example:
    * </p>
    *
    * <pre class="stREPL">
    * scala> 1 :: End
    * res0: org.scalactic.NonEmptyList[Int] = NonEmptyList(1)
    * </pre>
    */
  def ::[T](element: T): NonEmptyList[T] = NonEmptyList(element)

  /**
    * Returns <code>"End"</code>.
    */
  override def toString: String = "End"
}