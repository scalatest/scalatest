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
package org

import scala.util.control.NonFatal

package object scalactic {

  /**
   * Type alias for <code>String</code>.
   */
  type ErrorMessage = String

  /**
   * Returns the result of evaluating the given block <code>f</code>, wrapped in a <code>Good</code>, or
   * if an exception is thrown, the <code>Throwable</code>, wrapped in a <code>Bad</code>.
   *
   * <p>
   * Here are some examples:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalactic._
   * import org.scalactic._
   *
   * scala&gt; attempt { 2 / 1 }
   * res0: org.scalactic.Or[Int,Throwable] = Good(2)
   *
   * scala&gt; attempt { 2 / 0 }
   * res1: org.scalactic.Or[Int,Throwable] = Bad(java.lang.ArithmeticException: / by zero)
   * </pre>
   *
   * @param f the block to attempt to evaluate
   * @return the result of evaluating the block, wrapped in a <code>Good</code>, or the
   *     thrown exception, wrapped in a <code>Bad</code>
   */
  def attempt[R](f: => R): R Or Throwable =
    try Good(f)
    catch {
      case e: Throwable if NonFatal(e) => Bad(e)
    }

  /**
   * The version number of Scalactic.
   *
   * @return the Scalactic version number.
   */
  val ScalacticVersion: String = ScalacticVersions.ScalacticVersion

  /**
   * <strong>The name <code>org.scalactic.Chain</code> has been deprecated and will be removed in a future version of Scalactic. Please use
   * its new name, <code>org.scalatest.anyvals.NonEmptyList</code>, instead.</strong>
   *
   * <p>This type has been renamed for consistency with other '<code>NonEmpty</code>' anyvals.</p>
   */
  @deprecated("Chain has been deprecated and will be removed in a future version of Scalactic. Please use org.scalactic.anyvals.NonEmptyList instead.", "3.1.0")
  type Chain[+T] = org.scalactic.anyvals.NonEmptyList[T]

  /**
   * <strong>The name <code>org.scalactic.Chain</code> has been deprecated and will be removed in a future version of Scalactic. Please use
   * its new name, <code>org.scalatest.anyvals.NonEmptyList</code>, instead.</strong>
   *
   * <p>This type has been renamed for consistency with other '<code>NonEmpty</code>' anyvals.</p>
   */
  @deprecated("Chain has been deprecated and will be removed in a future version of Scalactic. Please use org.scalactic.anyvals.NonEmptyList instead.", "3.1.0")
  val Chain = org.scalactic.anyvals.NonEmptyList

  /**
   * <strong>The name <code>org.scalactic.End</code> has been deprecated and will be removed in a future version of Scalactic. Please use
   * its new name, <code>org.scalatest.anyvals.End</code>, instead.</strong>
   *
   * <p>This type has been renamed for consistency with other '<code>NonEmpty</code>' anyvals.</p>
   */
  @deprecated("End has been deprecated and will be removed in a future version of Scalactic. Please use org.scalactic.anyvals.End instead.", "3.1.0")
  val End = org.scalactic.anyvals.End
}
