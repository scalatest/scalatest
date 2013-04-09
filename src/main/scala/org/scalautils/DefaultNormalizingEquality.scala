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
package org.scalautils

/**
 * An <code>Equality</code> implementation that determines the equality of two objects by normalizing 
 * both objects, if possible, and then comparing the results using default equality (as defined by
 * the <code>areEqual</code> method of <a href="DefaultEquality.html"><code>DefaultEquality</code></a>).
 * </p>
 * <pre class="stHighlight">
 * import org.scalautils._
 *
 * class StringEquality extends NormalizedEquality[String] {
 *   def isInstanceOfA(b: Any) = b.isInstanceOf[String]
 *   def normalized(s: String): String = s.trim.toLowerCase
 * }
 * </pre>
 *
 */
trait DefaultNormalizingEquality[A] extends Equality[A] with Normalization[A] {

  private val defaultEquality = new DefaultEquality[A]

  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal by
   * first passing <code>b</code> to the <code>isInstanceOfA</code> method. If <code>isInstanceOfA</code> returns
   * <code>true</code>, this method casts <code>b</code> to type <code>A</code> and passes that to <code>normalized</code>
   * to obtain <code>b</code> in  normalized form. This method then passes <code>a</code> to <code>normalized</code>, to
   * obtain <code>a</code> in normalized form. Finally, this method invokes <code>areEqual</code> on 
   * <code>DefaultEquality</code>, passing the normalized <code>a</code> object, and either the normalized <code>b</code>
   * object, if <code>b</code> was an instance of <code>A</code>, else just the raw, unnormalized <code>b</code>. This
   *  method returns the result of that <code>areEqual</code> invocation.
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality
   * @tparam A the type whose normalized equality is being defined
   */
  final def areEqual(a: A, b: Any): Boolean = {
    val nb = if (isInstanceOfA(b)) normalized(b.asInstanceOf[A]) else b
    defaultEquality.areEqual(normalized(a), nb)
  }

  /**
   * Indicates whether the passed object is an instance of type <code>A</code>.
   *
   * <p>
   * This method is invoked by sibling method <code>areEqual</code> to determine whether or not
   * <code>b</code> can be cast to </code>A</code> so that it can be safely passed to <code>normalized</code>.
   * To implement this method, simply call <code>b.isInstanceOf[A]</code> for the actual <code>A</code> type.
   * For example, if you are defining a <code>NormalizedEquality[String]</code>, your <code>isInstanceOf</code>
   * method should look like:
   * </p>
   *
   * <pre class="stHighlight">
   * def isInstanceOfA(b: Any) = b.isInstanceOf[String]
   * </pre>
   *
   * <p>
   * If you are defining a <code>NormalizedEquality[xml.Node]</code> your <code>isInstanceOf</code> method
   * should look like:
   * </p>
   *
   * <pre class="stHighlight">
   * def isInstanceOfA(b: Any) = b.isInstanceOf[xml.Node]
   * </pre>
   *
   * @param b the object to inspect to determine whether it is an instance of <code>A<code>
   * @return true if the passed object is an instance of <code>A</code>
   */
  def isInstanceOfA(b: Any): Boolean

  /**
   * Normalizes the passed object.
   *
   * @param o the object to normalize
   * @return the normalized form of the passed object
   */
  def normalized(o: A): A
}

