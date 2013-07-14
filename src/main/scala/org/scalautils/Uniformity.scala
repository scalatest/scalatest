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
 * Defines a custom way to normalize instances of a type that can also handle normalization of that type when passed as <code>Any</code>.
 *
 * <p>
 * </p>
 * 
 * @tparam A the type whose normalization is being defined
 */
trait Uniformity[A] extends Normalization[A] { thisUniformity =>

  // TODO: Add an example of Array[String] isInstanceOfA here and in NormalizedEquality
  /* TODO: Need to fix this scaladoc!
   * Indicates whether the passed object is an instance of type <code>A</code>.
   *
   * <p>
   * This method is invoked by the <code>areEqual</code> method of subclass <code>NormalizedEquality</code> to determine whether or not
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
  def normalizedOrSame(b: Any): Any

  /**
   * Indicates whether this <code>Uniformity</code>'s <code>normalized</code> method can handle the passed object, if cast to the appropriate type.
   *
   * <p>
   * If this method returns true for a particular passed object, it means that if the object is passed
   * to <code>normalizedOrSame</code>, that method will return the result of passing it to <code>normalized</code>.
   * It does not mean that the object will necessarily be <em>modified</em> when passed to <code>normalizedOrSame</code> or <code>normalized</code>.
   * For example, the <code>lowerCased</code> field of <code>StringNormalizations</code> is a <code>Uniformity[String]</code>
   * that normalizes strings by forcing all characters to lower case:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalautils._
   * import org.scalautils._
   *
   * scala&gt; import StringNormalizations._
   * import StringNormalizations._
   *
   * scala&gt; lowerCased
   * res0: org.scalautils.Uniformity[String] = org.scalautils.StringNormalizations$$anon$1@236db810
   *
   * scala&gt; lowerCased.normalized("HALLOOO!")
   * res1: String = hallooo!
   * </pre>
   *
   * <p>
   * Now consider two strings held from variables of type <code>AnyRef</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; val yell: AnyRef = "HOWDY"
   * yell: AnyRef = HOWDY
   *
   * scala&gt; val whisper: AnyRef = "howdy"
   * whisper: AnyRef = howdy
   * </pre>
   *
   * <p>
   * As you would expect, when <code>yell</code> is passed to <code>normalizedCanHandle</code>, it returns true, and when
   * <code>yell</code> is passed to <code>normalizedOrSame</code>, it returns a lower-cased (normal) form:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; lowerCased.normalizedCanHandle(yell)
   * res2: Boolean = true
   *
   * scala&gt; lowerCased.normalizedOrSame(yell)
   * res3: Any = howdy
   * </pre>
   *
   * <p>
   * A similar thing happens, however, when <code>whisper</code> is passed to <code>normalizedCanHandle</code> and <code>normalizedOrSame</code>,
   * even though in this case the string is already in normal form according to the <code>lowerCased</code> <code>Uniformity</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; lowerCased.normalizedCanHandle(whisper)
   * res4: Boolean = true
   *
   * scala&gt; lowerCased.normalizedOrSame(whisper)
   * res5: Any = howdy
   * </pre>
   *
   * <p>
   * This illustrates that <code>normalizedCanHandle</code> does <em>not</em> indicate that the passed object is not in normalized form already, just that
   * it can be be handled by the <code>normalized</code> method. This further means that the <code>normalized</code> method itself
   * simply ensures that objects are returned in normal form. It need not necessarily change them: if a passed object is already in
   * normal form, <code>normalized</code> can (and usually should) return the exact same object. That is in fact what happened when we normalized
   * <code>whisper</code>. Since <code>whisper</code>'s value of <code>"hello"</code> was already in normal form (all lower-cased), <code>normalized</code> (
   * invoked by the <code>normalizedOrSame</code> method) returned the exact same object passed:
   * </p>
   *
   * <pre>
   * scala&gt; val whisperNormed = res5.asInstanceOf[AnyRef]
   * whisperNormed: AnyRef = howdy
   *
   * scala&gt; whisperNormed eq whisper
   * res8: Boolean = true
   * </pre>
   */
  def normalizedCanHandle(b: Any): Boolean

  /**
   * Returns a new <code>Uniformity</code> that combines this and the passed <code>Uniformity</code>.
   */
  final def and(other: Uniformity[A]): Uniformity[A] =
    new Uniformity[A] {
      // Note in Scaladoc what order, and recommend people don't do side effects anyway.
      // By order, I mean left's normalized gets called first then right's normalized gets called on that result, for "left and right"
      def normalized(a: A): A = other.normalized(thisUniformity.normalized(a))
      def normalizedCanHandle(b: Any): Boolean = other.normalizedCanHandle(b) || thisUniformity.normalizedCanHandle(b)
      def normalizedOrSame(b: Any): Any = other.normalizedOrSame(thisUniformity.normalizedOrSame(b))
    }
}
