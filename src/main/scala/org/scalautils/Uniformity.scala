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
 * For example, to normalize <code>Double</code>s by truncating off any decimal part,
 * you might write:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalautils._
 *
 * val truncated = 
 *   new Uniformity[Double] {
 *    def normalized(d: Double) = d.floor
 *    def normalizedCanHandle(o: Any) = o.isInstanceOf[Double]
 *    def normalizedOrSame(o: Any): Any =
 *      o match {
 *        case d: Double =&gt; normalized(d)
 *        case _ =&gt; o
 *      }
 *  }
 * </pre>
 *
 * <p>
 * Given this definition you could use it with the <a href="Explicitly.html"><code>Explicitly</code></a> DSL like this:
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalatest._
 * import Matchers._
 * 
 * 2.1 should equal (2.0) (after being truncated)
 * </pre>
 *
 * <p>
 * If you make the <code>truncated</code> <code>val</code> implicit and import or mix in the members of <a href="NormMethods.html"><code>NormMethods</code></a>,
 * you can access the behavior by invoking <code>.norm</code> on <code>Double</code>s.
 * </p>
 *
 * <pre class="stHighlight">
 * implicit val doubleUniformity = truncated
 * import NormMethods._
 *
 * val d = 2.1
 * d.norm // returns 2.0
 * </pre>
 * 
 * <p>
 * Note that by creating a <code>Uniformity</code> rather than just an instance of its supertype, <a href="Normalization.html"><code>Normalization</code></a>,
 * it can be used more generally. For example, <code>Uniformity</code>s allow you to the <code>Explicitly</code> DSL with
 * <a href="TripleEquals.html"><code>TripleEquals</code></a>, whereas <code>Normalization</code>s require
 * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> or
 * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a>.
 * <code>Uniformity</code>s also enable you to use the <code>Explicitly</code> DSL with ScalaTest's <code>should</code> <code>===</code>, <code>equal</code>,
 * and <code>contain</code> matcher syntax, whereas a plain <code>Normalization</code> can only be used with <code>should</code> <code>===</code>, and only
 * under either <code>TypeCheckedTripleEquals</code> or <code>ConversionCheckedTripleEquals</code>.
 * </p>
 * 
 * @tparam A the type whose uniformity is being defined
 */
trait Uniformity[A] extends Normalization[A] { thisUniformity =>

  /**
   * Returns either the result of passing this object to <code>normalized</code>, if appropriate, or the same object.
   *
   * <p>
   * Implementations can decide what &ldquo;appropriate&rdquo; means, but the intent is that it will usually mean the
   * value passed is of the type <code>A</code>.  For example, if this is a <code>Uniformity[String]</code>, appropriate means
   * that the value (of type <code>Any</code>) passed is actually an instance of <code>String</code>. Because of erasure,
   * however, a <code>Uniformity[List[String]]</code> will only be able to tell whether a value is a <code>List[_]</code>, 
   * so it might declare any <code>List[_]</code> that contains only <code>String</code>s (determined by invoking
   * <code>isInstanceOf[String]</code> on each element) to be appropriate. This means a <code>Uniformity[List[String]]</code> might normalize
   * a <code>List[AnyRef]</code> that happens to contain only <code>Strings</code>.
   * </p>
   *
   * @param b the object to normalize, if appropriate
   * @return a normalized form of the passed object, if this <code>Uniformity</code> was able to normalize it, else the same object passed
   */
  def normalizedOrSame(b: Any): Any

  /**
   * Indicates whether this <code>Uniformity</code>'s <code>normalized</code> method can &ldquo;handle&rdquo; the passed object, if cast to the
   * appropriate type <code>A</code>.
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
   * res0: org.scalautils.Uniformity[String] = lowerCased
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
   * <pre class="stREPL">
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
   *
   * <p>
   * The <code>normalized</code> and <code>normalizedOrSame</code> methods
   * of the <code>Uniformity</code> returned by this method return a result 
   * obtained by forwarding the passed value first to this <code>Uniformity</code>'s implementation of the method,
   * then passing that result to the other <code>Uniformity</code>'s implementation of the method, respectively.
   * Essentially, the body of the composed <code>normalized</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * uniformityPassedToAnd.normalized(uniformityOnWhichAndWasInvoked.normalized(a))
   * </pre>
   *
   * <p>
   * And the body of the composed <code>normalizedOrSame</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * uniformityPassedToAnd.normalizedOrSame(uniformityOnWhichAndWasInvoked.normalizedOrSame(a))
   * </pre>
   *
   * <p>
   * The <code>normalizeCanHandle</code> method of the <code>Uniformity</code> returned by this method returns a result 
   * obtained by anding the result of forwarding the passed value to this <code>Uniformity</code>'s implementation of the method
   * with the result of forwarding it to the passed <code>Uniformity</code>'s implementation.
   * Essentially, the body of the composed <code>normalizeCanHandle</code> method is:
   * </p>
   *
   * <pre class="stHighlight">
   * uniformityOnWhichAndWasInvoked.normalizeCanHandle(a) &amp;&amp; uniformityPassedToAnd.normalizeCanHandle(a)
   * </pre>
   *
   * @param other a <code>Uniformity</code> to 'and' with this one
   * @return a <code>Uniformity</code> representing the composition of this and the passed <code>Uniformity</code>
   */
  final def and(other: Uniformity[A]): Uniformity[A] =
    new Uniformity[A] {
      // Note in Scaladoc what order, and recommend people don't do side effects anyway.
      // By order, I mean left's normalized gets called first then right's normalized gets called on that result, for "left and right"
      def normalized(a: A): A = other.normalized(thisUniformity.normalized(a))
      def normalizedCanHandle(b: Any): Boolean = other.normalizedCanHandle(b) && thisUniformity.normalizedCanHandle(b)
      def normalizedOrSame(b: Any): Any = other.normalizedOrSame(thisUniformity.normalizedOrSame(b))
    }

  /**
   * Converts this <code>Uniformity</code> to a <code>NormalizingEquality[A]</code> whose <code>normalized</code>,
   * <code>normalizedCanHandle</code>, and <code>normalizedOrSame</code> methods delegate
   * to this <code>Uniformity[A]</code> and whose <code>afterNormalizationEquality</code> field returns the
   * implicitly passed <code>Equality[A]</code>.
   *
   * @param equality the <code>Equality</code> that the returned <code>NormalizingEquality</code>
   *     will delegate to determine equality after normalizing both left and right (if appropriate) sides.
   */
  final def toEquality(implicit equality: Equality[A]): NormalizingEquality[A] =
    new NormalizingEquality[A] {
      override val afterNormalizationEquality = equality
      def normalized(a: A): A = thisUniformity.normalized(a)
      def normalizedCanHandle(b: Any): Boolean = thisUniformity.normalizedCanHandle(b)
      def normalizedOrSame(b: Any): Any = thisUniformity.normalizedOrSame(b)
    }
}

