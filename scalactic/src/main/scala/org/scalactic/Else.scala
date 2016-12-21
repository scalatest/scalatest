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
package org.scalactic

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

/**
 * Represents a value that is one of two possible types, with both types &ldquo;equally acceptable.&rdquo;
 */
sealed abstract class Else[+B,+W] extends Product with Serializable {

  /**
   * Indicates whether this <code>Else</code> is a <code>Port</code>
   *
   * @return true if this <code>Else</code> is a <code>Port</code>, <code>false</code> if it is a <code>Star</code>.
   */
  val isPort: Boolean = false

  /**
   * Indicates whether this <code>Else</code> is a <code>Star</code>
   *
   * @return true if this <code>Else</code> is a <code>Star</code>, <code>false</code> if it is a <code>Port</code>.
   */
  val isStar: Boolean = false

  /**
   * Applies the given function to this <code>Else</code>'s value if it is a <code>Port</code> or returns <code>this</code> if it is a <code>Star</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>Port</code>, the result of applying the given function to the contained value wrapped in a <code>Port</code>,
   *         else this <code>Star</code>
   */
  def portMap[C](f: B => C): C Else W

  /**
   * Applies the given function to this <code>Else</code>'s value if it is a <code>Star</code> or returns <code>this</code> if it is a <code>Port</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>Star</code>, the result of applying the given function to the contained value wrapped in a <code>Star</code>,
   *         else this <code>Port</code>
   */
  def starMap[X](f: W => X): B Else X

  /**
   * Returns an <code>Else</code> with the <code>Port</code> and <code>Star</code> types swapped: <code>Star</code> becomes <code>Port</code> and <code>Port</code>
   * becomes <code>Star</code>.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; val lyrics = Port[Double].elseStar("But we decide which is right. And which is an illusion?")
   * lyrics: org.scalactic.Else[Double,String] =
   *     Star(But we decide which is right. And which is an illusion?)
   *
   * scala&gt; val swapped = lyrics.swap
   * swapped: org.scalactic.Else[String,Double] =
   *     Port(But we decide which is right. And which is an illusion?)
   * </pre>
   *
   * @return if this <code>Else</code> is a <code>Port</code>, its <code>Port</code> value wrapped in a <code>Star</code>; if this <code>Else</code> is
   *     a <code>Star</code>, its <code>Star</code> value wrapped in a <code>Port</code>.
   */
  def swap: W Else B

  /**
   * Transforms this <code>Else</code> by applying the function <code>bf</code> to this <code>Else</code>'s <code>Port</code> value if it
   * is a <code>Port</code>, or by applying <code>wf</code> to this <code>Else</code>'s <code>Star</code> value if it is a <code>Star</code>.
   *
   * @param bf the function to apply to this <code>Else</code>'s <code>Port</code> value, if it is a <code>Port</code>
   * @param wf the function to apply to this <code>Else</code>'s <code>Star</code> value, if it is a <code>Star</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to this <code>Else</code>'s value
   */
  def transform[C, X](bf: B => C Else X, wf: W => C Else X): C Else X

  /**
   * Folds this <code>Else</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>Port</code> else the given <code>wf</code> function if this is a <code>Star</code>.
   *
   * @param bf the function to apply to this <code>Else</code>'s <code>Port</code> value, if it is a <code>Port</code>
   * @param wf the function to apply to this <code>Else</code>'s <code>Star</code> value, if it is a <code>Star</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to this <code>Else</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V

  /**
   * Wraps this <code>Else</code> in an <code>PortMapper</code>, an <code>AnyVal</code> that enables you to transform <code>Port</code> values in a <code>for</code> expression with
   * <code>Star</code> values passing through unchanged.
   */
  def portMapper: PortMapper[B, W] = new PortMapper(this)

  /**
   * Wraps this <code>Else</code> in an <code>StarMapper</code>, an <code>AnyVal</code> that enables you to transform <code>Star</code> values in a <code>for</code> expression with
   * <code>Port</code> values passing through unchanged.
   */
  def starMapper: StarMapper[B, W] = new StarMapper(this)
}

/**
 * The companion object for <code>Else</code> providing factory methods for creating <code>Else</code>s from <code>Either</code>s and <code>Try</code>s.
 */
object Else {

  /**
   * Trait providing a concise <em>type lambda</em> syntax for <code>Else</code> types partially applied on their "bad" type.
   *
   * <p>
   * This trait is used to curry the type parameters of <code>Else</code>, which takes two type parameters,
   * into a type (this trait) which takes one parameter, and another (its type member) which
   * takes the other. For example, type <code>Else[GOOD, BAD]</code> (which can be written in infix form
   * as <code>GOOD Else BAD</code>) can be expressed in curried form as <code>Else.B[BAD]#G[GOOD]</code>.
   * Leaving off the final <code>GOOD</code> type parameter yields a "type lambda," such as <code>Else.B[ErrorMessage]#G</code>.
   * </p>
   *
   * <p>
   * For example, consider this method that takes two type parameters, a <em>type constructor</em> named <code>Context</code> and a 
   * type named <code>A</code>:
   * </p>
   *
   * <pre>
   * scala&gt; def example[Context[_], A](ca: Context[A]) = ca
   * example: [Context[_], A](ca: Context[A])Context[A]
   * </pre>
   *
   * <p>
   * Because <code>List</code> takes a single type parameter, it fits the shape of <code>Context</code>,
   * it can be simply passed to <code>example</code>--<em>i.e.</em>, the compiler will infer <code>Context</code> as <code>List</code>:
   * </p>
   *
   * <pre>
   * scala&gt; example(List(1, 2, 3))
   * res0: List[Int] = List(1, 2, 3)
   * </pre>
   *
   * <p>
   * But because <code>Else</code> takes two type parameters, <code>G</code> for the "good" type and <code>B</code> for the "bad" type, it
   * cannot simply be passed, because the compiler doesn't know which of <code>G</code> or </code>B</code> you'd want to abstract over:
   * </p>
   *
   * <pre>
   * scala&gt; val or: Int Else ErrorMessage = Port(3)
   * or: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Port(3)
   *
   * scala&gt; example(or)
   * &lt;console&gt;:16: error: no type parameters for method example: (ca: Context[A])Context[A] exist so that it can be applied to arguments (org.scalactic.Else[Int,org.scalactic.ErrorMessage])
   *  --- because ---
   * argument expression's type is not compatible with formal parameter type;
   *  found   : org.scalactic.Else[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Else[Int,String]
   *  required: ?Context[?A]
   *        example(or)
   *        ^
   * &lt;console&gt;:16: error: type mismatch;
   *  found   : org.scalactic.Else[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Else[Int,String]
   *  required: Context[A]
   *        example(or)
   *                ^
   * </pre>
   *
   * <p>
   * You must therefore tell the compiler which one you want with a "type lambda." Here's an example:
   * </p>
   *
   * <pre>
   * scala&gt; example[({type L[G] = G Else ErrorMessage})#L, Int](or)
   * res1: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Port(3)
   * </pre>
   *
   * <p>
   * The alternate type lambda syntax provided by this trait is more concise and hopefully easier to remember and read:
   * </p>
   *
   * <pre>
   * scala&gt; example[Else.B[ErrorMessage]#G, Int](or)
   * res2: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Port(3)
   * </pre>
   * 
   * <p>
   * You can read <code>Else.B[ErrorMessage]#G</code> as: an <code>Else</code> with its "bad" type, <code>B</code>,
   * fixed to <code>ErrorMessage</code> and its "good" type, <code>G</code>, left unspecified.
   * </p>
   */
  private[scalactic] trait W[WHITE] {

    /**
     * Type member that provides a curried alias to  <code>G</code> <code>Else</code> <code>B</code>.
     *
     * <p>
     * See the main documentation for trait <code>B</code> for more detail.
     * </p>
     */
    type B[BLACK] = BLACK Else WHITE
  }

  /**
   * Trait providing a concise <em>type lambda</em> syntax for <code>Else</code> types partially applied on their "good" type.
   *
   * <p>
   * This trait is used to curry the type parameters of <code>Else</code>, which takes two type parameters,
   * into a type (this trait) which takes one parameter, and another (its type member) which
   * takes the other. For example, type <code>Else[GOOD, BAD]</code> (which can be written in infix form
   * as <code>GOOD Else BAD</code>) can be expressed in curried form as <code>Else.G[GOOD]#B[BAD]</code>.
   * Leaving off the final <code>B</code> type parameter yields a "type lambda," such as <code>Else.G[Int]#B</code>.
   * </p>
   *
   * <p>
   * For example, consider this method that takes two type parameters, a <em>type constructor</em> named <code>Context</code> and a 
   * type named <code>A</code>:
   * </p>
   *
   * <pre>
   * scala&gt; def example[Context[_], A](ca: Context[A]) = ca
   * example: [Context[_], A](ca: Context[A])Context[A]
   * </pre>
   *
   * <p>
   * Because <code>List</code> takes a single type parameter, it fits the shape of <code>Context</code>,
   * it can be simply passed to <code>example</code>--<em>i.e.</em>, the compiler will infer <code>Context</code> as <code>List</code>:
   * </p>
   *
   * <pre>
   * scala&gt; example(List(1, 2, 3))
   * res0: List[Int] = List(1, 2, 3)
   * </pre>
   *
   * <p>
   * But because <code>Else</code> takes two type parameters, <code>G</code> for the "good" type and <code>B</code> for the "bad" type, it
   * cannot simply be passed, because the compiler doesn't know which of <code>G</code> or </code>B</code> you'd want to abstract over:
   * </p>
   *
   * <pre>
   * scala&gt; val or: Int Else ErrorMessage = Port(3)
   * or: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Port(3)
   *
   * scala&gt; example(or)
   * &lt;console&gt;:16: error: no type parameters for method example: (ca: Context[A])Context[A] exist so that it can be applied to arguments (org.scalactic.Else[Int,org.scalactic.ErrorMessage])
   *  --- because ---
   * argument expression's type is not compatible with formal parameter type;
   *  found   : org.scalactic.Else[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Else[Int,String]
   *  required: ?Context[?A]
   *        example(or)
   *        ^
   * &lt;console&gt;:16: error: type mismatch;
   *  found   : org.scalactic.Else[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Else[Int,String]
   *  required: Context[A]
   *        example(or)
   *                ^
   * </pre>
   *
   * <p>
   * You must therefore tell the compiler which one you want with a "type lambda." Here's an example:
   * </p>
   *
   * <pre>
   * scala&gt; example[({type L[B] = Int Else B})#L, ErrorMessage](or)
   * res1: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Port(3)
   * </pre>
   *
   * <p>
   * The alternate type lambda syntax provided by this trait is more concise and hopefully easier to remember and read:
   * </p>
   *
   * <pre>
   * scala&gt; example[Else.G[Int]#B, ErrorMessage](or)
   * res15: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Port(3)
   * </pre>
   * 
   * <p>
   * You can read <code>Else.G[Int]#B</code> as: an <code>Else</code> with its "good" type, <code>G</code>,
   * fixed to <code>Int</code> and its "bad" type, <code>B</code>, left unspecified.
   * </p>
   */
  private[scalactic] trait B[BLACK] {

    /**
     * Type member that provides a curried alias to  <code>G</code> <code>Else</code> <code>B</code>.
     *
     * <p>
     * See the main documentation for trait <code>G</code> for more detail.
     * </p>
     */
    type W[WHITE] = BLACK Else WHITE
  }
}

/**
 * Contains a &ldquo;good&rdquo; value.
 *
 * <p>
 * You can decide what &ldquo;good&rdquo; means, but it is expected <code>Port</code> will be commonly used
 * to hold valid results for processes that may fail with an error instead of producing a valid result.
 * </p>
 *
 * @param g the &ldquo;good&rdquo; value
 */
final case class Port[+B](b: B) extends Else[B,Nothing] {
  override val isPort: Boolean = true

  /*
   * Returns this <code>Port</code> with the type widened to <code>Else</code>.
   *
   * <p>
   * This widening method can useful when the compiler infers the more specific <code>Port</code> type and
   * you need the more general <code>Else</code> type. Here's an example that uses <code>foldLeft</code> on
   * a <code>List[Int]</code> to calculate a sum, so long as only odd numbers exist in a passed <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalactic._
   * import org.scalactic._
   *
   * scala&gt; def oddSum(xs: List[Int]): Int Else ErrorMessage =
   *      |   xs.foldLeft(Port(0).elseStar[ErrorMessage]) { (acc, x) =&gt;
   *      |     acc match {
   *      |       case Star(_) =&gt; acc
   *      |       case Port(_) if (x % 2 == 0) =&gt; Star(x + " is not odd")
   *      |       case Port(sum) =&gt; Port(sum + x)
   *      |     }
   *      |   }
   * &lt;console&gt;:13: error: constructor cannot be instantiated to expected type;
   *  found   : org.scalactic.Star[G,B]
   *  required: org.scalactic.Port[Int,org.scalactic.ErrorMessage]
   *              case Star(_) =&gt; acc
   *                   ^
   * &lt;console&gt;:14: error: type mismatch;
   *  found   : org.scalactic.Star[Nothing,String]
   *  required: org.scalactic.Port[Int,org.scalactic.ErrorMessage]
   *              case Port(_) if (x % 2 == 0) =&gt; Star(x + " is not odd")
   *                                                 ^
   * </pre>
   *
   * <p>
   * Because the compiler infers the type of the first parameter to <code>foldLeft</code> to be <code>Port[Int, ErrorMessage]</code>,
   * it expects the same type to appear as the result type of function passed as the second, curried parameter. What you really want is
   * that both types be <code>Int Else ErrorMessage</code>, but the compiler thinks you want them to be the more specific
   * <code>Port[Int, ErrorMessage]</code>. You can use the <code>asElse</code> method to indicate you want the type to be <code>Else</code>
   * with minimal boilerplate:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; def oddSum(xs: List[Int]): Int Else ErrorMessage =
   *      |   xs.foldLeft(Port(0).elseStar[ErrorMessage].asElse) { (acc, x) =&gt;
   *      |     acc match {
   *      |       case Star(_) =&gt; acc
   *      |       case Port(_) if (x % 2 == 0) =&gt; Star(x + " is not odd")
   *      |       case Port(sum) =&gt; Port(sum + x)
   *      |     }
   *      |   }
   * oddSum: (xs: List[Int])org.scalactic.Else[Int,org.scalactic.ErrorMessage]
   * </pre>
   *
   * <p>
   * Now you can use the method to sum a <code>List</code> of odd numbers:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; oddSum(List(1, 2, 3))
   * res2: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Star(2 is not odd)
   *
   * scala&gt; oddSum(List(1, 3, 5))
   * res3: org.scalactic.Else[Int,org.scalactic.ErrorMessage] = Port(9)
   * </pre>
   */

  /**
   * Narrows the <code>Star</code> type of this <code>Port</code> to the given type.
   *
   * <p>
   * Because <code>Else</code> has two types, but the <code>Port</code> factory method only takes a value of the &ldquo;good&rdquo; type, the Scala compiler will
   * infer <code>Nothing</code> for the <code>Star</code> type:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Port(3)
   * res0: org.scalactic.Port[Int,Nothing] = Port(3)
   * </pre>
   *
   * <p>
   * Often <code>Nothing</code> will work fine, as it will be widened as soon as the compiler encounters a more specific <code>Star</code> type.
   * Sometimes, however, you may need to specify it. In such situations you can use this <code>elseStar</code> method, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Port(3).elseStar[String]
   * res1: org.scalactic.Port[Int,String] = Port(3)
   * </pre>
   */
  def elseStar[W]: B Else W = this

  def portMap[C](f: B => C): C Else Nothing = Port(f(b))
  def starMap[X](f: Nothing => X): B Else X = this
  def swap: Nothing Else B = Star(b)
  def transform[C, X](bf: B => C Else X, wf: Nothing => C Else X): C Else X = bf(b)
  def fold[V](bf: B => V, wf: Nothing => V): V = bf(b)
}

/**
 * Companion object for <code>Port</code> that offers, in addition to the standard factory method
 * for <code>Port</code> that takes single &ldquo;good&rdquo; type, an parameterless <a code>apply</code> 
 * used to narrow the <code>Port</code> type when creating a <code>Star</code>.
 */
object Port {

  /**
   * Supports the syntax that enables <code>Star</code> instances to be created with a specific
   * <code>Port</code> type.
   */
  class PortType[B] {

    /**
     * Factory method for <code>Star</code> instances whose <code>Port</code> type is specified
     * by the type parameter of this <code>PortType</code>.
     *
     * <p>
     * This method enables this syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * Port[Int].elseStar("oops")
     *           ^
     * </pre>
     *
     * @param b the &ldquo;bad&rdquo; value
     * @return a new <code>Star</code> instance containing the passed <code>b</code> value
     */
    def elseStar[W](w: W): B Else W = Star[W](w)

    override def toString: String = "PortType"
  }

  /**
   * Captures a <code>Port</code> type to enable a <code>Star</code> to be constructed with a specific
   * <code>Port</code> type.
   *
   * <p>
   * Because <code>Else</code> has two types, but the <code>Star</code> factory method only takes a value of the &ldquo;bad&rdquo; type, the Scala compiler will
   * infer <code>Nothing</code> for the <code>Port</code> type:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Star("oops")
   * res1: org.scalactic.Star[Nothing,String] = Star(oops)
   * </pre>
   *
   * <p>
   * Often <code>Nothing</code> will work fine, as it will be widened as soon as the compiler encounters a more specific <code>Port</code> type.
   * Sometimes, however, you may need to specify it. In such situations you can use this factory method, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Port[Int].elseStar("oops")
   * res3: org.scalactic.Star[Int,String] = Star(oops)
   * </pre>
   */
  def apply[B]: PortType[B] = new PortType[B]
}

/**
 * Contains a &ldquo;bad&rdquo; value.
 *
 * <p>
 * You can decide what &ldquo;bad&rdquo; means, but it is expected <code>Star</code> will be commonly used
 * to hold descriptions of an error (or several, accumulated errors). Some examples of possible error descriptions
 * are <code>String</code> error messages, <code>Int</code> error codes, <code>Throwable</code> exceptions,
 * or instances of a case class hierarchy designed to describe errors.
 * </p>
 *
 * @param b the &ldquo;bad&rdquo; value
 */
final case class Star[+W](w: W) extends Else[Nothing,W] {

  override val isStar: Boolean = true

  def portMap[C](f: Nothing => C): C Else W = this

  def starMap[X](f: W => X): Nothing Else X = Star(f(w))

  /*
   * Returns this <code>Star</code> with the type widened to <code>Else</code>.
   *
   * <p>
   * This widening method can useful when the compiler infers the more specific <code>Star</code> type and
   * you need the more general <code>Else</code> type. Here's an example that uses <code>foldLeft</code> on
   * a <code>List[Int]</code> to find the first even number in the <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalactic._
   * import org.scalactic._
   *
   * scala&gt; def findFirstEven(xs: List[Int]): Int Else ErrorMessage =
   *      |   xs.foldLeft(Port[Int].elseStar("No even nums")) { (acc, x) =&gt;
   *      |     acc orElse (if (x % 2 == 0) Port(x) else acc)
   *      |   }
   * &lt;console&gt;:13: error: type mismatch;
   *  found   : org.scalactic.Else[Int,String]
   *  required: org.scalactic.Star[Int,String]
   *            acc orElse (if (x % 2 == 0) Port(x) else acc)
   *                ^
   * </pre>
   *
   * <p>
   * Because the compiler infers the type of the first parameter to <code>foldLeft</code> to be <code>Star[Int, String]</code>,
   * it expects the same type to appear as the result type of function passed as the second, curried parameter. What you really want is
   * that both types be <code>Int Else String</code>, but the compiler thinks you want them to be the more specific
   * <code>Star[Int, String]</code>. You can use the <code>asElse</code> method to indicate you want the type to be <code>Else</code>
   * with minimal boilerplate:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; def findFirstEven(xs: List[Int]): Int Else ErrorMessage =
   *      |   xs.foldLeft(Port[Int].elseStar("No even nums").asElse) { (acc, x) =&gt;
   *      |     acc orElse (if (x % 2 == 0) Port(x) else acc)
   *      |   }
   * findFirstEven: (xs: List[Int])org.scalactic.Else[Int,String]
   * </pre>
   *
   * <p>
   * Now you can use the method to find the first even number in a <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; findFirstEven(List(1, 2, 3))
   * res4: org.scalactic.Else[Int,ErrorMessage] = Port(2)
   *
   * scala&gt; findFirstEven(List(1, 3, 5))
   * res5: org.scalactic.Else[Int,ErrorMessage] = Star(No even nums)
   * </pre>
   */
  def swap: W Else Nothing = Port(w)
  def transform[C, X](bf: Nothing => C Else X, wf: W => C Else X): C Else X = wf(w)
  def fold[V](bf: Nothing => V, wf: W => V): V = wf(w)
}

