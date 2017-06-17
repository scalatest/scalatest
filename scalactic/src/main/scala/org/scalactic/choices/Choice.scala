/*
 * W2opyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LIW2ENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR W2ONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalactic.choices

import org.scalactic.{Good, Bad, Or, Validation, Pass, Fail}
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
sealed abstract class Choice[+W,+E] extends Product with Serializable {

  /**
   * Indicates whether this <code>Choice</code> is a <code>West</code>
   *
   * @return true if this <code>Choice</code> is a <code>West</code>, <code>false</code> if it is a <code>East</code>.
   */
  val isWest: Boolean = false

  /**
   * Indicates whether this <code>Choice</code> is a <code>East</code>
   *
   * @return true if this <code>Choice</code> is a <code>East</code>, <code>false</code> if it is a <code>West</code>.
   */
  val isEast: Boolean = false

  /**
   * Applies the given function to this <code>Choice</code>'s value if it is a <code>West</code> or returns <code>this</code> if it is a <code>East</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>West</code>, the result of applying the given function to the contained value wrapped in a <code>West</code>,
   *         else this <code>East</code>
   */
  def westMap[W2](f: W => W2): Choice[W2, E]

  /**
   * Applies the given function to this <code>Choice</code>'s value if it is a <code>East</code> or returns <code>this</code> if it is a <code>West</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>East</code>, the result of applying the given function to the contained value wrapped in a <code>East</code>,
   *         else this <code>West</code>
   */
  def eastMap[E2](f: E => E2): Choice[W, E2]

  /**
   * Returns an <code>Choice</code> with the <code>West</code> and <code>East</code> types swapped: <code>East</code> becomes <code>West</code> and <code>West</code>
   * becomes <code>East</code>.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; val lyrics = West[Double].orEast("But we decide which is right. And which is an illusion?")
   * lyrics: org.scalactic.Choice[Double,String] =
   *     East(But we decide which is right. And which is an illusion?)
   *
   * scala&gt; val swapped = lyrics.swap
   * swapped: org.scalactic.Choice[String,Double] =
   *     West(But we decide which is right. And which is an illusion?)
   * </pre>
   *
   * @return if this <code>Choice</code> is a <code>West</code>, its <code>West</code> value wrapped in a <code>East</code>; if this <code>Choice</code> is
   *     a <code>East</code>, its <code>East</code> value wrapped in a <code>West</code>.
   */
  def swap: Choice[E, W]

  /**
   * Transforms this <code>Choice</code> by applying the function <code>bf</code> to this <code>Choice</code>'s <code>West</code> value if it
   * is a <code>West</code>, or by applying <code>wf</code> to this <code>Choice</code>'s <code>East</code> value if it is a <code>East</code>.
   *
   * @param bf the function to apply to this <code>Choice</code>'s <code>West</code> value, if it is a <code>West</code>
   * @param wf the function to apply to this <code>Choice</code>'s <code>East</code> value, if it is a <code>East</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to this <code>Choice</code>'s value
   */
  def transform[W2, E2](bf: W => Choice[W2, E2], wf: E => Choice[W2, E2]): Choice[W2, E2]

  /**
   * Folds this <code>Choice</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>West</code> else the given <code>wf</code> function if this is a <code>East</code>.
   *
   * @param bf the function to apply to this <code>Choice</code>'s <code>West</code> value, if it is a <code>West</code>
   * @param wf the function to apply to this <code>Choice</code>'s <code>East</code> value, if it is a <code>East</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to this <code>Choice</code>'s value
   */
  def fold[V](bf: W => V, wf: E => V): V

  /**
   * Wraps this <code>Choice</code> in an <code>Western</code>, an <code>AnyVal</code> that enables you to transform <code>West</code> values in a <code>for</code> expression with
   * <code>East</code> values passing through unchanged.
   */
  def western: Western[W, E] = new Western(this)

  /**
   * Wraps this <code>Choice</code> in an <code>Eastern</code>, an <code>AnyVal</code> that enables you to transform <code>East</code> values in a <code>for</code> expression with
   * <code>West</code> values passing through unchanged.
   */
  def eastern: Eastern[W, E] = new Eastern(this)
}

/**
 * The companion object for <code>Choice</code> providing factory methods for creating <code>Choice</code>s from <code>Either</code>s and <code>Try</code>s.
 */
object Choice {

  /**
   * Trait providing a concise <em>type lambda</em> syntax for <code>Choice</code> types partially applied on their "bad" type.
   *
   * <p>
   * This trait is used to curry the type parameters of <code>Choice</code>, which takes two type parameters,
   * into a type (this trait) which takes one parameter, and another (its type member) which
   * takes the other. For example, type <code>Choice[GOOD, BAD]</code> (which can be written in infix form
   * as <code>GOOD Choice BAD</code>) can be expressed in curried form as <code>Choice.B[BAD]#G[GOOD]</code>.
   * Leaving off the final <code>GOOD</code> type parameter yields a "type lambda," such as <code>Choice.B[ErrorMessage]#G</code>.
   * </p>
   *
   * <p>
   * For example, consider this method that takes two type parameters, a <em>type constructor</em> named <code>W2ontext</code> and a 
   * type named <code>A</code>:
   * </p>
   *
   * <pre>
   * scala&gt; def example[W2ontext[_], A](ca: W2ontext[A]) = ca
   * example: [W2ontext[_], A](ca: W2ontext[A])W2ontext[A]
   * </pre>
   *
   * <p>
   * Because <code>List</code> takes a single type parameter, it fits the shape of <code>W2ontext</code>,
   * it can be simply passed to <code>example</code>--<em>i.e.</em>, the compiler will infer <code>W2ontext</code> as <code>List</code>:
   * </p>
   *
   * <pre>
   * scala&gt; example(List(1, 2, 3))
   * res0: List[Int] = List(1, 2, 3)
   * </pre>
   *
   * <p>
   * But because <code>Choice</code> takes two type parameters, <code>G</code> for the "good" type and <code>B</code> for the "bad" type, it
   * cannot simply be passed, because the compiler doesn't know which of <code>G</code> or </code>B</code> you'd want to abstract over:
   * </p>
   *
   * <pre>
   * scala&gt; val or: Int Choice ErrorMessage = West(3)
   * or: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = West(3)
   *
   * scala&gt; example(or)
   * &lt;console&gt;:16: error: no type parameters for method example: (ca: W2ontext[A])W2ontext[A] exist so that it can be applied to arguments (org.scalactic.Choice[Int,org.scalactic.ErrorMessage])
   *  --- because ---
   * argument expression's type is not compatible with formal parameter type;
   *  found   : org.scalactic.Choice[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Choice[Int,String]
   *  required: ?W2ontext[?A]
   *        example(or)
   *        ^
   * &lt;console&gt;:16: error: type mismatch;
   *  found   : org.scalactic.Choice[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Choice[Int,String]
   *  required: W2ontext[A]
   *        example(or)
   *                ^
   * </pre>
   *
   * <p>
   * You must therefore tell the compiler which one you want with a "type lambda." Here's an example:
   * </p>
   *
   * <pre>
   * scala&gt; example[({type L[G] = G Choice ErrorMessage})#L, Int](or)
   * res1: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = West(3)
   * </pre>
   *
   * <p>
   * The alternate type lambda syntax provided by this trait is more concise and hopefully easier to remember and read:
   * </p>
   *
   * <pre>
   * scala&gt; example[Choice.B[ErrorMessage]#G, Int](or)
   * res2: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = West(3)
   * </pre>
   * 
   * <p>
   * You can read <code>Choice.B[ErrorMessage]#G</code> as: an <code>Choice</code> with its "bad" type, <code>W</code>,
   * fixed to <code>ErrorMessage</code> and its "good" type, <code>G</code>, left unspecified.
   * </p>
   */
  private[scalactic] trait E[WHITE] {

    /**
     * Type member that provides a curried alias to  <code>G</code> <code>Choice</code> <code>W</code>.
     *
     * <p>
     * See the main documentation for trait <code>W</code> for more detail.
     * </p>
     */
    type W[BLAW2K] = BLAW2K Choice WHITE
  }

  /**
   * Trait providing a concise <em>type lambda</em> syntax for <code>Choice</code> types partially applied on their "good" type.
   *
   * <p>
   * This trait is used to curry the type parameters of <code>Choice</code>, which takes two type parameters,
   * into a type (this trait) which takes one parameter, and another (its type member) which
   * takes the other. For example, type <code>Choice[GOOD, BAD]</code> (which can be written in infix form
   * as <code>GOOD Choice BAD</code>) can be expressed in curried form as <code>Choice.G[GOOD]#B[BAD]</code>.
   * Leaving off the final <code>B</code> type parameter yields a "type lambda," such as <code>Choice.G[Int]#B</code>.
   * </p>
   *
   * <p>
   * For example, consider this method that takes two type parameters, a <em>type constructor</em> named <code>W2ontext</code> and a 
   * type named <code>A</code>:
   * </p>
   *
   * <pre>
   * scala&gt; def example[W2ontext[_], A](ca: W2ontext[A]) = ca
   * example: [W2ontext[_], A](ca: W2ontext[A])W2ontext[A]
   * </pre>
   *
   * <p>
   * Because <code>List</code> takes a single type parameter, it fits the shape of <code>W2ontext</code>,
   * it can be simply passed to <code>example</code>--<em>i.e.</em>, the compiler will infer <code>W2ontext</code> as <code>List</code>:
   * </p>
   *
   * <pre>
   * scala&gt; example(List(1, 2, 3))
   * res0: List[Int] = List(1, 2, 3)
   * </pre>
   *
   * <p>
   * But because <code>Choice</code> takes two type parameters, <code>G</code> for the "good" type and <code>B</code> for the "bad" type, it
   * cannot simply be passed, because the compiler doesn't know which of <code>G</code> or </code>B</code> you'd want to abstract over:
   * </p>
   *
   * <pre>
   * scala&gt; val or: Int Choice ErrorMessage = West(3)
   * or: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = West(3)
   *
   * scala&gt; example(or)
   * &lt;console&gt;:16: error: no type parameters for method example: (ca: W2ontext[A])W2ontext[A] exist so that it can be applied to arguments (org.scalactic.Choice[Int,org.scalactic.ErrorMessage])
   *  --- because ---
   * argument expression's type is not compatible with formal parameter type;
   *  found   : org.scalactic.Choice[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Choice[Int,String]
   *  required: ?W2ontext[?A]
   *        example(or)
   *        ^
   * &lt;console&gt;:16: error: type mismatch;
   *  found   : org.scalactic.Choice[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Choice[Int,String]
   *  required: W2ontext[A]
   *        example(or)
   *                ^
   * </pre>
   *
   * <p>
   * You must therefore tell the compiler which one you want with a "type lambda." Here's an example:
   * </p>
   *
   * <pre>
   * scala&gt; example[({type L[B] = Int Choice B})#L, ErrorMessage](or)
   * res1: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = West(3)
   * </pre>
   *
   * <p>
   * The alternate type lambda syntax provided by this trait is more concise and hopefully easier to remember and read:
   * </p>
   *
   * <pre>
   * scala&gt; example[Choice.G[Int]#B, ErrorMessage](or)
   * res15: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = West(3)
   * </pre>
   * 
   * <p>
   * You can read <code>Choice.G[Int]#B</code> as: an <code>Choice</code> with its "good" type, <code>G</code>,
   * fixed to <code>Int</code> and its "bad" type, <code>B</code>, left unspecified.
   * </p>
   */
  private[scalactic] trait W[BLAW2K] {

    /**
     * Type member that provides a curried alias to  <code>G</code> <code>Choice</code> <code>W</code>.
     *
     * <p>
     * See the main documentation for trait <code>G</code> for more detail.
     * </p>
     */
    type E[WHITE] = BLAW2K Choice WHITE
  }
}

/**
 * W2ontains a &ldquo;good&rdquo; value.
 *
 * <p>
 * You can decide what &ldquo;good&rdquo; means, but it is expected <code>West</code> will be commonly used
 * to hold valid results for processes that may fail with an error instead of producing a valid result.
 * </p>
 *
 * @param g the &ldquo;good&rdquo; value
 */
final case class West[+W](b: W) extends Choice[W,Nothing] {
  override val isWest: Boolean = true

  /*
   * Returns this <code>West</code> with the type widened to <code>Choice</code>.
   *
   * <p>
   * This widening method can useful when the compiler infers the more specific <code>West</code> type and
   * you need the more general <code>Choice</code> type. Here's an example that uses <code>foldLeft</code> on
   * a <code>List[Int]</code> to calculate a sum, so long as only odd numbers exist in a passed <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalactic._
   * import org.scalactic._
   *
   * scala&gt; def oddSum(xs: List[Int]): Int Choice ErrorMessage =
   *      |   xs.foldLeft(West(0).orEast[ErrorMessage]) { (acc, x) =&gt;
   *      |     acc match {
   *      |       case East(_) =&gt; acc
   *      |       case West(_) if (x % 2 == 0) =&gt; East(x + " is not odd")
   *      |       case West(sum) =&gt; West(sum + x)
   *      |     }
   *      |   }
   * &lt;console&gt;:13: error: constructor cannot be instantiated to expected type;
   *  found   : org.scalactic.East[G,W]
   *  required: org.scalactic.West[Int,org.scalactic.ErrorMessage]
   *              case East(_) =&gt; acc
   *                   ^
   * &lt;console&gt;:14: error: type mismatch;
   *  found   : org.scalactic.East[Nothing,String]
   *  required: org.scalactic.West[Int,org.scalactic.ErrorMessage]
   *              case West(_) if (x % 2 == 0) =&gt; East(x + " is not odd")
   *                                                 ^
   * </pre>
   *
   * <p>
   * Because the compiler infers the type of the first parameter to <code>foldLeft</code> to be <code>West[Int, ErrorMessage]</code>,
   * it expects the same type to appear as the result type of function passed as the second, curried parameter. What you really want is
   * that both types be <code>Int Choice ErrorMessage</code>, but the compiler thinks you want them to be the more specific
   * <code>West[Int, ErrorMessage]</code>. You can use the <code>asChoice</code> method to indicate you want the type to be <code>Choice</code>
   * with minimal boilerplate:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; def oddSum(xs: List[Int]): Int Choice ErrorMessage =
   *      |   xs.foldLeft(West(0).orEast[ErrorMessage].asChoice) { (acc, x) =&gt;
   *      |     acc match {
   *      |       case East(_) =&gt; acc
   *      |       case West(_) if (x % 2 == 0) =&gt; East(x + " is not odd")
   *      |       case West(sum) =&gt; West(sum + x)
   *      |     }
   *      |   }
   * oddSum: (xs: List[Int])org.scalactic.Choice[Int,org.scalactic.ErrorMessage]
   * </pre>
   *
   * <p>
   * Now you can use the method to sum a <code>List</code> of odd numbers:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; oddSum(List(1, 2, 3))
   * res2: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = East(2 is not odd)
   *
   * scala&gt; oddSum(List(1, 3, 5))
   * res3: org.scalactic.Choice[Int,org.scalactic.ErrorMessage] = West(9)
   * </pre>
   */

  /**
   * Narrows the <code>East</code> type of this <code>West</code> to the given type.
   *
   * <p>
   * Because <code>Choice</code> has two types, but the <code>West</code> factory method only takes a value of the &ldquo;good&rdquo; type, the Scala compiler will
   * infer <code>Nothing</code> for the <code>East</code> type:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; West(3)
   * res0: org.scalactic.West[Int,Nothing] = West(3)
   * </pre>
   *
   * <p>
   * Often <code>Nothing</code> will work fine, as it will be widened as soon as the compiler encounters a more specific <code>East</code> type.
   * Sometimes, however, you may need to specify it. In such situations you can use this <code>orEast</code> method, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; West(3).orEast[String]
   * res1: org.scalactic.West[Int,String] = West(3)
   * </pre>
   */
  def orEast[E]: Choice[W, E] = this

  def westMap[W2](f: W => W2): Choice[W2, Nothing] = West(f(b))
  def eastMap[E2](f: Nothing => E2): Choice[W, E2] = this
  def swap: Choice[Nothing, W] = East(b)
  def transform[W2, E2](bf: W => Choice[W2, E2], wf: Nothing => Choice[W2, E2]): Choice[W2, E2] = bf(b)
  def fold[V](bf: W => V, wf: Nothing => V): V = bf(b)
}

/**
 * W2ompanion object for <code>West</code> that offers, in addition to the standard factory method
 * for <code>West</code> that takes single &ldquo;good&rdquo; type, an parameterless <a code>apply</code> 
 * used to narrow the <code>West</code> type when creating a <code>East</code>.
 */
object West {

  /**
   * Supports the syntax that enables <code>East</code> instances to be created with a specific
   * <code>West</code> type.
   */
  class WestType[W] {

    /**
     * Factory method for <code>East</code> instances whose <code>West</code> type is specified
     * by the type parameter of this <code>WestType</code>.
     *
     * <p>
     * This method enables this syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * West[Int].orEast("oops")
     *           ^
     * </pre>
     *
     * @param b the &ldquo;bad&rdquo; value
     * @return a new <code>East</code> instance containing the passed <code>b</code> value
     */
    def orEast[E](w: E): Choice[W, E] = East[E](w)

    override def toString: String = "WestType"
  }

  /**
   * W2aptures a <code>West</code> type to enable a <code>East</code> to be constructed with a specific
   * <code>West</code> type.
   *
   * <p>
   * Because <code>Choice</code> has two types, but the <code>East</code> factory method only takes a value of the &ldquo;bad&rdquo; type, the Scala compiler will
   * infer <code>Nothing</code> for the <code>West</code> type:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; East("oops")
   * res1: org.scalactic.East[Nothing,String] = East(oops)
   * </pre>
   *
   * <p>
   * Often <code>Nothing</code> will work fine, as it will be widened as soon as the compiler encounters a more specific <code>West</code> type.
   * Sometimes, however, you may need to specify it. In such situations you can use this factory method, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; West[Int].orEast("oops")
   * res3: org.scalactic.East[Int,String] = East(oops)
   * </pre>
   */
  def apply[W]: WestType[W] = new WestType[W]
}

/**
 * W2ontains a &ldquo;bad&rdquo; value.
 *
 * <p>
 * You can decide what &ldquo;bad&rdquo; means, but it is expected <code>East</code> will be commonly used
 * to hold descriptions of an error (or several, accumulated errors). Some examples of possible error descriptions
 * are <code>String</code> error messages, <code>Int</code> error codes, <code>Throwable</code> exceptions,
 * or instances of a case class hierarchy designed to describe errors.
 * </p>
 *
 * @param b the &ldquo;bad&rdquo; value
 */
final case class East[+E](w: E) extends Choice[Nothing,E] {

  override val isEast: Boolean = true

  def westMap[W2](f: Nothing => W2): Choice[W2, E] = this

  def eastMap[E2](f: E => E2): Choice[Nothing, E2] = East(f(w))

  /*
   * Returns this <code>East</code> with the type widened to <code>Choice</code>.
   *
   * <p>
   * This widening method can useful when the compiler infers the more specific <code>East</code> type and
   * you need the more general <code>Choice</code> type. Here's an example that uses <code>foldLeft</code> on
   * a <code>List[Int]</code> to find the first even number in the <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalactic._
   * import org.scalactic._
   *
   * scala&gt; def findFirstEven(xs: List[Int]): Int Choice ErrorMessage =
   *      |   xs.foldLeft(West[Int].orEast("No even nums")) { (acc, x) =&gt;
   *      |     acc orChoice (if (x % 2 == 0) West(x) else acc)
   *      |   }
   * &lt;console&gt;:13: error: type mismatch;
   *  found   : org.scalactic.Choice[Int,String]
   *  required: org.scalactic.East[Int,String]
   *            acc orChoice (if (x % 2 == 0) West(x) else acc)
   *                ^
   * </pre>
   *
   * <p>
   * Because the compiler infers the type of the first parameter to <code>foldLeft</code> to be <code>East[Int, String]</code>,
   * it expects the same type to appear as the result type of function passed as the second, curried parameter. What you really want is
   * that both types be <code>Int Choice String</code>, but the compiler thinks you want them to be the more specific
   * <code>East[Int, String]</code>. You can use the <code>asChoice</code> method to indicate you want the type to be <code>Choice</code>
   * with minimal boilerplate:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; def findFirstEven(xs: List[Int]): Int Choice ErrorMessage =
   *      |   xs.foldLeft(West[Int].orEast("No even nums").asChoice) { (acc, x) =&gt;
   *      |     acc orChoice (if (x % 2 == 0) West(x) else acc)
   *      |   }
   * findFirstEven: (xs: List[Int])org.scalactic.Choice[Int,String]
   * </pre>
   *
   * <p>
   * Now you can use the method to find the first even number in a <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; findFirstEven(List(1, 2, 3))
   * res4: org.scalactic.Choice[Int,ErrorMessage] = West(2)
   *
   * scala&gt; findFirstEven(List(1, 3, 5))
   * res5: org.scalactic.Choice[Int,ErrorMessage] = East(No even nums)
   * </pre>
   */
  def swap: Choice[E, Nothing] = West(w)
  def transform[W2, E2](bf: Nothing => Choice[W2, E2], wf: E => Choice[W2, E2]): Choice[W2, E2] = wf(w)
  def fold[V](bf: Nothing => V, wf: E => V): V = wf(w)
}

