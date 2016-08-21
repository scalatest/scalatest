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
sealed abstract class Otherwise[+B,+W] extends Product with Serializable {

  /**
   * Indicates whether this <code>Otherwise</code> is a <code>Black</code>
   *
   * @return true if this <code>Otherwise</code> is a <code>Black</code>, <code>false</code> if it is a <code>White</code>.
   */
  val isBlack: Boolean = false

  /**
   * Indicates whether this <code>Otherwise</code> is a <code>White</code>
   *
   * @return true if this <code>Otherwise</code> is a <code>White</code>, <code>false</code> if it is a <code>Black</code>.
   */
  val isWhite: Boolean = false

  /**
   * Applies the given function to this <code>Otherwise</code>'s value if it is a <code>Black</code> or returns <code>this</code> if it is a <code>White</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>Black</code>, the result of applying the given function to the contained value wrapped in a <code>Black</code>,
   *         else this <code>White</code>
   */
  def blackMap[C](f: B => C): C Otherwise W

  /**
   * Applies the given function to this <code>Otherwise</code>'s value if it is a <code>White</code> or returns <code>this</code> if it is a <code>Black</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>White</code>, the result of applying the given function to the contained value wrapped in a <code>White</code>,
   *         else this <code>Black</code>
   */
  def whiteMap[X](f: W => X): B Otherwise X

  /**
   * Returns an <code>Otherwise</code> with the <code>Black</code> and <code>White</code> types swapped: <code>White</code> becomes <code>Black</code> and <code>Black</code>
   * becomes <code>White</code>.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; val lyrics = Black[Double].otherwiseWhite("But we decide which is right. And which is an illusion?")
   * lyrics: org.scalactic.Otherwise[Double,String] =
   *     White(But we decide which is right. And which is an illusion?)
   *
   * scala&gt; val swapped = lyrics.swap
   * swapped: org.scalactic.Otherwise[String,Double] =
   *     Black(But we decide which is right. And which is an illusion?)
   * </pre>
   *
   * @return if this <code>Otherwise</code> is a <code>Black</code>, its <code>Black</code> value wrapped in a <code>White</code>; if this <code>Otherwise</code> is
   *     a <code>White</code>, its <code>White</code> value wrapped in a <code>Black</code>.
   */
  def swap: W Otherwise B

  /**
   * Transforms this <code>Otherwise</code> by applying the function <code>bf</code> to this <code>Otherwise</code>'s <code>Black</code> value if it
   * is a <code>Black</code>, or by applying <code>wf</code> to this <code>Otherwise</code>'s <code>White</code> value if it is a <code>White</code>.
   *
   * @param bf the function to apply to this <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param wf the function to apply to this <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to this <code>Otherwise</code>'s value
   */
  def transform[C, X](bf: B => C Otherwise X, wf: W => C Otherwise X): C Otherwise X

  /**
   * Folds this <code>Otherwise</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>Black</code> else the given <code>wf</code> function if this is a <code>White</code>.
   *
   * @param bf the function to apply to this <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param wf the function to apply to this <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to this <code>Otherwise</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V

  /**
   * Wraps this <code>Otherwise</code> in an <code>Ebony</code>, an <code>AnyVal</code> that enables you to transform <code>Black</code> values in a <code>for</code> expression with
   * <code>White</code> values passing through unchanged.
   */
  def ebony: Ebony[B, W] = new Ebony(this)

  /**
   * Wraps this <code>Otherwise</code> in an <code>Ivory</code>, an <code>AnyVal</code> that enables you to transform <code>White</code> values in a <code>for</code> expression with
   * <code>Black</code> values passing through unchanged.
   */
  def ivory: Ivory[B, W] = new Ivory(this)
}

/**
 * The companion object for <code>Otherwise</code> providing factory methods for creating <code>Otherwise</code>s from <code>Either</code>s and <code>Try</code>s.
 */
object Otherwise {

  /**
   * Trait providing a concise <em>type lambda</em> syntax for <code>Otherwise</code> types partially applied on their "bad" type.
   *
   * <p>
   * This trait is used to curry the type parameters of <code>Otherwise</code>, which takes two type parameters,
   * into a type (this trait) which takes one parameter, and another (its type member) which
   * takes the other. For example, type <code>Otherwise[GOOD, BAD]</code> (which can be written in infix form
   * as <code>GOOD Otherwise BAD</code>) can be expressed in curried form as <code>Otherwise.B[BAD]#G[GOOD]</code>.
   * Leaving off the final <code>GOOD</code> type parameter yields a "type lambda," such as <code>Otherwise.B[ErrorMessage]#G</code>.
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
   * But because <code>Otherwise</code> takes two type parameters, <code>G</code> for the "good" type and <code>B</code> for the "bad" type, it
   * cannot simply be passed, because the compiler doesn't know which of <code>G</code> or </code>B</code> you'd want to abstract over:
   * </p>
   *
   * <pre>
   * scala&gt; val or: Int Otherwise ErrorMessage = Black(3)
   * or: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = Black(3)
   *
   * scala&gt; example(or)
   * &lt;console&gt;:16: error: no type parameters for method example: (ca: Context[A])Context[A] exist so that it can be applied to arguments (org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage])
   *  --- because ---
   * argument expression's type is not compatible with formal parameter type;
   *  found   : org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Otherwise[Int,String]
   *  required: ?Context[?A]
   *        example(or)
   *        ^
   * &lt;console&gt;:16: error: type mismatch;
   *  found   : org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Otherwise[Int,String]
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
   * scala&gt; example[({type L[G] = G Otherwise ErrorMessage})#L, Int](or)
   * res1: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = Black(3)
   * </pre>
   *
   * <p>
   * The alternate type lambda syntax provided by this trait is more concise and hopefully easier to remember and read:
   * </p>
   *
   * <pre>
   * scala&gt; example[Otherwise.B[ErrorMessage]#G, Int](or)
   * res2: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = Black(3)
   * </pre>
   * 
   * <p>
   * You can read <code>Otherwise.B[ErrorMessage]#G</code> as: an <code>Otherwise</code> with its "bad" type, <code>B</code>,
   * fixed to <code>ErrorMessage</code> and its "good" type, <code>G</code>, left unspecified.
   * </p>
   */
  private[scalactic] trait W[WHITE] {

    /**
     * Type member that provides a curried alias to  <code>G</code> <code>Otherwise</code> <code>B</code>.
     *
     * <p>
     * See the main documentation for trait <code>B</code> for more detail.
     * </p>
     */
    type B[BLACK] = BLACK Otherwise WHITE
  }

  /**
   * Trait providing a concise <em>type lambda</em> syntax for <code>Otherwise</code> types partially applied on their "good" type.
   *
   * <p>
   * This trait is used to curry the type parameters of <code>Otherwise</code>, which takes two type parameters,
   * into a type (this trait) which takes one parameter, and another (its type member) which
   * takes the other. For example, type <code>Otherwise[GOOD, BAD]</code> (which can be written in infix form
   * as <code>GOOD Otherwise BAD</code>) can be expressed in curried form as <code>Otherwise.G[GOOD]#B[BAD]</code>.
   * Leaving off the final <code>B</code> type parameter yields a "type lambda," such as <code>Otherwise.G[Int]#B</code>.
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
   * But because <code>Otherwise</code> takes two type parameters, <code>G</code> for the "good" type and <code>B</code> for the "bad" type, it
   * cannot simply be passed, because the compiler doesn't know which of <code>G</code> or </code>B</code> you'd want to abstract over:
   * </p>
   *
   * <pre>
   * scala&gt; val or: Int Otherwise ErrorMessage = Black(3)
   * or: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = Black(3)
   *
   * scala&gt; example(or)
   * &lt;console&gt;:16: error: no type parameters for method example: (ca: Context[A])Context[A] exist so that it can be applied to arguments (org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage])
   *  --- because ---
   * argument expression's type is not compatible with formal parameter type;
   *  found   : org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Otherwise[Int,String]
   *  required: ?Context[?A]
   *        example(or)
   *        ^
   * &lt;console&gt;:16: error: type mismatch;
   *  found   : org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage]
   *     (which expands to)  org.scalactic.Otherwise[Int,String]
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
   * scala&gt; example[({type L[B] = Int Otherwise B})#L, ErrorMessage](or)
   * res1: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = Black(3)
   * </pre>
   *
   * <p>
   * The alternate type lambda syntax provided by this trait is more concise and hopefully easier to remember and read:
   * </p>
   *
   * <pre>
   * scala&gt; example[Otherwise.G[Int]#B, ErrorMessage](or)
   * res15: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = Black(3)
   * </pre>
   * 
   * <p>
   * You can read <code>Otherwise.G[Int]#B</code> as: an <code>Otherwise</code> with its "good" type, <code>G</code>,
   * fixed to <code>Int</code> and its "bad" type, <code>B</code>, left unspecified.
   * </p>
   */
  private[scalactic] trait B[BLACK] {

    /**
     * Type member that provides a curried alias to  <code>G</code> <code>Otherwise</code> <code>B</code>.
     *
     * <p>
     * See the main documentation for trait <code>G</code> for more detail.
     * </p>
     */
    type W[WHITE] = BLACK Otherwise WHITE
  }
}

/**
 * Contains a &ldquo;good&rdquo; value.
 *
 * <p>
 * You can decide what &ldquo;good&rdquo; means, but it is expected <code>Black</code> will be commonly used
 * to hold valid results for processes that may fail with an error instead of producing a valid result.
 * </p>
 *
 * @param g the &ldquo;good&rdquo; value
 */
final case class Black[+B](b: B) extends Otherwise[B,Nothing] {
  override val isBlack: Boolean = true

  /*
   * Returns this <code>Black</code> with the type widened to <code>Otherwise</code>.
   *
   * <p>
   * This widening method can useful when the compiler infers the more specific <code>Black</code> type and
   * you need the more general <code>Otherwise</code> type. Here's an example that uses <code>foldLeft</code> on
   * a <code>List[Int]</code> to calculate a sum, so long as only odd numbers exist in a passed <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalactic._
   * import org.scalactic._
   *
   * scala&gt; def oddSum(xs: List[Int]): Int Otherwise ErrorMessage =
   *      |   xs.foldLeft(Black(0).otherwiseWhite[ErrorMessage]) { (acc, x) =&gt;
   *      |     acc match {
   *      |       case White(_) =&gt; acc
   *      |       case Black(_) if (x % 2 == 0) =&gt; White(x + " is not odd")
   *      |       case Black(sum) =&gt; Black(sum + x)
   *      |     }
   *      |   }
   * &lt;console&gt;:13: error: constructor cannot be instantiated to expected type;
   *  found   : org.scalactic.White[G,B]
   *  required: org.scalactic.Black[Int,org.scalactic.ErrorMessage]
   *              case White(_) =&gt; acc
   *                   ^
   * &lt;console&gt;:14: error: type mismatch;
   *  found   : org.scalactic.White[Nothing,String]
   *  required: org.scalactic.Black[Int,org.scalactic.ErrorMessage]
   *              case Black(_) if (x % 2 == 0) =&gt; White(x + " is not odd")
   *                                                 ^
   * </pre>
   *
   * <p>
   * Because the compiler infers the type of the first parameter to <code>foldLeft</code> to be <code>Black[Int, ErrorMessage]</code>,
   * it expects the same type to appear as the result type of function passed as the second, curried parameter. What you really want is
   * that both types be <code>Int Otherwise ErrorMessage</code>, but the compiler thinks you want them to be the more specific
   * <code>Black[Int, ErrorMessage]</code>. You can use the <code>asOtherwise</code> method to indicate you want the type to be <code>Otherwise</code>
   * with minimal boilerplate:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; def oddSum(xs: List[Int]): Int Otherwise ErrorMessage =
   *      |   xs.foldLeft(Black(0).otherwiseWhite[ErrorMessage].asOtherwise) { (acc, x) =&gt;
   *      |     acc match {
   *      |       case White(_) =&gt; acc
   *      |       case Black(_) if (x % 2 == 0) =&gt; White(x + " is not odd")
   *      |       case Black(sum) =&gt; Black(sum + x)
   *      |     }
   *      |   }
   * oddSum: (xs: List[Int])org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage]
   * </pre>
   *
   * <p>
   * Now you can use the method to sum a <code>List</code> of odd numbers:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; oddSum(List(1, 2, 3))
   * res2: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = White(2 is not odd)
   *
   * scala&gt; oddSum(List(1, 3, 5))
   * res3: org.scalactic.Otherwise[Int,org.scalactic.ErrorMessage] = Black(9)
   * </pre>
   */

  /**
   * Narrows the <code>White</code> type of this <code>Black</code> to the given type.
   *
   * <p>
   * Because <code>Otherwise</code> has two types, but the <code>Black</code> factory method only takes a value of the &ldquo;good&rdquo; type, the Scala compiler will
   * infer <code>Nothing</code> for the <code>White</code> type:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Black(3)
   * res0: org.scalactic.Black[Int,Nothing] = Black(3)
   * </pre>
   *
   * <p>
   * Often <code>Nothing</code> will work fine, as it will be widened as soon as the compiler encounters a more specific <code>White</code> type.
   * Sometimes, however, you may need to specify it. In such situations you can use this <code>otherwiseWhite</code> method, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Black(3).otherwiseWhite[String]
   * res1: org.scalactic.Black[Int,String] = Black(3)
   * </pre>
   */
  def otherwiseWhite[W]: B Otherwise W = this

  def blackMap[C](f: B => C): C Otherwise Nothing = Black(f(b))
  def whiteMap[X](f: Nothing => X): B Otherwise X = this
  def swap: Nothing Otherwise B = White(b)
  def transform[C, X](bf: B => C Otherwise X, wf: Nothing => C Otherwise X): C Otherwise X = bf(b)
  def fold[V](bf: B => V, wf: Nothing => V): V = bf(b)
}

/**
 * Companion object for <code>Black</code> that offers, in addition to the standard factory method
 * for <code>Black</code> that takes single &ldquo;good&rdquo; type, an parameterless <a code>apply</code> 
 * used to narrow the <code>Black</code> type when creating a <code>White</code>.
 */
object Black {

  /**
   * Supports the syntax that enables <code>White</code> instances to be created with a specific
   * <code>Black</code> type.
   */
  class BlackType[B] {

    /**
     * Factory method for <code>White</code> instances whose <code>Black</code> type is specified
     * by the type parameter of this <code>BlackType</code>.
     *
     * <p>
     * This method enables this syntax:
     * </p>
     *
     * <pre class="stHighlight">
     * Black[Int].otherwiseWhite("oops")
     *           ^
     * </pre>
     *
     * @param b the &ldquo;bad&rdquo; value
     * @return a new <code>White</code> instance containing the passed <code>b</code> value
     */
    def otherwiseWhite[W](w: W): B Otherwise W = White[W](w)

    override def toString: String = "BlackType"
  }

  /**
   * Captures a <code>Black</code> type to enable a <code>White</code> to be constructed with a specific
   * <code>Black</code> type.
   *
   * <p>
   * Because <code>Otherwise</code> has two types, but the <code>White</code> factory method only takes a value of the &ldquo;bad&rdquo; type, the Scala compiler will
   * infer <code>Nothing</code> for the <code>Black</code> type:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; White("oops")
   * res1: org.scalactic.White[Nothing,String] = White(oops)
   * </pre>
   *
   * <p>
   * Often <code>Nothing</code> will work fine, as it will be widened as soon as the compiler encounters a more specific <code>Black</code> type.
   * Sometimes, however, you may need to specify it. In such situations you can use this factory method, like this:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Black[Int].otherwiseWhite("oops")
   * res3: org.scalactic.White[Int,String] = White(oops)
   * </pre>
   */
  def apply[B]: BlackType[B] = new BlackType[B]
}

/**
 * Contains a &ldquo;bad&rdquo; value.
 *
 * <p>
 * You can decide what &ldquo;bad&rdquo; means, but it is expected <code>White</code> will be commonly used
 * to hold descriptions of an error (or several, accumulated errors). Some examples of possible error descriptions
 * are <code>String</code> error messages, <code>Int</code> error codes, <code>Throwable</code> exceptions,
 * or instances of a case class hierarchy designed to describe errors.
 * </p>
 *
 * @param b the &ldquo;bad&rdquo; value
 */
final case class White[+W](w: W) extends Otherwise[Nothing,W] {

  override val isWhite: Boolean = true

  def blackMap[C](f: Nothing => C): C Otherwise W = this

  def whiteMap[X](f: W => X): Nothing Otherwise X = White(f(w))

  /*
   * Returns this <code>White</code> with the type widened to <code>Otherwise</code>.
   *
   * <p>
   * This widening method can useful when the compiler infers the more specific <code>White</code> type and
   * you need the more general <code>Otherwise</code> type. Here's an example that uses <code>foldLeft</code> on
   * a <code>List[Int]</code> to find the first even number in the <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; import org.scalactic._
   * import org.scalactic._
   *
   * scala&gt; def findFirstEven(xs: List[Int]): Int Otherwise ErrorMessage =
   *      |   xs.foldLeft(Black[Int].otherwiseWhite("No even nums")) { (acc, x) =&gt;
   *      |     acc orOtherwise (if (x % 2 == 0) Black(x) else acc)
   *      |   }
   * &lt;console&gt;:13: error: type mismatch;
   *  found   : org.scalactic.Otherwise[Int,String]
   *  required: org.scalactic.White[Int,String]
   *            acc orOtherwise (if (x % 2 == 0) Black(x) else acc)
   *                ^
   * </pre>
   *
   * <p>
   * Because the compiler infers the type of the first parameter to <code>foldLeft</code> to be <code>White[Int, String]</code>,
   * it expects the same type to appear as the result type of function passed as the second, curried parameter. What you really want is
   * that both types be <code>Int Otherwise String</code>, but the compiler thinks you want them to be the more specific
   * <code>White[Int, String]</code>. You can use the <code>asOtherwise</code> method to indicate you want the type to be <code>Otherwise</code>
   * with minimal boilerplate:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; def findFirstEven(xs: List[Int]): Int Otherwise ErrorMessage =
   *      |   xs.foldLeft(Black[Int].otherwiseWhite("No even nums").asOtherwise) { (acc, x) =&gt;
   *      |     acc orOtherwise (if (x % 2 == 0) Black(x) else acc)
   *      |   }
   * findFirstEven: (xs: List[Int])org.scalactic.Otherwise[Int,String]
   * </pre>
   *
   * <p>
   * Now you can use the method to find the first even number in a <code>List</code>:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; findFirstEven(List(1, 2, 3))
   * res4: org.scalactic.Otherwise[Int,ErrorMessage] = Black(2)
   *
   * scala&gt; findFirstEven(List(1, 3, 5))
   * res5: org.scalactic.Otherwise[Int,ErrorMessage] = White(No even nums)
   * </pre>
   */
  def swap: W Otherwise Nothing = Black(w)
  def transform[C, X](bf: Nothing => C Otherwise X, wf: W => C Otherwise X): C Otherwise X = wf(w)
  def fold[V](bf: Nothing => V, wf: W => V): V = wf(w)
}

