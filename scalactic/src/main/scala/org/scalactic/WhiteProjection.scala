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

class WhiteProjection[+B,+W] private[scalactic] (val underlying: B Otherwise W) extends AnyVal with Serializable {

  /**
   * Indicates whether this <code>Otherwise</code> is a <code>Good</code>
   *
   * @return true if this <code>Otherwise</code> is a <code>Good</code>, <code>false</code> if it is a <code>White</code>.
   */
  def isBlack: Boolean = underlying.isBlack

  /**
   * Indicates whether this <code>Otherwise</code> is a <code>White</code>
   *
   * @return true if this <code>Otherwise</code> is a <code>White</code>, <code>false</code> if it is a <code>Black</code>.
   */
  def isWhite: Boolean = underlying.isWhite

  /**
   * Maps the given function to this <code>Otherwise</code>'s value if it is a <code>Black</code> or returns <code>this</code> if it is a <code>White</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>Black</code>, the result of applying the given function to the contained value wrapped in a <code>Black</code>,
   *         else this <code>White</code> is returned
   */
  def map[X](f: W => X): WhiteProjection[B, X] =
    underlying match {
      case White(w) => new WhiteProjection(White(f(w)))
      case b: Black[B] => new WhiteProjection(b)
    }

  /**
   * Maps the given function to this <code>Otherwise</code>'s value if it is a <code>White</code>, transforming it into a <code>Black</code>, or returns
   * <code>this</code> if it is already a <code>Black</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>White</code>, the result of applying the given function to the contained value wrapped in a <code>Black</code>,
   *         else this <code>Black</code> is returned
   */
  def recover[X >: W](f: B => X): WhiteProjection[B, X] =
    underlying match {
      case Black(b) => new WhiteProjection(White(f(b)))
      case w: White[W] => new WhiteProjection(w)
    }

  /**
   * Maps the given function to this <code>WhiteProjection</code>'s value if the underlying is a <code>Black</code>, returning the result, or returns
   * <code>this</code> if the underlying is already a <code>White</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>Black</code>, the result of applying the given function to the contained value,
   *         else this <code>White</code> is returned
   */
  def recoverWith[C, X >: W](f: B => WhiteProjection[C, X]): WhiteProjection[C, X] =
    underlying match {
      case Black(b) => f(b)
      case w: White[W] => new WhiteProjection(w) // It looks inefficient to an old C programmer, but it doesn't box because AnyVal
    }

  /**
   * Applies the given function f to the contained value if this <code>Otherwise</code> is a <code>Black</code>; does nothing if this <code>Otherwise</code>
   * is a <code>White</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: W => Unit): Unit =
    underlying match {
      case White(w) => f(w)
      case _ => ()
    }

  /**
   * Returns the given function applied to the value contained in this <code>Otherwise</code> if it is a <code>Black</code>,
   * or returns <code>this</code> if it is a <code>White</code>.
   *
   * @param f the function to apply
   * @return if this is a <code>Black</code>, the result of applying the given function to the contained value wrapped in a <code>Black</code>,
   *         else this <code>White</code> is returned
   */
  def flatMap[C >: B, X](f: W => WhiteProjection[C, X]): WhiteProjection[C, X] =
    underlying match {
      case White(w) => f(w)
      case b: Black[B] => new WhiteProjection(b)
    }

  /**
   * Returns this <code>Otherwise</code> if either 1) it is a <code>White</code> or 2) it is a <code>Black</code> and applying the validation function <code>f</code> to this
   * <code>Black</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>White</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>Black</code>'s value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return a <code>Black</code> if this <code>Otherwise</code> is a <code>Black</code> that passes the validation function, else a <code>White</code>.
   */
  def filter[C >: B](f: W => Validation[C]): WhiteProjection[C, W] =
    underlying match {
      case White(w) =>
        f(w) match {
          case Pass => this
          case Fail(c) => new WhiteProjection(Black(c))
        }
      case b: Black[B] => new WhiteProjection(b)
    }

  // TODO: What should we do about withFilter. Black question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[C >: B](f: W => Validation[C]): WhiteProjection[C, W] = filter(f)

  /**
   * Returns <code>true</code> if this <code>Otherwise</code> is a <code>Black</code> and the predicate <code>p</code> returns true when applied to this <code>Black</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if this <code>Otherwise</code> is a <code>Black</code>, but the opposite
   * result if this <code>Otherwise</code> is a <code>White</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Black</code> value, if this is a <code>Black</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Black</code> value, if this is a <code>Black</code>, else <code>false</code>
   */
  def exists(p: W => Boolean): Boolean = ???

  /**
   * Returns <code>true</code> if either this <code>Otherwise</code> is a <code>White</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to this <code>Black</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if this <code>Otherwise</code> is a <code>Black</code>, but the opposite
   * result if this <code>Otherwise</code> is a <code>White</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Black</code> value, if this is a <code>Black</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Black</code> value, if this is a <code>Black</code>, else <code>true</code>
   */
  def forall(f: W => Boolean): Boolean = ???

  /**
   * Returns, if this <code>Otherwise</code> is <code>Black</code>, this <code>Black</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if this <code>Otherwise</code> is a <code>White</code>
   * @return the contained value, if this <code>Otherwise</code> is a <code>Black</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrOtherwise[X >: W](default: => X): X = ???

  /**
   * Returns this <code>Otherwise</code> if it is a <code>Black</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if this <code>Otherwise</code> is a <code>White</code>
   * @return this <code>Otherwise</code>, if it is a <code>Black</code>, else the result of evaluating <code>alternative</code>
   */
  def orOtherwise[C >: B, X >: W](alternative: => C Otherwise X): C Otherwise X = ???

  /**
   * Returns a <code>Some</code> containing the <code>Black</code> value, if this <code>Otherwise</code> is a <code>Black</code>, else <code>None</code>.
   *
   * @return the contained &ldquo;good&rdquo; value wrapped in a <code>Some</code>, if this <code>Otherwise</code> is a <code>Black</code>; <code>None</code>
   *     if this <code>Otherwise</code> is a <code>White</code>.
   */
  def toOption: Option[W] = ???

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>Black</code> value, if this <code>Otherwise</code> is a <code>Black</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained &ldquo;good&rdquo; value in a lone-element <code>Seq</code> if this <code>Otherwise</code> is a <code>Black</code>; an empty <code>Seq</code> if
   *     this <code>Otherwise</code> is a <code>White</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[W] = ???

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>Black</code> value, if this is a <code>Black</code>; a <code>Left</code>
   * containing the <code>White</code> value, if this is a <code>White</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when convering an <code>Otherwise</code> to an <code>Either</code>. If the type of the
   * <code>Otherwise</code> on which you invoke <code>toEither</code> is <code>Otherwise[Int, ErrorMessage]</code> for example, the result will be an
   * <code>Either[ErrorMessage, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;bad&rdquo;
   * values and <code>Right</code> is used for &ldquo;good&rdquo; ones.
   * </p>
   *
   * @return this <code>Black</code> value, wrapped in a <code>Right</code>, or this <code>White</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[B, W] = ???

  /**
   * Converts this <code>Otherwise</code> to an <code>Otherwise</code> with the same <code>Black</code> type and a <code>White</code> type consisting of
   * <a href="One.html"><code>One</code></a> parameterized by this <code>Otherwise</code>'s <code>White</code> type.
   *
   * <p>
   * For example, invoking the <code>accumulating</code> method on an <code>Int Otherwise ErrorMessage</code> would convert it to an
   * <code>Int Otherwise One[ErrorMessage]</code>. This result type, because the <code>White</code> type is an <code>Every</code>, can be used
   * with the mechanisms provided in trait <a href="Accumulation.html"><code>Accumulation</code></a> to accumulate errors.
   * <p>
   *
   * <p>
   * Note that if this <code>Otherwise</code> is already an accumulating <code>Otherwise</code>, the behavior of this <code>accumulating</code> method does not change.
   * For example, if you invoke <code>accumulating</code> on an <code>Int Otherwise One[ErrorMessage]</code> you will be rewarded with an
   * <code>Int Otherwise One[One[ErrorMessage]]</code>.
   * </p>
   *
   * @return this <code>Black</code>, if this <code>Otherwise</code> is a <code>Black</code>; or this <code>White</code> value wrapped in a <code>One</code> if
   *     this <code>Otherwise</code> is a <code>White</code>.
   */
  def toOr: W Or B = ???

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>Black</code> value, if this is a <code>Black</code>; a <code>Failure</code>
   * containing the <code>White</code> value, if this is a <code>White</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>White</code> type of this <code>Otherwise</code> is a subclass
   * of <code>Throwable</code> (or <code>Throwable</code> itself).
   * </p>
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>Otherwise</code> to an <code>Either</code>. If the type of the
   * <code>Otherwise</code> on which you invoke <code>toEither</code> is <code>Otherwise[Int, ErrorMessage]</code> for example, the result will be an
   * <code>Either[ErrorMessage, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;bad&rdquo;
   * values and <code>Right</code> is used for &ldquo;good&rdquo; ones.
   * </p>
   *
   * @return this <code>Black</code> value, wrapped in a <code>Right</code>, or this <code>White</code> value, wrapped in a <code>Left</code>.
   */
  def toTry(implicit ev: B <:< Throwable): Try[W] = ???

  /**
   * Returns an <code>Otherwise</code> with the <code>Black</code> and <code>White</code> types swapped: <code>White</code> becomes <code>Black</code> and <code>Black</code>
   * becomes <code>White</code>.
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; val lyrics = White("Hey Jude, don't make it bad. Take a sad song and make it better.")
   * lyrics: org.scalactic.White[Nothing,String] =
   *     White(Hey Jude, don't make it bad. Take a sad song and make it better.)
   *
   * scala&gt; lyrics.swap
   * res12: org.scalactic.Otherwise[String,Nothing] =
   *     Black(Hey Jude, don't make it bad. Take a sad song and make it better.)
   * </pre>
   *
   * <p>
   * Now that song will be rolling around in your head all afternoon. But at least it is a good song (thanks to <code>swap</code>).
   * </p>
   *
   * @return if this <code>Otherwise</code> is a <code>Black</code>, its <code>Black</code> value wrapped in a <code>White</code>; if this <code>Otherwise</code> is
   *     a <code>White</code>, its <code>White</code> value wrapped in a <code>Black</code>.
   */
  def swap: WhiteProjection[W, B] = ???

  /**
   * Transforms this <code>Otherwise</code> by applying the function <code>gf</code> to this <code>Otherwise</code>'s <code>Black</code> value if it is a <code>Black</code>,
   * or by applying <code>bf</code> to this <code>Otherwise</code>'s <code>White</code> value if it is a <code>White</code>.
   *
   * @param gf the function to apply to this <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param bf the function to apply to this <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>gf</code> or </code>bf</code>, to this <code>Otherwise</code>'s value
   */
  def transform[C, X](bf: B => WhiteProjection[C, X], wf: W => WhiteProjection[C, X]): WhiteProjection[C, X] = ???

  /**
   * Folds this <code>Otherwise</code> into a value of type <code>V</code> by applying the given <code>gf</code> function if this is
   * a <code>Black</code> else the given <code>bf</code> function if this is a <code>White</code>.
   *
   * @param gf the function to apply to this <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param bf the function to apply to this <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>gf</code> or </code>bf</code>, to this <code>Otherwise</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V = ???

  override def toString = s"WhiteProjection($underlying)"
}
