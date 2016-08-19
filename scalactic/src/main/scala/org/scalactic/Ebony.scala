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

class Ebony[+B,+W] private[scalactic] (val otherwise: B Otherwise W) extends AnyVal with Serializable { thisEbony =>

  /**
   * Indicates whether the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>Black</code>
   *
   * @return true if the underlying <code>Otherwise</code> is a <code>Black</code>, <code>false</code> if it is a <code>White</code>.
   */
  def isBlack: Boolean = otherwise.isBlack

  /**
   * Indicates whether the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>White</code>
   *
   * @return true if the underlying <code>Otherwise</code> is a <code>White</code>, <code>false</code> if it is a <code>Black</code>.
   */
  def isWhite: Boolean = otherwise.isWhite

  /**
   * Applies the given function to the value contained in the underlying <code>Otherwise</code> if it is a <code>Black</code>, and 
   * returns a new <code>Ebony</code> wrapping a new <code>Black</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Otherwise</code> is a <code>White</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, the result of applying the given function to the contained
   *         value wrapped in a <code>Black</code> wrapped in an <code>Ebony</code>,
   *         else this <code>Ebony</code> (already containing a <code>White</code>)
   */
  def map[C](f: B => C): Ebony[C, W] =
    otherwise match {
      case Black(b) => new Ebony(Black(f(b)))
      case w: White[W] => new Ebony(w)
    }

  /**
   * Applies the given function to the value in the <code>Otherwise</code> underlying this <code>Ebony</code> if the underlying
   * <code>Otherwise</code> is a <code>White</code>, returning an <code>Ebony</code> wrapping a <code>Black</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Otherwise</code> is already a <code>Black</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>White</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>Black</code> wrapped in an <code>Ebony</code>,
   *         else this <code>Ebony</code> (already containing a <code>Black</code>)
   */
  def recover[C >: B](f: W => C): Ebony[C, W] =
    otherwise match {
      case White(w) => new Ebony(Black(f(w)))
      case b: Black[B] => new Ebony(b)
    }

  /**
   * Applies the given function to the value in the <code>Otherwise</code> underlying this <code>Ebony</code>'s value if the
   * underlying <code>Otherwise</code> is a <code>White</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Otherwise</code> is a <code>Black</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>White</code>, the result of applying the given function to the
   *         contained value, else this <code>Ebony</code> (already containing a <code>Black</code>)
   */
  def recoverWith[C >: B, X](f: W => Ebony[C, X]): Ebony[C, X] =
    otherwise match {
      case White(w) => f(w)
      case b: Black[B] => new Ebony(b)
    }

  /**
   * Applies the given function f to the contained value if the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>Black</code>; does nothing if the underlying <code>Otherwise</code>
   * is a <code>White</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: B => Unit): Unit =
    otherwise match {
      case Black(b) => f(b)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Otherwise</code> underlying this <code>Ebony</code> if it is a <code>Black</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Otherwise</code> is a <code>White</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, the result of applying the given function to the value contained in the
   *         underlying <code>Black</code>,
   *         else this <code>Ebony</code> (already containing a <code>White</code>)
   */
  def flatMap[C, X >: W](f: B => Ebony[C, X]): Ebony[C, X] =
    otherwise match {
      case Black(b) => f(b)
      case w: White[W] => new Ebony(w)
    }

  /**
   * Returns this <code>Ebony</code> if either 1) the underlying <code>Otherwise</code> is a <code>White</code> or 2) it is a <code>Black</code> and applying the validation function <code>f</code> to the
   * <code>Black</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>Ebony</code> wrapping a <code>White</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>Ebony</code>'s underlying <code>Black</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return an <code>Ebony</code> wrapping a <code>Black</code> if the underlying <code>Otherwise</code> is a <code>Black</code> that passes the validation function, else an <code>Ebony</code> wrapping a <code>White</code>.
   */
  def filter[X >: W](f: B => Validation[X]): Ebony[B, X] =
    otherwise match {
      case Black(b) =>
        f(b) match {
          case Pass => thisEbony
          case Fail(x) => new Ebony(White(x))
        }
      case _ => thisEbony
    }

  // TODO: What should we do about withFilter. Black question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[X >: W](f: B => Validation[X]): Ebony[B, X] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>Black</code> and the predicate <code>p</code> returns true when applied to the underlying <code>Black</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Otherwise</code> is a <code>Black</code>, but the opposite
   * result if the underlying <code>Otherwise</code> is a <code>White</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Black</code> value, if the underlying <code>Otherwise</code> is a <code>Black</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Black</code> value, if this is a <code>Black</code>, else <code>false</code>
   */
  def exists(p: B => Boolean): Boolean =
    otherwise match {
      case Black(b) => p(b)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>White</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>Black</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Otherwise</code> is a <code>Black</code>, but the opposite
   * result if the underlying <code>Otherwise</code> is a <code>White</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Black</code> value, if the underlying <code>Otherwise</code> is a <code>Black</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Black</code> value, if this is a <code>Black</code>, else <code>true</code>
   */
  def forall(p: B => Boolean): Boolean =
    otherwise match {
      case Black(b) => p(b)
      case _ => true
    }

  /**
   * Returns, if the <code>Otherwise</code> underlying this </code>Ebony</code> is <code>Black</code>, this <code>Black</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Otherwise</code> is a <code>White</code>
   * @return the contained value, if the underlying <code>Otherwise</code> is a <code>Black</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[C >: B](default: => C): C =
    otherwise match {
      case Black(b) => b
      case _ => default
    }

  /**
   * Returns this </code>Ebony</code> if the underlying <code>Otherwise</code> is a <code>Black</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Otherwise</code> is a <code>White</code>
   * @return this <code>Ebony</code>, if the underlying <code>Otherwise</code> is a <code>Black</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[C >: B, X >: W](alternative: => Ebony[C, X]): Ebony[C, X] =
    if (isBlack) thisEbony else alternative

  /**
   * Returns a <code>Some</code> containing the <code>Black</code> value, if the <code>Otherwise</code> underlying this <code>Ebony</code>
   * is a <code>Black</code>, else <code>None</code>.
   *
   * @return the contained &ldquo;black&rdquo; value wrapped in a <code>Some</code>, if the underlying <code>Otherwise</code> is a <code>Black</code>;
   * <code>None</code> if the underlying <code>Otherwise</code> is a <code>White</code>.
   */
  def toOption: Option[B] =
    otherwise match {
      case Black(b) => Some(b)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>Black</code> value, if the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>Black</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained &ldquo;good&rdquo; value in a lone-element <code>Seq</code> if the underlying <code>Otherwise</code> is a <code>Black</code>; an empty <code>Seq</code> if
   *     the underlying <code>Otherwise</code> is a <code>White</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[B] = ???

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
  def toEither: Either[W, B] = ???

  /**
   * Converts this <code>Ebony</code> to an <code>Or</code> with the same <code>Black</code> type and a <code>White</code> type consisting of
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
  def toOr: B Or W = ???

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
  def toTry(implicit ev: W <:< Throwable): Try[B] = ???

  /**
   * Returns a <code>Ebony</code> with the <code>Black</code> and <code>White</code> types swapped: <code>White</code> becomes <code>Black</code> and <code>Black</code>
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
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, its <code>Black</code> value wrapped in a <code>White</code>; if the underlying <code>Otherwise</code> is
   *     a <code>White</code>, its <code>White</code> value wrapped in a <code>Black</code>.
   */
  def swap: Ebony[W, B] = ???

  /**
   * Transforms this <code>Otherwise</code> by applying the function <code>gf</code> to the underlying <code>Otherwise</code>'s <code>Black</code> value if it is a <code>Black</code>,
   * or by applying <code>bf</code> to the underlying <code>Otherwise</code>'s <code>White</code> value if it is a <code>White</code>.
   *
   * @param gf the function to apply to the underlying <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param bf the function to apply to the underlying <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>gf</code> or </code>bf</code>, to the underlying <code>Otherwise</code>'s value
   */
  def transform[C, X](bf: B => Ebony[C, X], wf: W => Ebony[C, X]): Ebony[C, X] = ???

  /**
   * Folds this <code>Otherwise</code> into a value of type <code>V</code> by applying the given <code>gf</code> function if this is
   * a <code>Black</code> else the given <code>bf</code> function if this is a <code>White</code>.
   *
   * @param gf the function to apply to the underlying <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param bf the function to apply to the underlying <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>gf</code> or </code>bf</code>, to the underlying <code>Otherwise</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V = ???

  override def toString = s"Ebony($otherwise)"
}

