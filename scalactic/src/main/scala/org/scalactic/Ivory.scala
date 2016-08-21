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
 * @param value the underlying <code>Otherwise</code> value wrapped in this <code>Ivory</code>.
 */
class Ivory[+B,+W] private[scalactic] (val value: B Otherwise W) extends AnyVal with Serializable { thisIvory =>

  /**
   * Indicates whether the <code>Otherwise</code> underlying this <code>Ivory</code> is a <code>Black</code>
   *
   * @return true if the underlying <code>Otherwise</code> is a <code>Black</code>, <code>false</code> if it is a <code>White</code>.
   */
  def isBlack: Boolean = thisIvory.value.isBlack

  /**
   * Indicates whether the <code>Otherwise</code> underlying this <code>Ivory</code> is a <code>White</code>
   *
   * @return true if the underlying <code>Otherwise</code> is a <code>White</code>, <code>false</code> if it is a <code>Black</code>.
   */
  def isWhite: Boolean = thisIvory.value.isWhite

  /**
   * Applies the given function to the value contained in the underlying <code>Otherwise</code> if it is a <code>White</code>, and 
   * returns a new <code>Ivory</code> wrapping a new <code>White</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Otherwise</code> is a <code>Black</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>White</code>, the result of applying the given function to the contained value wrapped in a <code>White</code> wrapped in an <code>Ivory</code>,
   *         else this <code>Ivory</code> (already containing a <code>Black</code>)
   */
  def map[X](f: W => X): Ivory[B, X] =
    thisIvory.value match {
      case White(w) => new Ivory(White(f(w)))
      case b: Black[B] => new Ivory(b) // My dox says "this", but i'm doing new, but it is an AnyVal so it shouldn't box. 
    }                                  // For an AnyVal, "this" can kind of refer to the underlying seems like, and I am returning that.

  /**
   * Applies the given function to the value in the <code>Otherwise</code> underlying this <code>Ivory</code> if the underlying
   * <code>Otherwise</code> is a <code>Black</code>, returning an <code>Ivory</code> wrapping a <code>White</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Otherwise</code> is already a <code>White</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>White</code> wrapped in an <code>Ivory</code>,
   *         else this <code>Ivory</code> (already containing a <code>White</code>)
   */
  def recover[X >: W](f: B => X): Ivory[B, X] =
    thisIvory.value match {
      case Black(b) => new Ivory(White(f(b)))
      case w: White[W] => new Ivory(w)
    }

  /**
   * Applies the given function to the value in the <code>Otherwise</code> underlying this <code>Ivory</code>'s value if the
   * underlying <code>Otherwise</code> is a <code>Black</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Otherwise</code> is already a <code>White</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, the result of applying the given function to the
   *         contained value, else this <code>Ivory</code> (already containing a <code>White</code>)
   */
  def recoverWith[C, X >: W](f: B => Ivory[C, X]): Ivory[C, X] =
    thisIvory.value match {
      case Black(b) => f(b)
      case w: White[W] => new Ivory(w) // It looks inefficient to an old C programmer, but it doesn't box because AnyVal
    }

  /**
   * Applies the given function f to the contained value if the <code>Otherwise</code> underlying this <code>Ivory</code> is a <code>White</code>; does nothing if the underlying <code>Otherwise</code>
   * is a <code>Black</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: W => Unit): Unit =
    thisIvory.value match {
      case White(w) => f(w)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Otherwise</code> underlying this <code>Ivory</code> if it is a <code>White</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Otherwise</code> is a <code>Black</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Otherwise</code> is a <code>White</code>, the result of applying the given function to the value contained in the
   *         underlying <code>White</code>,
   *         else this <code>Ivory</code> (already containing a <code>Black</code>)
   */
  def flatMap[C >: B, X](f: W => Ivory[C, X]): Ivory[C, X] =
    thisIvory.value match {
      case White(w) => f(w)
      case b: Black[B] => new Ivory(b)
    }

  /**
   * Returns this <code>Ivory</code> if either 1) the underlying <code>Otherwise</code> is a <code>Black</code> or 2) it is a <code>White</code> and applying the validation function <code>f</code> to the
   * <code>White</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>Ivory</code> wrapping a <code>Black</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>Ivory</code>'s underlying <code>White</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return an <code>Ivory</code> wrapping a <code>White</code> if the underlying <code>Otherwise</code> is a <code>White</code> that passes the validation function, else an <code>Ivory</code> wrapping a <code>Black</code>.
   */
  def filter[C >: B](f: W => Validation[C]): Ivory[C, W] =
    thisIvory.value match {
      case White(w) =>
        f(w) match {
          case Pass => thisIvory
          case Fail(c) => new Ivory(Black(c))
        }
       case _ => thisIvory
    }

  // TODO: What should we do about withFilter. Black question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[C >: B](f: W => Validation[C]): Ivory[C, W] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Otherwise</code> underlying this <code>Ivory</code> is a <code>White</code> and the predicate <code>p</code> returns true when applied to the underlying <code>White</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Otherwise</code> is a <code>White</code>, but the opposite
   * result if the underlying <code>Otherwise</code> is a <code>Black</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>White</code> value, if the underlying <code>Otherwise</code> is a <code>White</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>White</code> value, if this is a <code>White</code>, else <code>false</code>
   */
  def exists(p: W => Boolean): Boolean =
    thisIvory.value match {
      case White(w) => p(w)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Otherwise</code> underlying this <code>Ivory</code> is a <code>White</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>White</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Otherwise</code> is a <code>White</code>, but the opposite
   * result if the underlying <code>Otherwise</code> is a <code>Black</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Black</code> value, if the underlying <code>Otherwise</code> is a <code>Black</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>White</code> value, if this is a <code>White</code>, else <code>true</code>
   */
  def forall(p: W => Boolean): Boolean =
    thisIvory.value match {
      case White(w) => p(w)
      case _ => true
    }

  /**
   * Returns, if the <code>Otherwise</code> underlying this <code>Ivory</code> is <code>White</code>, the <code>White</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Otherwise</code> is a <code>Black</code>
   * @return the contained value, if the underlying <code>Otherwise</code> is a <code>White</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[X >: W](default: => X): X =
    thisIvory.value match {
      case White(w) => w
      case _ => default
    }

  /**
   * Returns this <code>Ivory</code> if the underlying <code>Otherwise</code> is a <code>White</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Otherwise</code> is a <code>Black</code>
   * @return this <code>Ivory</code>, if the underlying <code>Otherwise</code> is a <code>White</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[C >: B, X >: W](alternative: => Ivory[C, X]): Ivory[C, X] =
    if (isWhite) thisIvory else alternative

  /**
   * Returns a <code>Some</code> containing the <code>White</code> value, if the <code>Otherwise</code> underlying this <code>Ivory</code>
   * is a <code>White</code>, else <code>None</code>.
   *
   * @return the contained <code>White</code> value wrapped in a <code>Some</code>, if the <code>Otherwise</code> underlying this <code>Ivory</code>
   * is a <code>White</code>; <code>None</code> if the underlying <code>Otherwise</code> is a <code>Black</code>.
   */
  def toOption: Option[W] =
    thisIvory.value match {
      case White(w) => Some(w)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>White</code> value, if the <code>Otherwise</code> underlying this <code>Ivory</code> is a <code>White</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>White</code> value in a lone-element <code>Seq</code> if the underlying <code>Otherwise</code> is a <code>White</code>; an empty <code>Seq</code> if
   *     the underlying <code>Otherwise</code> is a <code>Black</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[W] =
    thisIvory.value match {
      case White(w) => Vector(w)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>White</code> value, if this is a <code>White</code>; or a <code>Left</code>
   * containing the <code>Black</code> value, if this is a <code>Black</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting an <code>Ivory</code> to an <code>Either</code>. If the type of the
   * <code>Ivory</code> on which you invoke <code>toEither</code> is <code>Ivory[Double, Int]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Otherwise</code> is a <code>White</code>, the <code>White</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>Black</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[B, W] =
    thisIvory.value match {
      case White(w) => Right(w)
      case Black(b) => Left(b)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>White</code> value, if this is a <code>White</code>; or a <code>Bad</code>
   * containing the <code>Black</code> value, if this is a <code>Black</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>Ivory</code> to an <code>Or</code>. If the type of the
   * <code>Ivory</code> on which you invoke <code>toOr</code> is <code>Ivory[Int, Double]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   * 
   * @return if the underlying <code>Otherwise</code> is a <code>White</code>, the <code>White</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>Black</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: W Or B =
    thisIvory.value match {
      case White(w) => Good(w)
      case Black(b) => Bad(b)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>White</code> value, if this is a <code>White</code>; or a <code>Failure</code>
   * containing the <code>Black</code> value, if this is a <code>Black</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>Black</code> type of this <code>Ivory</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>White</code> value, wrapped in a <code>Success</code>, if the underlying <code>Otherwise</code> is a <code>White</code>; else
   * the underlying <code>Black</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: B <:< Throwable): Try[W] =
    thisIvory.value match {
      case White(w) => Success(w)
      case Black(b) => Failure(b)
    }

  /**
   * Returns an <code>Ivory</code> with the <code>Black</code> and <code>White</code> types swapped: <code>White</code> becomes <code>Black</code> and <code>Black</code>
   * becomes <code>White</code>.
   *
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, its <code>Black</code> value wrapped in a <code>White</code> then wrapped in
   * an <code>Ivory</code>; if the underlying <code>Otherwise</code> is
   *     a <code>White</code>, its <code>White</code> value wrapped in a <code>Black</code> then wrapped in an <code>Ivory</code>.
   */
  def swap: Ivory[W, B] =
    thisIvory.value match {
      case White(w) => new Ivory(Black(w))
      case Black(b) => new Ivory(White(b))
    }

  /**
   * Transforms this <code>Ivory</code> by applying the function <code>bf</code> to the underlying <code>Otherwise</code>'s <code>Black</code> value if it is a <code>Black</code>,
   * or by applying <code>wf</code> to the underlying <code>Otherwise</code>'s <code>White</code> value if it is a <code>White</code>.
   *
   * @param bf the function to apply to the <code>Ivory</code>'s underlying <code>Black</code> value, if it is a <code>Black</code>
   * @param wf the function to apply to the <code>Ivory</code>'s underlying <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Otherwise</code>'s value
   */
  def transform[C, X](bf: B => Ivory[C, X], wf: W => Ivory[C, X]): Ivory[C, X] =
    thisIvory.value match {
      case White(w) => wf(w)
      case Black(b) => bf(b)
    }

  /**
   * Folds this <code>Ivory</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>Black</code> else the given <code>wf</code> function if this is a <code>White</code>.
   *
   * @param bf the function to apply to the underlying <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param wf the function to apply to the underlying <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Otherwise</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V =
    thisIvory.value match {
      case White(w) => wf(w)
      case Black(b) => bf(b)
    }

  /**
   * A string representation for this <code>Ivory</code>.
   */
  override def toString = s"Ivory($thisIvory.value)"
}
