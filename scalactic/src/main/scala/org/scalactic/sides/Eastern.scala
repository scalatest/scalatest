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
package org.scalactic.sides

import org.scalactic.{Good, Bad, Or, Validation, Pass, Fail}
import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

/**
 * @param value the underlying <code>Side</code> value wrapped in this <code>Eastern</code>.
 */
class Eastern[+B,+W] private[scalactic] (val value: B Side W) extends AnyVal with Serializable { thisEastern =>

  /**
   * Indicates whether the <code>Side</code> underlying this <code>Eastern</code> is a <code>West</code>
   *
   * @return true if the underlying <code>Side</code> is a <code>West</code>, <code>false</code> if it is a <code>East</code>.
   */
  def isWest: Boolean = thisEastern.value.isWest

  /**
   * Indicates whether the <code>Side</code> underlying this <code>Eastern</code> is a <code>East</code>
   *
   * @return true if the underlying <code>Side</code> is a <code>East</code>, <code>false</code> if it is a <code>West</code>.
   */
  def isEast: Boolean = thisEastern.value.isEast

  /**
   * Applies the given function to the value contained in the underlying <code>Side</code> if it is a <code>East</code>, and 
   * returns a new <code>Eastern</code> wrapping a new <code>East</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Side</code> is a <code>West</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>East</code>, the result of applying the given function to the contained value wrapped in a <code>East</code> wrapped in an <code>Eastern</code>,
   *         else this <code>Eastern</code> (already containing a <code>West</code>)
   */
  def map[X](f: W => X): Eastern[B, X] =
    thisEastern.value match {
      case East(w) => new Eastern(East(f(w)))
      case b: West[B] => new Eastern(b) // My dox says "this", but i'm doing new, but it is an AnyVal so it shouldn't box. 
    }                                  // For an AnyVal, "this" can kind of refer to the underlying seems like, and I am returning that.

  /**
   * Applies the given function to the value in the <code>Side</code> underlying this <code>Eastern</code> if the underlying
   * <code>Side</code> is a <code>West</code>, returning an <code>Eastern</code> wrapping a <code>East</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Side</code> is already a <code>East</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>West</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>East</code> wrapped in an <code>Eastern</code>,
   *         else this <code>Eastern</code> (already containing a <code>East</code>)
   */
  def recover[X >: W](f: B => X): Eastern[B, X] =
    thisEastern.value match {
      case West(b) => new Eastern(East(f(b)))
      case w: East[W] => new Eastern(w)
    }

  /**
   * Applies the given function to the value in the <code>Side</code> underlying this <code>Eastern</code>'s value if the
   * underlying <code>Side</code> is a <code>West</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Side</code> is already a <code>East</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>West</code>, the result of applying the given function to the
   *         contained value, else this <code>Eastern</code> (already containing a <code>East</code>)
   */
  def recoverWith[C, X >: W](f: B => Eastern[C, X]): Eastern[C, X] =
    thisEastern.value match {
      case West(b) => f(b)
      case w: East[W] => new Eastern(w) // It looks inefficient to an old C programmer, but it doesn't box because AnyVal
    }

  /**
   * Applies the given function f to the contained value if the <code>Side</code> underlying this <code>Eastern</code> is a <code>East</code>; does nothing if the underlying <code>Side</code>
   * is a <code>West</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: W => Unit): Unit =
    thisEastern.value match {
      case East(w) => f(w)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Side</code> underlying this <code>Eastern</code> if it is a <code>East</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Side</code> is a <code>West</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>East</code>, the result of applying the given function to the value contained in the
   *         underlying <code>East</code>,
   *         else this <code>Eastern</code> (already containing a <code>West</code>)
   */
  def flatMap[C >: B, X](f: W => Eastern[C, X]): Eastern[C, X] =
    thisEastern.value match {
      case East(w) => f(w)
      case b: West[B] => new Eastern(b)
    }

  /**
   * Returns this <code>Eastern</code> if either 1) the underlying <code>Side</code> is a <code>West</code> or 2) it is a <code>East</code> and applying the validation function <code>f</code> to the
   * <code>East</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>Eastern</code> wrapping a <code>West</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>Eastern</code>'s underlying <code>East</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return an <code>Eastern</code> wrapping a <code>East</code> if the underlying <code>Side</code> is a <code>East</code> that passes the validation function, else an <code>Eastern</code> wrapping a <code>West</code>.
   */
  def filter[C >: B](f: W => Validation[C]): Eastern[C, W] =
    thisEastern.value match {
      case East(w) =>
        f(w) match {
          case Pass => thisEastern
          case Fail(c) => new Eastern(West(c))
        }
       case _ => thisEastern
    }

  // TODO: What should we do about withFilter. West question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[C >: B](f: W => Validation[C]): Eastern[C, W] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Side</code> underlying this <code>Eastern</code> is a <code>East</code> and the predicate <code>p</code> returns true when applied to the underlying <code>East</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Side</code> is a <code>East</code>, but the opposite
   * result if the underlying <code>Side</code> is a <code>West</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>East</code> value, if the underlying <code>Side</code> is a <code>East</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>East</code> value, if this is a <code>East</code>, else <code>false</code>
   */
  def exists(p: W => Boolean): Boolean =
    thisEastern.value match {
      case East(w) => p(w)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Side</code> underlying this <code>Eastern</code> is a <code>East</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>East</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Side</code> is a <code>East</code>, but the opposite
   * result if the underlying <code>Side</code> is a <code>West</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>West</code> value, if the underlying <code>Side</code> is a <code>West</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>East</code> value, if this is a <code>East</code>, else <code>true</code>
   */
  def forall(p: W => Boolean): Boolean =
    thisEastern.value match {
      case East(w) => p(w)
      case _ => true
    }

  /**
   * Returns, if the <code>Side</code> underlying this <code>Eastern</code> is <code>East</code>, the <code>East</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Side</code> is a <code>West</code>
   * @return the contained value, if the underlying <code>Side</code> is a <code>East</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[X >: W](default: => X): X =
    thisEastern.value match {
      case East(w) => w
      case _ => default
    }

  /**
   * Returns this <code>Eastern</code> if the underlying <code>Side</code> is a <code>East</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Side</code> is a <code>West</code>
   * @return this <code>Eastern</code>, if the underlying <code>Side</code> is a <code>East</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[C >: B, X >: W](alternative: => Eastern[C, X]): Eastern[C, X] =
    if (isEast) thisEastern else alternative

  /**
   * Returns a <code>Some</code> containing the <code>East</code> value, if the <code>Side</code> underlying this <code>Eastern</code>
   * is a <code>East</code>, else <code>None</code>.
   *
   * @return the contained <code>East</code> value wrapped in a <code>Some</code>, if the <code>Side</code> underlying this <code>Eastern</code>
   * is a <code>East</code>; <code>None</code> if the underlying <code>Side</code> is a <code>West</code>.
   */
  def toOption: Option[W] =
    thisEastern.value match {
      case East(w) => Some(w)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>East</code> value, if the <code>Side</code> underlying this <code>Eastern</code> is a <code>East</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>East</code> value in a lone-element <code>Seq</code> if the underlying <code>Side</code> is a <code>East</code>; an empty <code>Seq</code> if
   *     the underlying <code>Side</code> is a <code>West</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[W] =
    thisEastern.value match {
      case East(w) => Vector(w)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>East</code> value, if this is a <code>East</code>; or a <code>Left</code>
   * containing the <code>West</code> value, if this is a <code>West</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting an <code>Eastern</code> to an <code>Either</code>. If the type of the
   * <code>Eastern</code> on which you invoke <code>toEither</code> is <code>Eastern[Double, Int]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Side</code> is a <code>East</code>, the <code>East</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>West</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[B, W] =
    thisEastern.value match {
      case East(w) => Right(w)
      case West(b) => Left(b)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>East</code> value, if this is a <code>East</code>; or a <code>Bad</code>
   * containing the <code>West</code> value, if this is a <code>West</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>Eastern</code> to an <code>Or</code>. If the type of the
   * <code>Eastern</code> on which you invoke <code>toOr</code> is <code>Eastern[Int, Double]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   * 
   * @return if the underlying <code>Side</code> is a <code>East</code>, the <code>East</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>West</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: W Or B =
    thisEastern.value match {
      case East(w) => Good(w)
      case West(b) => Bad(b)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>East</code> value, if this is a <code>East</code>; or a <code>Failure</code>
   * containing the <code>West</code> value, if this is a <code>West</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>West</code> type of this <code>Eastern</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>East</code> value, wrapped in a <code>Success</code>, if the underlying <code>Side</code> is a <code>East</code>; else
   * the underlying <code>West</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: B <:< Throwable): Try[W] =
    thisEastern.value match {
      case East(w) => Success(w)
      case West(b) => Failure(b)
    }

  /**
   * Returns an <code>Eastern</code> with the <code>West</code> and <code>East</code> types swapped: <code>East</code> becomes <code>West</code> and <code>West</code>
   * becomes <code>East</code>.
   *
   * @return if the underlying <code>Side</code> is a <code>West</code>, its <code>West</code> value wrapped in a <code>East</code> then wrapped in
   * an <code>Eastern</code>; if the underlying <code>Side</code> is
   *     a <code>East</code>, its <code>East</code> value wrapped in a <code>West</code> then wrapped in an <code>Eastern</code>.
   */
  def swap: Eastern[W, B] =
    thisEastern.value match {
      case East(w) => new Eastern(West(w))
      case West(b) => new Eastern(East(b))
    }

  /**
   * Transforms this <code>Eastern</code> by applying the function <code>bf</code> to the underlying <code>Side</code>'s <code>West</code> value if it is a <code>West</code>,
   * or by applying <code>wf</code> to the underlying <code>Side</code>'s <code>East</code> value if it is a <code>East</code>.
   *
   * @param bf the function to apply to the <code>Eastern</code>'s underlying <code>West</code> value, if it is a <code>West</code>
   * @param wf the function to apply to the <code>Eastern</code>'s underlying <code>East</code> value, if it is a <code>East</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Side</code>'s value
   */
  def transform[C, X](bf: B => Eastern[C, X], wf: W => Eastern[C, X]): Eastern[C, X] =
    thisEastern.value match {
      case East(w) => wf(w)
      case West(b) => bf(b)
    }

  /**
   * Folds this <code>Eastern</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>West</code> else the given <code>wf</code> function if this is a <code>East</code>.
   *
   * @param bf the function to apply to the underlying <code>Side</code>'s <code>West</code> value, if it is a <code>West</code>
   * @param wf the function to apply to the underlying <code>Side</code>'s <code>East</code> value, if it is a <code>East</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Side</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V =
    thisEastern.value match {
      case East(w) => wf(w)
      case West(b) => bf(b)
    }

  /**
   * A string representation for this <code>Eastern</code>.
   */
  override def toString = s"Eastern(${thisEastern.value})"
}
