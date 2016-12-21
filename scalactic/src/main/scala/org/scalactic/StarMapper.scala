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
 * @param value the underlying <code>Else</code> value wrapped in this <code>StarMapper</code>.
 */
class StarMapper[+B,+W] private[scalactic] (val value: B Else W) extends AnyVal with Serializable { thisStarMapper =>

  /**
   * Indicates whether the <code>Else</code> underlying this <code>StarMapper</code> is a <code>Port</code>
   *
   * @return true if the underlying <code>Else</code> is a <code>Port</code>, <code>false</code> if it is a <code>Star</code>.
   */
  def isPort: Boolean = thisStarMapper.value.isPort

  /**
   * Indicates whether the <code>Else</code> underlying this <code>StarMapper</code> is a <code>Star</code>
   *
   * @return true if the underlying <code>Else</code> is a <code>Star</code>, <code>false</code> if it is a <code>Port</code>.
   */
  def isStar: Boolean = thisStarMapper.value.isStar

  /**
   * Applies the given function to the value contained in the underlying <code>Else</code> if it is a <code>Star</code>, and 
   * returns a new <code>StarMapper</code> wrapping a new <code>Star</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Else</code> is a <code>Port</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Star</code>, the result of applying the given function to the contained value wrapped in a <code>Star</code> wrapped in an <code>StarMapper</code>,
   *         else this <code>StarMapper</code> (already containing a <code>Port</code>)
   */
  def map[X](f: W => X): StarMapper[B, X] =
    thisStarMapper.value match {
      case Star(w) => new StarMapper(Star(f(w)))
      case b: Port[B] => new StarMapper(b) // My dox says "this", but i'm doing new, but it is an AnyVal so it shouldn't box. 
    }                                  // For an AnyVal, "this" can kind of refer to the underlying seems like, and I am returning that.

  /**
   * Applies the given function to the value in the <code>Else</code> underlying this <code>StarMapper</code> if the underlying
   * <code>Else</code> is a <code>Port</code>, returning an <code>StarMapper</code> wrapping a <code>Star</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Else</code> is already a <code>Star</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Port</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>Star</code> wrapped in an <code>StarMapper</code>,
   *         else this <code>StarMapper</code> (already containing a <code>Star</code>)
   */
  def recover[X >: W](f: B => X): StarMapper[B, X] =
    thisStarMapper.value match {
      case Port(b) => new StarMapper(Star(f(b)))
      case w: Star[W] => new StarMapper(w)
    }

  /**
   * Applies the given function to the value in the <code>Else</code> underlying this <code>StarMapper</code>'s value if the
   * underlying <code>Else</code> is a <code>Port</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Else</code> is already a <code>Star</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Port</code>, the result of applying the given function to the
   *         contained value, else this <code>StarMapper</code> (already containing a <code>Star</code>)
   */
  def recoverWith[C, X >: W](f: B => StarMapper[C, X]): StarMapper[C, X] =
    thisStarMapper.value match {
      case Port(b) => f(b)
      case w: Star[W] => new StarMapper(w) // It looks inefficient to an old C programmer, but it doesn't box because AnyVal
    }

  /**
   * Applies the given function f to the contained value if the <code>Else</code> underlying this <code>StarMapper</code> is a <code>Star</code>; does nothing if the underlying <code>Else</code>
   * is a <code>Port</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: W => Unit): Unit =
    thisStarMapper.value match {
      case Star(w) => f(w)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Else</code> underlying this <code>StarMapper</code> if it is a <code>Star</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Else</code> is a <code>Port</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Star</code>, the result of applying the given function to the value contained in the
   *         underlying <code>Star</code>,
   *         else this <code>StarMapper</code> (already containing a <code>Port</code>)
   */
  def flatMap[C >: B, X](f: W => StarMapper[C, X]): StarMapper[C, X] =
    thisStarMapper.value match {
      case Star(w) => f(w)
      case b: Port[B] => new StarMapper(b)
    }

  /**
   * Returns this <code>StarMapper</code> if either 1) the underlying <code>Else</code> is a <code>Port</code> or 2) it is a <code>Star</code> and applying the validation function <code>f</code> to the
   * <code>Star</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>StarMapper</code> wrapping a <code>Port</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>StarMapper</code>'s underlying <code>Star</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return an <code>StarMapper</code> wrapping a <code>Star</code> if the underlying <code>Else</code> is a <code>Star</code> that passes the validation function, else an <code>StarMapper</code> wrapping a <code>Port</code>.
   */
  def filter[C >: B](f: W => Validation[C]): StarMapper[C, W] =
    thisStarMapper.value match {
      case Star(w) =>
        f(w) match {
          case Pass => thisStarMapper
          case Fail(c) => new StarMapper(Port(c))
        }
       case _ => thisStarMapper
    }

  // TODO: What should we do about withFilter. Port question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[C >: B](f: W => Validation[C]): StarMapper[C, W] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Else</code> underlying this <code>StarMapper</code> is a <code>Star</code> and the predicate <code>p</code> returns true when applied to the underlying <code>Star</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Else</code> is a <code>Star</code>, but the opposite
   * result if the underlying <code>Else</code> is a <code>Port</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Star</code> value, if the underlying <code>Else</code> is a <code>Star</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Star</code> value, if this is a <code>Star</code>, else <code>false</code>
   */
  def exists(p: W => Boolean): Boolean =
    thisStarMapper.value match {
      case Star(w) => p(w)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Else</code> underlying this <code>StarMapper</code> is a <code>Star</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>Star</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Else</code> is a <code>Star</code>, but the opposite
   * result if the underlying <code>Else</code> is a <code>Port</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Port</code> value, if the underlying <code>Else</code> is a <code>Port</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Star</code> value, if this is a <code>Star</code>, else <code>true</code>
   */
  def forall(p: W => Boolean): Boolean =
    thisStarMapper.value match {
      case Star(w) => p(w)
      case _ => true
    }

  /**
   * Returns, if the <code>Else</code> underlying this <code>StarMapper</code> is <code>Star</code>, the <code>Star</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Else</code> is a <code>Port</code>
   * @return the contained value, if the underlying <code>Else</code> is a <code>Star</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[X >: W](default: => X): X =
    thisStarMapper.value match {
      case Star(w) => w
      case _ => default
    }

  /**
   * Returns this <code>StarMapper</code> if the underlying <code>Else</code> is a <code>Star</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Else</code> is a <code>Port</code>
   * @return this <code>StarMapper</code>, if the underlying <code>Else</code> is a <code>Star</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[C >: B, X >: W](alternative: => StarMapper[C, X]): StarMapper[C, X] =
    if (isStar) thisStarMapper else alternative

  /**
   * Returns a <code>Some</code> containing the <code>Star</code> value, if the <code>Else</code> underlying this <code>StarMapper</code>
   * is a <code>Star</code>, else <code>None</code>.
   *
   * @return the contained <code>Star</code> value wrapped in a <code>Some</code>, if the <code>Else</code> underlying this <code>StarMapper</code>
   * is a <code>Star</code>; <code>None</code> if the underlying <code>Else</code> is a <code>Port</code>.
   */
  def toOption: Option[W] =
    thisStarMapper.value match {
      case Star(w) => Some(w)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>Star</code> value, if the <code>Else</code> underlying this <code>StarMapper</code> is a <code>Star</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>Star</code> value in a lone-element <code>Seq</code> if the underlying <code>Else</code> is a <code>Star</code>; an empty <code>Seq</code> if
   *     the underlying <code>Else</code> is a <code>Port</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[W] =
    thisStarMapper.value match {
      case Star(w) => Vector(w)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>Star</code> value, if this is a <code>Star</code>; or a <code>Left</code>
   * containing the <code>Port</code> value, if this is a <code>Port</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting an <code>StarMapper</code> to an <code>Either</code>. If the type of the
   * <code>StarMapper</code> on which you invoke <code>toEither</code> is <code>StarMapper[Double, Int]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Else</code> is a <code>Star</code>, the <code>Star</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>Port</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[B, W] =
    thisStarMapper.value match {
      case Star(w) => Right(w)
      case Port(b) => Left(b)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>Star</code> value, if this is a <code>Star</code>; or a <code>Bad</code>
   * containing the <code>Port</code> value, if this is a <code>Port</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>StarMapper</code> to an <code>Or</code>. If the type of the
   * <code>StarMapper</code> on which you invoke <code>toOr</code> is <code>StarMapper[Int, Double]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   * 
   * @return if the underlying <code>Else</code> is a <code>Star</code>, the <code>Star</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>Port</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: W Or B =
    thisStarMapper.value match {
      case Star(w) => Good(w)
      case Port(b) => Bad(b)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>Star</code> value, if this is a <code>Star</code>; or a <code>Failure</code>
   * containing the <code>Port</code> value, if this is a <code>Port</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>Port</code> type of this <code>StarMapper</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>Star</code> value, wrapped in a <code>Success</code>, if the underlying <code>Else</code> is a <code>Star</code>; else
   * the underlying <code>Port</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: B <:< Throwable): Try[W] =
    thisStarMapper.value match {
      case Star(w) => Success(w)
      case Port(b) => Failure(b)
    }

  /**
   * Returns an <code>StarMapper</code> with the <code>Port</code> and <code>Star</code> types swapped: <code>Star</code> becomes <code>Port</code> and <code>Port</code>
   * becomes <code>Star</code>.
   *
   * @return if the underlying <code>Else</code> is a <code>Port</code>, its <code>Port</code> value wrapped in a <code>Star</code> then wrapped in
   * an <code>StarMapper</code>; if the underlying <code>Else</code> is
   *     a <code>Star</code>, its <code>Star</code> value wrapped in a <code>Port</code> then wrapped in an <code>StarMapper</code>.
   */
  def swap: StarMapper[W, B] =
    thisStarMapper.value match {
      case Star(w) => new StarMapper(Port(w))
      case Port(b) => new StarMapper(Star(b))
    }

  /**
   * Transforms this <code>StarMapper</code> by applying the function <code>bf</code> to the underlying <code>Else</code>'s <code>Port</code> value if it is a <code>Port</code>,
   * or by applying <code>wf</code> to the underlying <code>Else</code>'s <code>Star</code> value if it is a <code>Star</code>.
   *
   * @param bf the function to apply to the <code>StarMapper</code>'s underlying <code>Port</code> value, if it is a <code>Port</code>
   * @param wf the function to apply to the <code>StarMapper</code>'s underlying <code>Star</code> value, if it is a <code>Star</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Else</code>'s value
   */
  def transform[C, X](bf: B => StarMapper[C, X], wf: W => StarMapper[C, X]): StarMapper[C, X] =
    thisStarMapper.value match {
      case Star(w) => wf(w)
      case Port(b) => bf(b)
    }

  /**
   * Folds this <code>StarMapper</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>Port</code> else the given <code>wf</code> function if this is a <code>Star</code>.
   *
   * @param bf the function to apply to the underlying <code>Else</code>'s <code>Port</code> value, if it is a <code>Port</code>
   * @param wf the function to apply to the underlying <code>Else</code>'s <code>Star</code> value, if it is a <code>Star</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Else</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V =
    thisStarMapper.value match {
      case Star(w) => wf(w)
      case Port(b) => bf(b)
    }

  /**
   * A string representation for this <code>StarMapper</code>.
   */
  override def toString = s"StarMapper(${thisStarMapper.value})"
}
