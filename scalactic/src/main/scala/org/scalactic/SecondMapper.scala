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
 * @param value the underlying <code>Else</code> value wrapped in this <code>SecondMapper</code>.
 */
class SecondMapper[+B,+W] private[scalactic] (val value: B Else W) extends AnyVal with Serializable { thisSecondMapper =>

  /**
   * Indicates whether the <code>Else</code> underlying this <code>SecondMapper</code> is a <code>First</code>
   *
   * @return true if the underlying <code>Else</code> is a <code>First</code>, <code>false</code> if it is a <code>Second</code>.
   */
  def isFirst: Boolean = thisSecondMapper.value.isFirst

  /**
   * Indicates whether the <code>Else</code> underlying this <code>SecondMapper</code> is a <code>Second</code>
   *
   * @return true if the underlying <code>Else</code> is a <code>Second</code>, <code>false</code> if it is a <code>First</code>.
   */
  def isSecond: Boolean = thisSecondMapper.value.isSecond

  /**
   * Applies the given function to the value contained in the underlying <code>Else</code> if it is a <code>Second</code>, and 
   * returns a new <code>SecondMapper</code> wrapping a new <code>Second</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Else</code> is a <code>First</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Second</code>, the result of applying the given function to the contained value wrapped in a <code>Second</code> wrapped in an <code>SecondMapper</code>,
   *         else this <code>SecondMapper</code> (already containing a <code>First</code>)
   */
  def map[X](f: W => X): SecondMapper[B, X] =
    thisSecondMapper.value match {
      case Second(w) => new SecondMapper(Second(f(w)))
      case b: First[B] => new SecondMapper(b) // My dox says "this", but i'm doing new, but it is an AnyVal so it shouldn't box. 
    }                                  // For an AnyVal, "this" can kind of refer to the underlying seems like, and I am returning that.

  /**
   * Applies the given function to the value in the <code>Else</code> underlying this <code>SecondMapper</code> if the underlying
   * <code>Else</code> is a <code>First</code>, returning an <code>SecondMapper</code> wrapping a <code>Second</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Else</code> is already a <code>Second</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>First</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>Second</code> wrapped in an <code>SecondMapper</code>,
   *         else this <code>SecondMapper</code> (already containing a <code>Second</code>)
   */
  def recover[X >: W](f: B => X): SecondMapper[B, X] =
    thisSecondMapper.value match {
      case First(b) => new SecondMapper(Second(f(b)))
      case w: Second[W] => new SecondMapper(w)
    }

  /**
   * Applies the given function to the value in the <code>Else</code> underlying this <code>SecondMapper</code>'s value if the
   * underlying <code>Else</code> is a <code>First</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Else</code> is already a <code>Second</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>First</code>, the result of applying the given function to the
   *         contained value, else this <code>SecondMapper</code> (already containing a <code>Second</code>)
   */
  def recoverWith[C, X >: W](f: B => SecondMapper[C, X]): SecondMapper[C, X] =
    thisSecondMapper.value match {
      case First(b) => f(b)
      case w: Second[W] => new SecondMapper(w) // It looks inefficient to an old C programmer, but it doesn't box because AnyVal
    }

  /**
   * Applies the given function f to the contained value if the <code>Else</code> underlying this <code>SecondMapper</code> is a <code>Second</code>; does nothing if the underlying <code>Else</code>
   * is a <code>First</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: W => Unit): Unit =
    thisSecondMapper.value match {
      case Second(w) => f(w)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Else</code> underlying this <code>SecondMapper</code> if it is a <code>Second</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Else</code> is a <code>First</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Second</code>, the result of applying the given function to the value contained in the
   *         underlying <code>Second</code>,
   *         else this <code>SecondMapper</code> (already containing a <code>First</code>)
   */
  def flatMap[C >: B, X](f: W => SecondMapper[C, X]): SecondMapper[C, X] =
    thisSecondMapper.value match {
      case Second(w) => f(w)
      case b: First[B] => new SecondMapper(b)
    }

  /**
   * Returns this <code>SecondMapper</code> if either 1) the underlying <code>Else</code> is a <code>First</code> or 2) it is a <code>Second</code> and applying the validation function <code>f</code> to the
   * <code>Second</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>SecondMapper</code> wrapping a <code>First</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>SecondMapper</code>'s underlying <code>Second</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return an <code>SecondMapper</code> wrapping a <code>Second</code> if the underlying <code>Else</code> is a <code>Second</code> that passes the validation function, else an <code>SecondMapper</code> wrapping a <code>First</code>.
   */
  def filter[C >: B](f: W => Validation[C]): SecondMapper[C, W] =
    thisSecondMapper.value match {
      case Second(w) =>
        f(w) match {
          case Pass => thisSecondMapper
          case Fail(c) => new SecondMapper(First(c))
        }
       case _ => thisSecondMapper
    }

  // TODO: What should we do about withFilter. First question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[C >: B](f: W => Validation[C]): SecondMapper[C, W] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Else</code> underlying this <code>SecondMapper</code> is a <code>Second</code> and the predicate <code>p</code> returns true when applied to the underlying <code>Second</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Else</code> is a <code>Second</code>, but the opposite
   * result if the underlying <code>Else</code> is a <code>First</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Second</code> value, if the underlying <code>Else</code> is a <code>Second</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Second</code> value, if this is a <code>Second</code>, else <code>false</code>
   */
  def exists(p: W => Boolean): Boolean =
    thisSecondMapper.value match {
      case Second(w) => p(w)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Else</code> underlying this <code>SecondMapper</code> is a <code>Second</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>Second</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Else</code> is a <code>Second</code>, but the opposite
   * result if the underlying <code>Else</code> is a <code>First</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>First</code> value, if the underlying <code>Else</code> is a <code>First</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Second</code> value, if this is a <code>Second</code>, else <code>true</code>
   */
  def forall(p: W => Boolean): Boolean =
    thisSecondMapper.value match {
      case Second(w) => p(w)
      case _ => true
    }

  /**
   * Returns, if the <code>Else</code> underlying this <code>SecondMapper</code> is <code>Second</code>, the <code>Second</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Else</code> is a <code>First</code>
   * @return the contained value, if the underlying <code>Else</code> is a <code>Second</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[X >: W](default: => X): X =
    thisSecondMapper.value match {
      case Second(w) => w
      case _ => default
    }

  /**
   * Returns this <code>SecondMapper</code> if the underlying <code>Else</code> is a <code>Second</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Else</code> is a <code>First</code>
   * @return this <code>SecondMapper</code>, if the underlying <code>Else</code> is a <code>Second</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[C >: B, X >: W](alternative: => SecondMapper[C, X]): SecondMapper[C, X] =
    if (isSecond) thisSecondMapper else alternative

  /**
   * Returns a <code>Some</code> containing the <code>Second</code> value, if the <code>Else</code> underlying this <code>SecondMapper</code>
   * is a <code>Second</code>, else <code>None</code>.
   *
   * @return the contained <code>Second</code> value wrapped in a <code>Some</code>, if the <code>Else</code> underlying this <code>SecondMapper</code>
   * is a <code>Second</code>; <code>None</code> if the underlying <code>Else</code> is a <code>First</code>.
   */
  def toOption: Option[W] =
    thisSecondMapper.value match {
      case Second(w) => Some(w)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>Second</code> value, if the <code>Else</code> underlying this <code>SecondMapper</code> is a <code>Second</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>Second</code> value in a lone-element <code>Seq</code> if the underlying <code>Else</code> is a <code>Second</code>; an empty <code>Seq</code> if
   *     the underlying <code>Else</code> is a <code>First</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[W] =
    thisSecondMapper.value match {
      case Second(w) => Vector(w)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>Second</code> value, if this is a <code>Second</code>; or a <code>Left</code>
   * containing the <code>First</code> value, if this is a <code>First</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting an <code>SecondMapper</code> to an <code>Either</code>. If the type of the
   * <code>SecondMapper</code> on which you invoke <code>toEither</code> is <code>SecondMapper[Double, Int]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Else</code> is a <code>Second</code>, the <code>Second</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>First</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[B, W] =
    thisSecondMapper.value match {
      case Second(w) => Right(w)
      case First(b) => Left(b)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>Second</code> value, if this is a <code>Second</code>; or a <code>Bad</code>
   * containing the <code>First</code> value, if this is a <code>First</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>SecondMapper</code> to an <code>Or</code>. If the type of the
   * <code>SecondMapper</code> on which you invoke <code>toOr</code> is <code>SecondMapper[Int, Double]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   * 
   * @return if the underlying <code>Else</code> is a <code>Second</code>, the <code>Second</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>First</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: W Or B =
    thisSecondMapper.value match {
      case Second(w) => Good(w)
      case First(b) => Bad(b)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>Second</code> value, if this is a <code>Second</code>; or a <code>Failure</code>
   * containing the <code>First</code> value, if this is a <code>First</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>First</code> type of this <code>SecondMapper</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>Second</code> value, wrapped in a <code>Success</code>, if the underlying <code>Else</code> is a <code>Second</code>; else
   * the underlying <code>First</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: B <:< Throwable): Try[W] =
    thisSecondMapper.value match {
      case Second(w) => Success(w)
      case First(b) => Failure(b)
    }

  /**
   * Returns an <code>SecondMapper</code> with the <code>First</code> and <code>Second</code> types swapped: <code>Second</code> becomes <code>First</code> and <code>First</code>
   * becomes <code>Second</code>.
   *
   * @return if the underlying <code>Else</code> is a <code>First</code>, its <code>First</code> value wrapped in a <code>Second</code> then wrapped in
   * an <code>SecondMapper</code>; if the underlying <code>Else</code> is
   *     a <code>Second</code>, its <code>Second</code> value wrapped in a <code>First</code> then wrapped in an <code>SecondMapper</code>.
   */
  def swap: SecondMapper[W, B] =
    thisSecondMapper.value match {
      case Second(w) => new SecondMapper(First(w))
      case First(b) => new SecondMapper(Second(b))
    }

  /**
   * Transforms this <code>SecondMapper</code> by applying the function <code>bf</code> to the underlying <code>Else</code>'s <code>First</code> value if it is a <code>First</code>,
   * or by applying <code>wf</code> to the underlying <code>Else</code>'s <code>Second</code> value if it is a <code>Second</code>.
   *
   * @param bf the function to apply to the <code>SecondMapper</code>'s underlying <code>First</code> value, if it is a <code>First</code>
   * @param wf the function to apply to the <code>SecondMapper</code>'s underlying <code>Second</code> value, if it is a <code>Second</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Else</code>'s value
   */
  def transform[C, X](bf: B => SecondMapper[C, X], wf: W => SecondMapper[C, X]): SecondMapper[C, X] =
    thisSecondMapper.value match {
      case Second(w) => wf(w)
      case First(b) => bf(b)
    }

  /**
   * Folds this <code>SecondMapper</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>First</code> else the given <code>wf</code> function if this is a <code>Second</code>.
   *
   * @param bf the function to apply to the underlying <code>Else</code>'s <code>First</code> value, if it is a <code>First</code>
   * @param wf the function to apply to the underlying <code>Else</code>'s <code>Second</code> value, if it is a <code>Second</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Else</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V =
    thisSecondMapper.value match {
      case Second(w) => wf(w)
      case First(b) => bf(b)
    }

  /**
   * A string representation for this <code>SecondMapper</code>.
   */
  override def toString = s"SecondMapper(${thisSecondMapper.value})"
}
