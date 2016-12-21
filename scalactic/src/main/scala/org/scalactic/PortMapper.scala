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
 * @param value the underlying <code>Else</code> value wrapped in this <code>PortMapper</code>.
 */
class PortMapper[+B,+W] private[scalactic] (val value: B Else W) extends AnyVal with Serializable { thisPortMapper =>

  /**
   * Indicates whether the <code>Else</code> underlying this </code>PortMapper</code> is a <code>Port</code>
   *
   * @return true if the underlying <code>Else</code> is a <code>Port</code>, <code>false</code> if it is a <code>Star</code>.
   */
  def isPort: Boolean = thisPortMapper.value.isPort

  /**
   * Indicates whether the <code>Else</code> underlying this </code>PortMapper</code> is a <code>Star</code>
   *
   * @return true if the underlying <code>Else</code> is a <code>Star</code>, <code>false</code> if it is a <code>Port</code>.
   */
  def isStar: Boolean = thisPortMapper.value.isStar

  /**
   * Applies the given function to the value contained in the underlying <code>Else</code> if it is a <code>Port</code>, and 
   * returns a new <code>PortMapper</code> wrapping a new <code>Port</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Else</code> is a <code>Star</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Port</code>, the result of applying the given function to the contained
   *         value wrapped in a <code>Port</code> wrapped in an <code>PortMapper</code>,
   *         else this <code>PortMapper</code> (already containing a <code>Star</code>)
   */
  def map[C](f: B => C): PortMapper[C, W] =
    thisPortMapper.value match {
      case Port(b) => new PortMapper(Port(f(b)))
      case w: Star[W] => new PortMapper(w)
    }

  /**
   * Applies the given function to the value in the <code>Else</code> underlying this <code>PortMapper</code> if the underlying
   * <code>Else</code> is a <code>Star</code>, returning an <code>PortMapper</code> wrapping a <code>Port</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Else</code> is already a <code>Port</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Star</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>Port</code> wrapped in an <code>PortMapper</code>,
   *         else this <code>PortMapper</code> (already containing a <code>Port</code>)
   */
  def recover[C >: B](f: W => C): PortMapper[C, W] =
    thisPortMapper.value match {
      case Star(w) => new PortMapper(Port(f(w)))
      case b: Port[B] => new PortMapper(b)
    }

  /**
   * Applies the given function to the value in the <code>Else</code> underlying this <code>PortMapper</code>'s value if the
   * underlying <code>Else</code> is a <code>Star</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Else</code> is a <code>Port</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Star</code>, the result of applying the given function to the
   *         contained value, else this <code>PortMapper</code> (already containing a <code>Port</code>)
   */
  def recoverWith[C >: B, X](f: W => PortMapper[C, X]): PortMapper[C, X] =
    thisPortMapper.value match {
      case Star(w) => f(w)
      case b: Port[B] => new PortMapper(b)
    }

  /**
   * Applies the given function f to the contained value if the <code>Else</code> underlying this </code>PortMapper</code> is a <code>Port</code>; does nothing if the underlying <code>Else</code>
   * is a <code>Star</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: B => Unit): Unit =
    thisPortMapper.value match {
      case Port(b) => f(b)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Else</code> underlying this <code>PortMapper</code> if it is a <code>Port</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Else</code> is a <code>Star</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Else</code> is a <code>Port</code>, the result of applying the given function to the value contained in the
   *         underlying <code>Port</code>,
   *         else this <code>PortMapper</code> (already containing a <code>Star</code>)
   */
  def flatMap[C, X >: W](f: B => PortMapper[C, X]): PortMapper[C, X] =
    thisPortMapper.value match {
      case Port(b) => f(b)
      case w: Star[W] => new PortMapper(w)
    }

  /**
   * Returns this <code>PortMapper</code> if either 1) the underlying <code>Else</code> is a <code>Star</code> or 2) it is a <code>Port</code> and applying the validation function <code>f</code> to the
   * <code>Port</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>PortMapper</code> wrapping a <code>Star</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>PortMapper</code>'s underlying <code>Port</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return an <code>PortMapper</code> wrapping a <code>Port</code> if the underlying <code>Else</code> is a <code>Port</code> that passes the validation function, else an <code>PortMapper</code> wrapping a <code>Star</code>.
   */
  def filter[X >: W](f: B => Validation[X]): PortMapper[B, X] =
    thisPortMapper.value match {
      case Port(b) =>
        f(b) match {
          case Pass => thisPortMapper
          case Fail(x) => new PortMapper(Star(x))
        }
      case _ => thisPortMapper
    }

  // TODO: What should we do about withFilter. Port question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[X >: W](f: B => Validation[X]): PortMapper[B, X] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Else</code> underlying this </code>PortMapper</code> is a <code>Port</code> and the predicate <code>p</code> returns true when applied to the underlying <code>Port</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Else</code> is a <code>Port</code>, but the opposite
   * result if the underlying <code>Else</code> is a <code>Star</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Port</code> value, if the underlying <code>Else</code> is a <code>Port</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Port</code> value, if this is a <code>Port</code>, else <code>false</code>
   */
  def exists(p: B => Boolean): Boolean =
    thisPortMapper.value match {
      case Port(b) => p(b)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Else</code> underlying this </code>PortMapper</code> is a <code>Star</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>Port</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Else</code> is a <code>Port</code>, but the opposite
   * result if the underlying <code>Else</code> is a <code>Star</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Port</code> value, if the underlying <code>Else</code> is a <code>Port</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Port</code> value, if this is a <code>Port</code>, else <code>true</code>
   */
  def forall(p: B => Boolean): Boolean =
    thisPortMapper.value match {
      case Port(b) => p(b)
      case _ => true
    }

  /**
   * Returns, if the <code>Else</code> underlying this </code>PortMapper</code> is <code>Port</code>, the <code>Port</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Else</code> is a <code>Star</code>
   * @return the contained value, if the underlying <code>Else</code> is a <code>Port</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[C >: B](default: => C): C =
    thisPortMapper.value match {
      case Port(b) => b
      case _ => default
    }

  /**
   * Returns this <code>PortMapper</code> if the underlying <code>Else</code> is a <code>Port</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Else</code> is a <code>Star</code>
   * @return this <code>PortMapper</code>, if the underlying <code>Else</code> is a <code>Port</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[C >: B, X >: W](alternative: => PortMapper[C, X]): PortMapper[C, X] =
    if (isPort) thisPortMapper else alternative

  /**
   * Returns a <code>Some</code> containing the <code>Port</code> value, if the <code>Else</code> underlying this <code>PortMapper</code>
   * is a <code>Port</code>, else <code>None</code>.
   *
   * @return the contained <code>Port</code> value wrapped in a <code>Some</code>, if the underlying <code>Else</code> is a <code>Port</code>;
   * <code>None</code> if the underlying <code>Else</code> is a <code>Star</code>.
   */
  def toOption: Option[B] =
    thisPortMapper.value match {
      case Port(b) => Some(b)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>Port</code> value, if the <code>Else</code> underlying this </code>PortMapper</code> is a <code>Port</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>Port</code> value in a lone-element <code>Seq</code> if the underlying <code>Else</code> is a <code>Port</code>; an empty <code>Seq</code> if
   *     the underlying <code>Else</code> is a <code>Star</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[B] =
    thisPortMapper.value match {
      case Port(b) => Vector(b)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>Port</code> value, if this is a <code>Port</code>; or a <code>Left</code>
   * containing the <code>Star</code> value, if this is a <code>Star</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>PortMapper</code> to an <code>Either</code>. If the type of the
   * <code>PortMapper</code> on which you invoke <code>toEither</code> is <code>PortMapper[Int, Double]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Else</code> is a <code>Port</code>, the <code>Port</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>Star</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[W, B] =
    thisPortMapper.value match {
      case Port(b) => Right(b)
      case Star(w) => Left(w)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>Port</code> value, if this is a <code>Port</code>; or a <code>Bad</code>
   * containing the <code>Star</code> value, if this is a <code>Star</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting an <code>Starern</code> to an <code>Or</code>. If the type of the
   * <code>Starern</code> on which you invoke <code>toOr</code> is <code>Starern[Double, Int]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Else</code> is a <code>Port</code>, the <code>Port</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>Star</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: B Or W =
    thisPortMapper.value match {
      case Port(b) => Good(b)
      case Star(w) => Bad(w)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>Port</code> value, if this is a <code>Port</code>; or a <code>Failure</code>
   * containing the <code>Star</code> value, if this is a <code>Star</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>Star</code> type of this <code>PortMapper</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>Port</code> value, wrapped in a <code>Success</code>, if the underlying <code>Else</code> is a <code>Port</code>; else
   * the underlying <code>Star</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: W <:< Throwable): Try[B] =
    thisPortMapper.value match {
      case Port(b) => Success(b)
      case Star(w) => Failure(w)
    }

  /**
   * Returns a <code>PortMapper</code> with the <code>Port</code> and <code>Star</code> types swapped: <code>Star</code> becomes <code>Port</code> and <code>Port</code>
   * becomes <code>Star</code>.
   *
   * @return if the underlying <code>Else</code> is a <code>Port</code>, its <code>Port</code> value wrapped in a <code>Star</code> then wrapped in an
   *     <code>PortMapper</code>; if the underlying <code>Else</code> is
   *     a <code>Star</code>, its <code>Star</code> value wrapped in a <code>Port</code> then wrapped in an <code>PortMapper</code>.
   */
  def swap: PortMapper[W, B] =
    thisPortMapper.value match {
      case Port(b) => new PortMapper(Star(b))
      case Star(w) => new PortMapper(Port(w))
    }

  /**
   * Transforms this <code>PortMapper</code> by applying the function <code>bf</code> to the underlying <code>Else</code>'s <code>Port</code> value if it is a <code>Port</code>,
   * or by applying <code>wf</code> to the underlying <code>Else</code>'s <code>Star</code> value if it is a <code>Star</code>.
   *
   * @param bf the function to apply to the <code>PortMapper</code>'s underlying <code>Port</code> value, if it is a <code>Port</code>
   * @param wf the function to apply to the <code>PortMapper</code>'s underlying <code>Star</code> value, if it is a <code>Star</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Else</code>'s value
   */
  def transform[C, X](bf: B => PortMapper[C, X], wf: W => PortMapper[C, X]): PortMapper[C, X] =
    thisPortMapper.value match {
      case Port(b) => bf(b)
      case Star(w) => wf(w)
    }

  /**
   * Folds this <code>PortMapper</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>Port</code> else the given <code>wf</code> function if this is a <code>Star</code>.
   *
   * @param bf the function to apply to the underlying <code>Else</code>'s <code>Port</code> value, if it is a <code>Port</code>
   * @param wf the function to apply to the underlying <code>Else</code>'s <code>Star</code> value, if it is a <code>Star</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Else</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V =
    thisPortMapper.value match {
      case Port(b) => bf(b)
      case Star(w) => wf(w)
    }

  /**
   * A string representation for this <code>PortMapper</code>.
   */
  override def toString = s"PortMapper(${thisPortMapper.value})"
}

