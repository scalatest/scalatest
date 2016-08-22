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
 * @param value the underlying <code>Or</code> value wrapped in this <code>Mitigator</code>.
 */
class Mitigator[+G,+B] private[scalactic] (val value: G Or B) extends AnyVal with Serializable { thisMitigator =>

  /**
   * Indicates whether the <code>Or</code> underlying this <code>Mitigator</code> is a <code>Good</code>
   *
   * @return true if the underlying <code>Or</code> is a <code>Good</code>, <code>false</code> if it is a <code>Bad</code>.
   */
  def isGood: Boolean = thisMitigator.value.isGood

  /**
   * Indicates whether the <code>Or</code> underlying this <code>Mitigator</code> is a <code>Bad</code>
   *
   * @return true if the underlying <code>Or</code> is a <code>Bad</code>, <code>false</code> if it is a <code>Good</code>.
   */
  def isBad: Boolean = thisMitigator.value.isBad

  /**
   * Applies the given function to the value contained in the underlying <code>Or</code> if it is a <code>Bad</code>, and 
   * returns a new <code>Mitigator</code> wrapping a new <code>Bad</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Or</code> is a <code>Good</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Or</code> is a <code>Bad</code>, the result of applying the given function to the contained value wrapped in a <code>Bad</code> wrapped in a <code>Mitigator</code>,
   *         else this <code>Mitigator</code> (already containing a <code>Good</code>)
   */
  def map[C](f: B => C): Mitigator[G, C] =
    thisMitigator.value match {
      case Bad(w) => new Mitigator(Bad(f(w)))
      case b: Good[G] => new Mitigator(b) // My dox says "this", but i'm doing new, but it is an AnyVal so it shouldn't box. 
    }                                  // For an AnyVal, "this" can kind of refer to the underlying seems like, and I am returning that.

  /**
   * Applies the given function to the value in the <code>Or</code> underlying this <code>Mitigator</code> if the underlying
   * <code>Or</code> is a <code>Good</code>, returning a <code>Mitigator</code> wrapping a <code>Bad</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Or</code> is already a <code>Bad</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Or</code> is a <code>Good</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>Bad</code> wrapped in a <code>Mitigator</code>,
   *         else this <code>Mitigator</code> (already containing a <code>Bad</code>)
   */
  def recover[C >: B](f: G => C): Mitigator[G, C] =
    thisMitigator.value match {
      case Good(b) => new Mitigator(Bad(f(b)))
      case w: Bad[B] => new Mitigator(w)
    }

  /**
   * Applies the given function to the value in the <code>Or</code> underlying this <code>Mitigator</code>'s value if the
   * underlying <code>Or</code> is a <code>Good</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Or</code> is already a <code>Bad</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Or</code> is a <code>Good</code>, the result of applying the given function to the
   *         contained value, else this <code>Mitigator</code> (already containing a <code>Bad</code>)
   */
  def recoverWith[H, C >: B](f: G => Mitigator[H, C]): Mitigator[H, C] =
    thisMitigator.value match {
      case Good(b) => f(b)
      case w: Bad[B] => new Mitigator(w) // It looks inefficient to an old C programmer, but it doesn't box because AnyVal
    }

  /**
   * Applies the given function f to the contained value if the <code>Or</code> underlying this <code>Mitigator</code> is a <code>Bad</code>; does nothing if the underlying <code>Or</code>
   * is a <code>Good</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: B => Unit): Unit =
    thisMitigator.value match {
      case Bad(w) => f(w)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Or</code> underlying this <code>Mitigator</code> if it is a <code>Bad</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Or</code> is a <code>Good</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Or</code> is a <code>Bad</code>, the result of applying the given function to the value contained in the
   *         underlying <code>Bad</code>,
   *         else this <code>Mitigator</code> (already containing a <code>Good</code>)
   */
  def flatMap[H >: G, C](f: B => Mitigator[H, C]): Mitigator[H, C] =
    thisMitigator.value match {
      case Bad(w) => f(w)
      case b: Good[G] => new Mitigator(b)
    }

  /**
   * Returns this <code>Mitigator</code> if either 1) the underlying <code>Or</code> is a <code>Good</code> or 2) it is a <code>Bad</code> and applying the validation function <code>f</code> to the
   * <code>Bad</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>Mitigator</code> wrapping a <code>Good</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>Mitigator</code>'s underlying <code>Bad</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return a <code>Mitigator</code> wrapping a <code>Bad</code> if the underlying <code>Or</code> is a <code>Bad</code> that passes the validation function, else a <code>Mitigator</code> wrapping a <code>Good</code>.
   */
  def filter[H >: G](f: B => Validation[H]): Mitigator[H, B] =
    thisMitigator.value match {
      case Bad(w) =>
        f(w) match {
          case Pass => thisMitigator
          case Fail(c) => new Mitigator(Good(c))
        }
       case _ => thisMitigator
    }

  // TODO: What should we do about withFilter. Good question for the hackathon.
  /**
   * Currently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[H >: G](f: B => Validation[H]): Mitigator[H, B] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Or</code> underlying this <code>Mitigator</code> is a <code>Bad</code> and the predicate <code>p</code> returns true when applied to the underlying <code>Bad</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Or</code> is a <code>Bad</code>, but the opposite
   * result if the underlying <code>Or</code> is a <code>Good</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Bad</code> value, if the underlying <code>Or</code> is a <code>Bad</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Bad</code> value, if this is a <code>Bad</code>, else <code>false</code>
   */
  def exists(p: B => Boolean): Boolean =
    thisMitigator.value match {
      case Bad(w) => p(w)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Or</code> underlying this <code>Mitigator</code> is a <code>Bad</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>Bad</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Or</code> is a <code>Bad</code>, but the opposite
   * result if the underlying <code>Or</code> is a <code>Good</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>Good</code> value, if the underlying <code>Or</code> is a <code>Good</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>Bad</code> value, if this is a <code>Bad</code>, else <code>true</code>
   */
  def forall(p: B => Boolean): Boolean =
    thisMitigator.value match {
      case Bad(w) => p(w)
      case _ => true
    }

  /**
   * Returns, if the <code>Or</code> underlying this <code>Mitigator</code> is <code>Bad</code>, the <code>Bad</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Or</code> is a <code>Good</code>
   * @return the contained value, if the underlying <code>Or</code> is a <code>Bad</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[C >: B](default: => C): C =
    thisMitigator.value match {
      case Bad(w) => w
      case _ => default
    }

  /**
   * Returns this <code>Mitigator</code> if the underlying <code>Or</code> is a <code>Bad</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Or</code> is a <code>Good</code>
   * @return this <code>Mitigator</code>, if the underlying <code>Or</code> is a <code>Bad</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[H >: G, C >: B](alternative: => Mitigator[H, C]): Mitigator[H, C] =
    if (isBad) thisMitigator else alternative

  /**
   * Returns a <code>Some</code> containing the <code>Bad</code> value, if the <code>Or</code> underlying this <code>Mitigator</code>
   * is a <code>Bad</code>, else <code>None</code>.
   *
   * @return the contained <code>Bad</code> value wrapped in a <code>Some</code>, if the <code>Or</code> underlying this <code>Mitigator</code>
   * is a <code>Bad</code>; <code>None</code> if the underlying <code>Or</code> is a <code>Good</code>.
   */
  def toOption: Option[B] =
    thisMitigator.value match {
      case Bad(w) => Some(w)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>Bad</code> value, if the <code>Or</code> underlying this <code>Mitigator</code> is a <code>Bad</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>Bad</code> value in a lone-element <code>Seq</code> if the underlying <code>Or</code> is a <code>Bad</code>; an empty <code>Seq</code> if
   *     the underlying <code>Or</code> is a <code>Good</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[B] =
    thisMitigator.value match {
      case Bad(w) => Vector(w)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>Bad</code> value, if this is a <code>Bad</code>; or a <code>Left</code>
   * containing the <code>Good</code> value, if this is a <code>Good</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting a <code>Mitigator</code> to an <code>Either</code>. If the type of the
   * <code>Mitigator</code> on which you invoke <code>toEither</code> is <code>Mitigator[Double, Int]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Or</code> is a <code>Bad</code>, the <code>Bad</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>Good</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[G, B] =
    thisMitigator.value match {
      case Bad(w) => Right(w)
      case Good(b) => Left(b)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>Bad</code> value, if this is a <code>Bad</code>; or a <code>Bad</code>
   * containing the <code>Good</code> value, if this is a <code>Good</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting a <code>Mitigator</code> to an <code>Or</code>. If the type of the
   * <code>Mitigator</code> on which you invoke <code>toOr</code> is <code>Mitigator[Int, Double]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   * 
   * @return if the underlying <code>Or</code> is a <code>Bad</code>, the <code>Bad</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>Good</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: B Or G =
    thisMitigator.value match {
      case Bad(w) => Good(w)
      case Good(b) => Bad(b)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>Bad</code> value, if this is a <code>Bad</code>; or a <code>Failure</code>
   * containing the <code>Good</code> value, if this is a <code>Good</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>Good</code> type of this <code>Mitigator</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>Bad</code> value, wrapped in a <code>Success</code>, if the underlying <code>Or</code> is a <code>Bad</code>; else
   * the underlying <code>Good</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: G <:< Throwable): Try[B] =
    thisMitigator.value match {
      case Bad(w) => Success(w)
      case Good(b) => Failure(b)
    }

  /**
   * Returns a <code>Mitigator</code> with the <code>Good</code> and <code>Bad</code> types swapped: <code>Bad</code> becomes <code>Good</code> and <code>Good</code>
   * becomes <code>Bad</code>.
   *
   * @return if the underlying <code>Or</code> is a <code>Good</code>, its <code>Good</code> value wrapped in a <code>Bad</code> then wrapped in
   * a <code>Mitigator</code>; if the underlying <code>Or</code> is
   *     a <code>Bad</code>, its <code>Bad</code> value wrapped in a <code>Good</code> then wrapped in a <code>Mitigator</code>.
   */
  def swap: Mitigator[B, G] =
    thisMitigator.value match {
      case Bad(w) => new Mitigator(Good(w))
      case Good(b) => new Mitigator(Bad(b))
    }

  /**
   * Transforms this <code>Mitigator</code> by applying the function <code>gf</code> to the underlying <code>Or</code>'s <code>Good</code> value if it is a <code>Good</code>,
   * or by applying <code>bf</code> to the underlying <code>Or</code>'s <code>Bad</code> value if it is a <code>Bad</code>.
   *
   * @param gf the function to apply to the <code>Mitigator</code>'s underlying <code>Good</code> value, if it is a <code>Good</code>
   * @param bf the function to apply to the <code>Mitigator</code>'s underlying <code>Bad</code> value, if it is a <code>Bad</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>gf</code> or </code>bf</code>, to the underlying <code>Or</code>'s value
   */
  def transform[H, C](gf: G => Mitigator[H, C], bf: B => Mitigator[H, C]): Mitigator[H, C] =
    thisMitigator.value match {
      case Bad(w) => bf(w)
      case Good(b) => gf(b)
    }

  /**
   * Folds this <code>Mitigator</code> into a value of type <code>V</code> by applying the given <code>gf</code> function if this is
   * a <code>Good</code> else the given <code>bf</code> function if this is a <code>Bad</code>.
   *
   * @param gf the function to apply to the underlying <code>Or</code>'s <code>Good</code> value, if it is a <code>Good</code>
   * @param bf the function to apply to the underlying <code>Or</code>'s <code>Bad</code> value, if it is a <code>Bad</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>gf</code> or </code>bf</code>, to the underlying <code>Or</code>'s value
   */
  def fold[V](gf: G => V, bf: B => V): V =
    thisMitigator.value match {
      case Bad(w) => bf(w)
      case Good(b) => gf(b)
    }

  /**
   * A string representation for this <code>Mitigator</code>.
   */
  override def toString = s"Mitigator($thisMitigator.value)"
}
