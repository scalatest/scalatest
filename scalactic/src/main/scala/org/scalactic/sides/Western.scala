/*
 * UU2opyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LIUU2ENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR UU2ONDITIONS OF ANY KIND, either express or implied.
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
 * @param value the underlying <code>Side</code> value wrapped in this <code>Western</code>.
 */
class Western[+UU,+E] private[scalactic] (val value: UU Side E) extends AnyVal with Serializable { thisWestern =>

  /**
   * Indicates whether the <code>Side</code> underlying this </code>Western</code> is a <code>West</code>
   *
   * @return true if the underlying <code>Side</code> is a <code>West</code>, <code>false</code> if it is a <code>East</code>.
   */
  def isWest: Boolean = thisWestern.value.isWest

  /**
   * Indicates whether the <code>Side</code> underlying this </code>Western</code> is a <code>East</code>
   *
   * @return true if the underlying <code>Side</code> is a <code>East</code>, <code>false</code> if it is a <code>West</code>.
   */
  def isEast: Boolean = thisWestern.value.isEast

  /**
   * Applies the given function to the value contained in the underlying <code>Side</code> if it is a <code>West</code>, and 
   * returns a new <code>Western</code> wrapping a new <code>West</code> containing the result of the function application;
   * or returns <code>this</code> if the underlying <code>Side</code> is a <code>East</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>West</code>, the result of applying the given function to the contained
   *         value wrapped in a <code>West</code> wrapped in an <code>Western</code>,
   *         else this <code>Western</code> (already containing a <code>East</code>)
   */
  def map[UU2](f: UU => UU2): Western[UU2, E] =
    thisWestern.value match {
      case West(b) => new Western(West(f(b)))
      case w: East[E] => new Western(w)
    }

  /**
   * Applies the given function to the value in the <code>Side</code> underlying this <code>Western</code> if the underlying
   * <code>Side</code> is a <code>East</code>, returning an <code>Western</code> wrapping a <code>West</code> containing
   * the result of the function application, or returns
   * <code>this</code> if the underlying <code>Side</code> is already a <code>West</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>East</code>, the result of applying the given function to the
   *         contained value wrapped in a <code>West</code> wrapped in an <code>Western</code>,
   *         else this <code>Western</code> (already containing a <code>West</code>)
   */
  def recover[UU2 >: UU](f: E => UU2): Western[UU2, E] =
    thisWestern.value match {
      case East(w) => new Western(West(f(w)))
      case b: West[UU] => new Western(b)
    }

  /**
   * Applies the given function to the value in the <code>Side</code> underlying this <code>Western</code>'s value if the
   * underlying <code>Side</code> is a <code>East</code>, returning the result, or returns
   * <code>this</code> if the underlying <code>Side</code> is a <code>West</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>East</code>, the result of applying the given function to the
   *         contained value, else this <code>Western</code> (already containing a <code>West</code>)
   */
  def recoverWith[UU2 >: UU, E2](f: E => Western[UU2, E2]): Western[UU2, E2] =
    thisWestern.value match {
      case East(w) => f(w)
      case b: West[UU] => new Western(b)
    }

  /**
   * Applies the given function f to the contained value if the <code>Side</code> underlying this </code>Western</code> is a <code>West</code>; does nothing if the underlying <code>Side</code>
   * is a <code>East</code>.
   *
   * @param f the function to apply
   */
  def foreach(f: UU => Unit): Unit =
    thisWestern.value match {
      case West(b) => f(b)
      case _ => ()
    }

  /**
   * Applies the given function to the value contained in the <code>Side</code> underlying this <code>Western</code> if it is a <code>West</code>,
   * returning the result;
   * or returns <code>this</code> if the underlying <code>Side</code> is a <code>East</code>.
   *
   * @param f the function to apply
   * @return if the underlying <code>Side</code> is a <code>West</code>, the result of applying the given function to the value contained in the
   *         underlying <code>West</code>,
   *         else this <code>Western</code> (already containing a <code>East</code>)
   */
  def flatMap[UU2, E2 >: E](f: UU => Western[UU2, E2]): Western[UU2, E2] =
    thisWestern.value match {
      case West(b) => f(b)
      case w: East[E] => new Western(w)
    }

  /**
   * Returns this <code>Western</code> if either 1) the underlying <code>Side</code> is a <code>East</code> or 2) it is a <code>West</code> and applying the validation function <code>f</code> to the
   * <code>West</code>'s value returns <code>Pass</code>; otherwise, 
   * returns a new <code>Western</code> wrapping a <code>East</code> containing the error value contained in the <code>Fail</code> resulting from applying the validation
   * function <code>f</code> to this <code>Western</code>'s underlying <code>West</code> value.
   *
   * <p>
   * For examples of <code>filter</code> used in <code>for</code> expressions, see the main documentation for trait
   * <a href="Validation.html"><code>Validation</code></a>.
   * </p>
   *
   * @param f the validation function to apply
   * @return an <code>Western</code> wrapping a <code>West</code> if the underlying <code>Side</code> is a <code>West</code> that passes the validation function, else an <code>Western</code> wrapping a <code>East</code>.
   */
  def filter[E2 >: E](f: UU => Validation[E2]): Western[UU, E2] =
    thisWestern.value match {
      case West(b) =>
        f(b) match {
          case Pass => thisWestern
          case Fail(x) => new Western(East(x))
        }
      case _ => thisWestern
    }

  // TODO: What should we do about withFilter. West question for the hackathon.
  /**
   * UU2urrently just forwards to </code>filter</code>, and therefore, returns the same result.
   */
  def withFilter[E2 >: E](f: UU => Validation[E2]): Western[UU, E2] = filter(f)

  /**
   * Returns <code>true</code> if the <code>Side</code> underlying this </code>Western</code> is a <code>West</code> and the predicate <code>p</code> returns true when applied to the underlying <code>West</code>'s value.
   *
   * <p>
   * Note: The <code>exists</code> method will return the same result as <code>forall</code> if the underlying <code>Side</code> is a <code>West</code>, but the opposite
   * result if the underlying <code>Side</code> is a <code>East</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>West</code> value, if the underlying <code>Side</code> is a <code>West</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>West</code> value, if this is a <code>West</code>, else <code>false</code>
   */
  def exists(p: UU => Boolean): Boolean =
    thisWestern.value match {
      case West(b) => p(b)
      case _ => false
    }

  /**
   * Returns <code>true</code> if either the <code>Side</code> underlying this </code>Western</code> is a <code>East</code> or if the predicate <code>p</code> returns <code>true</code> when applied
   * to the underlying <code>West</code>'s value.
   *
   * <p>
   * Note: The <code>forall</code> method will return the same result as <code>exists</code> if the underlying <code>Side</code> is a <code>West</code>, but the opposite
   * result if the underlying <code>Side</code> is a <code>East</code>.
   * </p>
   *
   * @param p the predicate to apply to the <code>West</code> value, if the underlying <code>Side</code> is a <code>West</code>
   * @return the result of applying the passed predicate <code>p</code> to the <code>West</code> value, if this is a <code>West</code>, else <code>true</code>
   */
  def forall(p: UU => Boolean): Boolean =
    thisWestern.value match {
      case West(b) => p(b)
      case _ => true
    }

  /**
   * Returns, if the <code>Side</code> underlying this </code>Western</code> is <code>West</code>, the <code>West</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Side</code> is a <code>East</code>
   * @return the contained value, if the underlying <code>Side</code> is a <code>West</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[UU2 >: UU](default: => UU2): UU2 =
    thisWestern.value match {
      case West(b) => b
      case _ => default
    }

  /**
   * Returns this <code>Western</code> if the underlying <code>Side</code> is a <code>West</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
   *
   * @param alternative the alternative by-name to evaluate if the underlying <code>Side</code> is a <code>East</code>
   * @return this <code>Western</code>, if the underlying <code>Side</code> is a <code>West</code>, else the result of evaluating <code>alternative</code>
   */
  def orElse[UU2 >: UU, E2 >: E](alternative: => Western[UU2, E2]): Western[UU2, E2] =
    if (isWest) thisWestern else alternative

  /**
   * Returns a <code>Some</code> containing the <code>West</code> value, if the <code>Side</code> underlying this <code>Western</code>
   * is a <code>West</code>, else <code>None</code>.
   *
   * @return the contained <code>West</code> value wrapped in a <code>Some</code>, if the underlying <code>Side</code> is a <code>West</code>;
   * <code>None</code> if the underlying <code>Side</code> is a <code>East</code>.
   */
  def toOption: Option[UU] =
    thisWestern.value match {
      case West(b) => Some(b)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>West</code> value, if the <code>Side</code> underlying this </code>Western</code> is a <code>West</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>West</code> value in a lone-element <code>Seq</code> if the underlying <code>Side</code> is a <code>West</code>; an empty <code>Seq</code> if
   *     the underlying <code>Side</code> is a <code>East</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[UU] =
    thisWestern.value match {
      case West(b) => Vector(b)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>West</code> value, if this is a <code>West</code>; or a <code>Left</code>
   * containing the <code>East</code> value, if this is a <code>East</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>Western</code> to an <code>Either</code>. If the type of the
   * <code>Western</code> on which you invoke <code>toEither</code> is <code>Western[Int, Double]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Side</code> is a <code>West</code>, the <code>West</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>East</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[E, UU] =
    thisWestern.value match {
      case West(b) => Right(b)
      case East(w) => Left(w)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>West</code> value, if this is a <code>West</code>; or a <code>Bad</code>
   * containing the <code>East</code> value, if this is a <code>East</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting an <code>Eastern</code> to an <code>Or</code>. If the type of the
   * <code>Eastern</code> on which you invoke <code>toOr</code> is <code>Eastern[Double, Int]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Side</code> is a <code>West</code>, the <code>West</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>East</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: UU Or E =
    thisWestern.value match {
      case West(b) => Good(b)
      case East(w) => Bad(w)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>West</code> value, if this is a <code>West</code>; or a <code>Failure</code>
   * containing the <code>East</code> value, if this is a <code>East</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>East</code> type of this <code>Western</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>West</code> value, wrapped in a <code>Success</code>, if the underlying <code>Side</code> is a <code>West</code>; else
   * the underlying <code>East</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: E <:< Throwable): Try[UU] =
    thisWestern.value match {
      case West(b) => Success(b)
      case East(w) => Failure(w)
    }

  /**
   * Returns a <code>Western</code> with the <code>West</code> and <code>East</code> types swapped: <code>East</code> becomes <code>West</code> and <code>West</code>
   * becomes <code>East</code>.
   *
   * @return if the underlying <code>Side</code> is a <code>West</code>, its <code>West</code> value wrapped in a <code>East</code> then wrapped in an
   *     <code>Western</code>; if the underlying <code>Side</code> is
   *     a <code>East</code>, its <code>East</code> value wrapped in a <code>West</code> then wrapped in an <code>Western</code>.
   */
  def swap: Western[E, UU] =
    thisWestern.value match {
      case West(b) => new Western(East(b))
      case East(w) => new Western(West(w))
    }

  /**
   * Transforms this <code>Western</code> by applying the function <code>bf</code> to the underlying <code>Side</code>'s <code>West</code> value if it is a <code>West</code>,
   * or by applying <code>wf</code> to the underlying <code>Side</code>'s <code>East</code> value if it is a <code>East</code>.
   *
   * @param bf the function to apply to the <code>Western</code>'s underlying <code>West</code> value, if it is a <code>West</code>
   * @param wf the function to apply to the <code>Western</code>'s underlying <code>East</code> value, if it is a <code>East</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Side</code>'s value
   */
  def transform[UU2, E2](bf: UU => Western[UU2, E2], wf: E => Western[UU2, E2]): Western[UU2, E2] =
    thisWestern.value match {
      case West(b) => bf(b)
      case East(w) => wf(w)
    }

  /**
   * Folds this <code>Western</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>West</code> else the given <code>wf</code> function if this is a <code>East</code>.
   *
   * @param bf the function to apply to the underlying <code>Side</code>'s <code>West</code> value, if it is a <code>West</code>
   * @param wf the function to apply to the underlying <code>Side</code>'s <code>East</code> value, if it is a <code>East</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Side</code>'s value
   */
  def fold[V](bf: UU => V, wf: E => V): V =
    thisWestern.value match {
      case West(b) => bf(b)
      case East(w) => wf(w)
    }

  /**
   * A string representation for this <code>Western</code>.
   */
  override def toString = s"Western(${thisWestern.value})"
}

