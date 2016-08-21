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
 * @param value the underlying <code>Otherwise</code> value wrapped in this <code>Ebony</code>.
 */
class Ebony[+B,+W] private[scalactic] (val value: B Otherwise W) extends AnyVal with Serializable { thisEbony =>

  /**
   * Indicates whether the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>Black</code>
   *
   * @return true if the underlying <code>Otherwise</code> is a <code>Black</code>, <code>false</code> if it is a <code>White</code>.
   */
  def isBlack: Boolean = thisEbony.value.isBlack

  /**
   * Indicates whether the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>White</code>
   *
   * @return true if the underlying <code>Otherwise</code> is a <code>White</code>, <code>false</code> if it is a <code>Black</code>.
   */
  def isWhite: Boolean = thisEbony.value.isWhite

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
    thisEbony.value match {
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
    thisEbony.value match {
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
    thisEbony.value match {
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
    thisEbony.value match {
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
    thisEbony.value match {
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
    thisEbony.value match {
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
    thisEbony.value match {
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
    thisEbony.value match {
      case Black(b) => p(b)
      case _ => true
    }

  /**
   * Returns, if the <code>Otherwise</code> underlying this </code>Ebony</code> is <code>Black</code>, the <code>Black</code>'s value; otherwise returns the result of evaluating <code>default</code>. 
   *
   * @param default the default expression to evaluate if the underlying <code>Otherwise</code> is a <code>White</code>
   * @return the contained value, if the underlying <code>Otherwise</code> is a <code>Black</code>, else the result of evaluating the given <code>default</code>
   */
  def getOrElse[C >: B](default: => C): C =
    thisEbony.value match {
      case Black(b) => b
      case _ => default
    }

  /**
   * Returns this <code>Ebony</code> if the underlying <code>Otherwise</code> is a <code>Black</code>, otherwise returns the result of evaluating the passed <code>alternative</code>.
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
   * @return the contained <code>Black</code> value wrapped in a <code>Some</code>, if the underlying <code>Otherwise</code> is a <code>Black</code>;
   * <code>None</code> if the underlying <code>Otherwise</code> is a <code>White</code>.
   */
  def toOption: Option[B] =
    thisEbony.value match {
      case Black(b) => Some(b)
      case _ => None
    }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the <code>Black</code> value, if the <code>Otherwise</code> underlying this </code>Ebony</code> is a <code>Black</code>, else an empty
   * immutable <code>IndexedSeq</code>.
   *
   * @return the contained <code>Black</code> value in a lone-element <code>Seq</code> if the underlying <code>Otherwise</code> is a <code>Black</code>; an empty <code>Seq</code> if
   *     the underlying <code>Otherwise</code> is a <code>White</code>.
   */
  def toSeq: scala.collection.immutable.IndexedSeq[B] =
    thisEbony.value match {
      case Black(b) => Vector(b)
      case _ => Vector.empty
    }

  /**
   * Returns an <code>Either</code>: a <code>Right</code> containing the <code>Black</code> value, if this is a <code>Black</code>; or a <code>Left</code>
   * containing the <code>White</code> value, if this is a <code>White</code>.
   *
   * <p>
   * Note that values effectively &ldquo;switch sides&rdquo; when converting an <code>Ebony</code> to an <code>Either</code>. If the type of the
   * <code>Ebony</code> on which you invoke <code>toEither</code> is <code>Ebony[Int, Double]</code>, for example, the result will be an
   * <code>Either[Double, Int]</code>. The reason is that the convention for <code>Either</code> is that <code>Left</code> is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Right</code> is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, the <code>Black</code> value wrapped in a <code>Right</code>, else the
   *         underlying <code>White</code> value, wrapped in a <code>Left</code>.
   */
  def toEither: Either[W, B] =
    thisEbony.value match {
      case Black(b) => Right(b)
      case White(w) => Left(w)
    }

  /**
   * Returns an <code>Or</code>: a <code>Good</code> containing the <code>Black</code> value, if this is a <code>Black</code>; or a <code>Bad</code>
   * containing the <code>White</code> value, if this is a <code>White</code>.
   *
   * <p>
   * Note that values effectively &ldquo;stay on the same sides&rdquo; when converting an <code>Ivory</code> to an <code>Or</code>. If the type of the
   * <code>Ivory</code> on which you invoke <code>toOr</code> is <code>Ivory[Double, Int]</code>, for example, the result will be an
   * <code>Or[Double, Int]</code>. The reason is that the convention for <code>Or</code> is that <code>Bad</code> (the right side) is used for &ldquo;unexpected&rdquo;
   * or &ldquo;error&rdquo; values and <code>Good</code> (the left side) is used for &ldquo;expected&rdquo; or &ldquo;successful&rdquo; ones.
   * </p>
   *
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, the <code>Black</code> value wrapped in a <code>Good</code>, else the
   *         underlying <code>White</code> value, wrapped in a <code>Bad</code>.
   */
  def toOr: B Or W =
    thisEbony.value match {
      case Black(b) => Good(b)
      case White(w) => Bad(w)
    }

  /**
   * Returns a <code>Try</code>: a <code>Success</code> containing the
   * <code>Black</code> value, if this is a <code>Black</code>; or a <code>Failure</code>
   * containing the <code>White</code> value, if this is a <code>White</code>.
   *
   * <p>
   * Note: This method can only be called if the <code>White</code> type of this <code>Ebony</code> is a subclass
   * of <code>Throwable</code> (including <code>Throwable</code> itself).
   * </p>
   *
   * @return the underlying <code>Black</code> value, wrapped in a <code>Success</code>, if the underlying <code>Otherwise</code> is a <code>Black</code>; else
   * the underlying <code>White</code> value, wrapped in a <code>Failure</code>.
   */
  def toTry(implicit ev: W <:< Throwable): Try[B] =
    thisEbony.value match {
      case Black(b) => Success(b)
      case White(w) => Failure(w)
    }

  /**
   * Returns a <code>Ebony</code> with the <code>Black</code> and <code>White</code> types swapped: <code>White</code> becomes <code>Black</code> and <code>Black</code>
   * becomes <code>White</code>.
   *
   * @return if the underlying <code>Otherwise</code> is a <code>Black</code>, its <code>Black</code> value wrapped in a <code>White</code> then wrapped in an
   *     <code>Ebony</code>; if the underlying <code>Otherwise</code> is
   *     a <code>White</code>, its <code>White</code> value wrapped in a <code>Black</code> then wrapped in an <code>Ebony</code>.
   */
  def swap: Ebony[W, B] =
    thisEbony.value match {
      case Black(b) => new Ebony(White(b))
      case White(w) => new Ebony(Black(w))
    }

  /**
   * Transforms this <code>Ebony</code> by applying the function <code>bf</code> to the underlying <code>Otherwise</code>'s <code>Black</code> value if it is a <code>Black</code>,
   * or by applying <code>wf</code> to the underlying <code>Otherwise</code>'s <code>White</code> value if it is a <code>White</code>.
   *
   * @param bf the function to apply to the <code>Ebony</code>'s underlying <code>Black</code> value, if it is a <code>Black</code>
   * @param wf the function to apply to the <code>Ebony</code>'s underlying <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Otherwise</code>'s value
   */
  def transform[C, X](bf: B => Ebony[C, X], wf: W => Ebony[C, X]): Ebony[C, X] =
    thisEbony.value match {
      case Black(b) => bf(b)
      case White(w) => wf(w)
    }

  /**
   * Folds this <code>Ebony</code> into a value of type <code>V</code> by applying the given <code>bf</code> function if this is
   * a <code>Black</code> else the given <code>wf</code> function if this is a <code>White</code>.
   *
   * @param bf the function to apply to the underlying <code>Otherwise</code>'s <code>Black</code> value, if it is a <code>Black</code>
   * @param wf the function to apply to the underlying <code>Otherwise</code>'s <code>White</code> value, if it is a <code>White</code>
   * @return the result of applying the appropriate one of the two passed functions, <code>bf</code> or </code>wf</code>, to the underlying <code>Otherwise</code>'s value
   */
  def fold[V](bf: B => V, wf: W => V): V =
    thisEbony.value match {
      case Black(b) => bf(b)
      case White(w) => wf(w)
    }

  /**
   * A string representation for this <code>Ebony</code>.
   */
  override def toString = s"Ebony($thisEbony.value)"
}

