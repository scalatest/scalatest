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
package org.scalautils

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.util.control.NonFatal
import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

/**
 * Represents an value of one of two possible types, with one type being &ldquo;good&rdquo; and the other &ldquo;bad.&rdquo;
 *
 * <p>
 * An <code>Or</code> will either be a &ldquo;good&rdquo; value wrapped in an instance of <a href="Good.html"><code>Good</code></a>
 * or a &ldquo;bad&rdquo; value wrapped in an instance of <a href="Bad.html"><code>Bad</code></a>.
 * </p>
 *
 * <h2>An <code>Either</code> with &ldquo;attitude&rdquo;</h2>
 *
 * <p>
 * <code>Or</code> differs from Scala's <code>Either</code> type in that
 * <code>Either</code> treats its <code>Left</code> and <code>Right</code> alternatives equally, whereas <code>Or</code> declares one
 * alternative to be <code>Good</code> and the other <code>Bad</code>.
 * </p>
 * 
 * <pre class="stHighlight">
 * <pre>
 * 
 * <p>
 * Because of this, it is more convenient to work with <code>Or</code>s
 * when you prefer one alternative over the other. For example,  alternative represents a valid result and another represents an error.
 * </p>
 */
sealed abstract class Or[+G,+B] {
  val isGood: Boolean = false
  val isBad: Boolean = false
  def get: G
  def map[H](f: G => H): H Or B
  def foreach(f: G => Unit): Unit
  def flatMap[H, C >: B](f: G => H Or C): H Or C
  def filter(f: G => Boolean): Option[G Or B]
  def exists(f: G => Boolean): Boolean
  def forall(f: G => Boolean): Boolean
  def getOrElse[H >: G](default: H): H
  def toOption: Option[G]
  def toSeq: Seq[G]
  def toEither: Either[B, G]
  def accumulating: G Or One[B]
  def toTry(implicit ev: B <:< Throwable): Try[G]
  def swap: B Or G
  def zip[H, ERR, EVERY[b] <: Every[b]](other: H Or EVERY[ERR])(implicit ev: B <:< Every[ERR]): (G, H) Or Every[ERR]
  def transform[H, ERR, EVERY[b] <: Every[b]](other: (G => H) Or EVERY[ERR])(implicit ev: B <:< Every[ERR]): H Or Every[ERR]
  def validate[ERR](validations: (G => Option[ERR])*)(implicit ev: B <:< Every[ERR]): G Or Every[ERR]
}

object Or {
  def from[G](theTry: Try[G]): G Or Throwable =
    theTry match {
      case Success(g) => Good(g)
      case Failure(e) => Bad(e)
    }
  def from[B, G](either: Either[B, G]): G Or B =
    either match {
      case Right(g) => Good(g)
      case Left(e) => Bad(e)
    }

/*
  def combine[G, ERR, COLL[_]](combinable: Combinable[G, ERR, COLL]): COLL[G] Or Every[ERR] = combinable.combined
  def validateBy[G, ERR, EVERY[e] <: Every[e], COLL[_]](validatable: Validatable[G, COLL])(fn: G => G Or EVERY[ERR]): COLL[G] Or Every[ERR] = 
    validatable match {
      case basic: BasicValidatable => basic.validatedBy(fn)
*/
}

final case class Good[+G,+B](g: G) extends Or[G,B] {
  override val isGood: Boolean = true
  def orBad[C](implicit ev: B <:< C): Good[G, C] = this.asInstanceOf[Good[G, C]]
  def get: G = g
  def map[H](f: G => H): Or[H, B] = Good(f(g))
  def foreach(f: G => Unit): Unit = f(g)
  def flatMap[H, C >: B](f: G => H Or C): H Or C = f(g)
  def filter(f: G => Boolean): Option[G Or B] = if (f(g)) Some(this) else None
  def exists(f: G => Boolean): Boolean = f(g)
  def forall(f: G => Boolean): Boolean = f(g)
  def getOrElse[H >: G](default: H): G = g
  def toOption: Some[G] = Some(g)
  def toSeq: Seq[G] = Seq(g)
  def toEither: Either[B, G] = Right(g)
  def accumulating: G Or One[B] = Good(g)
  def toTry(implicit ev: B <:< Throwable): Success[G] = Success(g)
  def swap: B Or G = Bad(g)
  def zip[H, ERR, EVERY[b] <: Every[b]](other: H Or EVERY[ERR])(implicit ev: B <:< Every[ERR]): (G, H) Or Every[ERR] =
    other match {
      case Good(h) => Good((g, h))
      case Bad(otherB) => Bad(otherB)
    }
  def transform[H, ERR, EVERY[b] <: Every[b]](other: (G => H) Or EVERY[ERR])(implicit ev: B <:< Every[ERR]): H Or Every[ERR] =
    other match {
      case Good(f) => Good(f(g))
      case Bad(otherB) => Bad(otherB)
    }
  def validate[ERR](validations: (G => Option[ERR])*)(implicit ev: B <:< Every[ERR]): G Or Every[ERR] = {
    val results = validations flatMap (_(g).toSeq)
    results.length match {
      case 0 => Good(g)
      case 1 => Bad(One(results.head))
      case _ =>
        val first = results.head
        val tail = results.tail
        val second = tail.head
        val rest = tail.tail
        Bad(Many(first, second, rest: _*))
    }
  }
}

object Good {
  class GoodieGoodieGumdrop[G] {
    def orBad[B](b: B): Bad[G, B] = Bad[G, B](b)
    override def toString: String = "GoodieGoodieGumdrop"
  }
  def apply[G]: GoodieGoodieGumdrop[G] = new GoodieGoodieGumdrop[G]
}

final case class Bad[+G,+B](b: B) extends Or[G,B] {
  override val isBad: Boolean = true
  def get: G = throw new NoSuchElementException("Bad(" + b + ").get")
  def map[H](f: G => H): H Or B = Bad(b)
  def foreach(f: G => Unit): Unit = ()
  def flatMap[H, C >: B](f: G => H Or C): H Or C = Bad(b)
  def filter(f: G => Boolean): None.type = None
  def exists(f: G => Boolean): Boolean = false
  def forall(f: G => Boolean): Boolean = true
  def getOrElse[H >: G](default: H): H = default
  def toOption: None.type = None
  def toSeq: Seq[G] = Seq.empty
  def toEither: Either[B, G] = Left(b)
  def accumulating: G Or One[B] = Bad(One(b))
  def toTry(implicit ev: B <:< Throwable): Failure[G] = Failure(b)
  def swap: B Or G = Good(b)
  def zip[H, ERR, EVERY[b] <: Every[b]](other: H Or EVERY[ERR])(implicit ev: B <:< Every[ERR]): (G, H) Or Every[ERR] =
    other match {
      case Good(_) => Bad(ev(b))
      case Bad(otherB) => Bad(ev(b) ++ otherB)
    }
  def transform[H, ERR, EVERY[b] <: Every[b]](other: (G => H) Or EVERY[ERR])(implicit ev: B <:< Every[ERR]): H Or Every[ERR] =
    other match {
      case Good(_) => Bad(ev(b))
      case Bad(otherB) => Bad(ev(b) ++ otherB)
    }
  def validate[ERR](validations: (G => Option[ERR])*)(implicit ev: B <:< Every[ERR]): G Or Every[ERR] = Bad(b)
}

