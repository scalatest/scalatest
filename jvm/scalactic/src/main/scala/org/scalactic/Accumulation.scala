/*
 * Copyright 2001-2025 Artima, Inc.
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

import scala.collection.GenSet
import org.scalactic.ColCompatHelper.IterableOnce
import scala.collection.generic.CanBuildFrom
import Accumulation.Accumulatable
import Accumulation.Combinable
import Accumulation.TravValidatable
import Accumulation.Validatable
import scala.collection.mutable.Builder
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

trait AccumulationLowPriorityImplicits {

  /**
    * Implicitly converts a <em>covariant</em> <code>IterableOnce</code> containing accumulating <code>Or</code>s to an instance of
    * <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
    * enables the <code>combined</code> method to be invoked on it.
    *
    * <p>
    * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section of the main documentation for class <code>Or</code>.
    * </p>
    */
  implicit def convertIterableOnceToCombinable[G, ERR, EVERY[b] <: Every[b], ITRONCE[+e] <: IterableOnce[e]](xs: ITRONCE[G Or EVERY[ERR]])(implicit cbf: CanBuildFrom[ITRONCE[G Or EVERY[ERR]], G, ITRONCE[G]]): Combinable[G, ERR, ITRONCE] =
    new Combinable[G, ERR, ITRONCE] {

      def combined: ITRONCE[G] Or Every[ERR] = {
        // So now I have an empty builder
        val emptyITRONCEOfGBuilder: Builder[G, ITRONCE[G]] = cbf(xs)
        // So now I want to foldLeft across my ITRONCE[G Or EVERY[ERR]], starting with an empty ITRONCEOfGBuilder, and each step along the way, I'll
        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        val tempOr: Builder[G, ITRONCE[G]] Or Every[ERR] =
        xs.foldLeft((Good(emptyITRONCEOfGBuilder): Builder[G, ITRONCE[G]] Or Every[ERR])) { (accumulator: Builder[G, ITRONCE[G]] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
          (accumulator, nextElem) match {
            case (Good(bldr), Good(ele)) => Good(bldr += ele)
            case (Good(_), Bad(err)) => Bad(err)
            case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
            case (Bad(errA), Good(_)) => Bad(errA)
          }
        }
        tempOr map (_.result())
      }
    }

  implicit def convertIterableOnceToCombinable3[E, ITRONCE[+e] <: IterableOnce[e]](xs: ITRONCE[Bad[Every[E]]]): Combinable[Nothing, E, ITRONCE] =
    new Combinable[Nothing, E, ITRONCE] {
      override def combined: Or[ITRONCE[Nothing], Every[E]] = {
        val either: Either[Every[E], ITRONCE[Nothing]] =
          if (xs.isEmpty)
            Right(Vector.empty[Nothing].asInstanceOf[ITRONCE[Nothing]])
          else {
            val i = xs.toIterable
            Left(
              (i.tail.foldLeft(i.head.b) { case (res, ele) =>
                res ++ ele.b
              }.asInstanceOf[Every[E]])
            )
          }

        Or.from(either)
      }
    }

}

/**
 * Provides mechanisms that enable errors to be accumulated in &ldquo;accumulating <a href="Or.html"><code>Or</code></a>s,&rdquo; <code>Or</code>s whose
 * <a href="Bad.html"><code>Bad</code></a> type is an <a href="Every.html"><code>Every</code></a>.
 *
 * <p>
 * The mechanisms are:
 * </p>
 *
 * <ul>
 * <li>Passing accumulating <code>Or</code>s to <code>withGood</code> methods</li>
 * <li>Invoking <code>combined</code> on a container of accumulating <code>Or</code>s</li>
 * <li>Invoking <code>validatedBy</code> on a container of any type, passing in a function from that type to an accumulating <code>Or</code></li>
 * <li>Invoking <code>zip</code> on an accumulating <code>Or</code></li>
 * <li>Invoking <code>when</code> on an accumulating <code>Or</code></li>
 * </ul>
 *
 * <p>
 * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
 * of the main documentation for class <code>Or</code>.
 * </p>
 */
trait Accumulation extends AccumulationLowPriorityImplicits {

  import scala.language.{higherKinds, implicitConversions}


  /**
   * Implicitly converts an accumulating <code>Or</code> to an instance of <a href="Accumulation$$Acumulatable.html"><code>Accumulatable</code></a>, which
   * enables <code>zip</code> and <code>when</code> methods to be invoked on it.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingZip">Using <code>zip</code></a> and <a href="Or.html#usingWhen">Using <code>when</code></a>
   * sections of the main documentation for class <code>Or</code>.
   * </p>
   */
  implicit def convertOrToAccumulatable[G, ERR, EVERY[b] <: Every[b]](accumulatable: G Or EVERY[ERR]): Accumulatable[G, ERR, EVERY] =
    new Accumulatable[G, ERR, EVERY] {
      def zip[H, OTHERERR >: ERR, OTHEREVERY[c] <: Every[c]](other: H Or OTHEREVERY[OTHERERR]): (G, H) Or Every[OTHERERR] = {
        accumulatable match {
          case Good(g) =>
            other match {
              case Good(h) => Good((g, h))
              case Bad(otherB) => Bad(otherB)
            }
          case Bad(myBad) =>
            other match {
              case Good(_) => Bad(myBad)
              case Bad(otherB) => Bad(myBad ++ otherB)
            }
        }
      }
      def when[OTHERERR >: ERR](validations: (G => Validation[OTHERERR])*): G Or Every[OTHERERR] = {
        accumulatable match {
          case Good(g) =>
            val results = validations flatMap (_(g) match { case Fail(x) => Seq(x); case Pass => Seq.empty})
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
          case Bad(myBad) => Bad(myBad)
        }
      }
    }

  implicit def convertIterableOnceToCombinable2[E, ITRONCE[+e] <: Iterable[e]](xs: ITRONCE[Good[E]])(implicit cbf: CanBuildFrom[ITRONCE[Good[E]], Good[E], ITRONCE[Good[E]]]): Combinable[E, Nothing, ITRONCE] =
    new Combinable[E, Nothing, ITRONCE] {
      override def combined: Or[ITRONCE[E], Every[Nothing]] = {
        // So now I have an empty builder
        val emptyITRONCEOfGBuilder: Builder[Good[E], ITRONCE[Good[E]]] = cbf(xs)
        Or.from(
          Right(
            (xs.foldLeft(emptyITRONCEOfGBuilder) { case (res, ele) =>
              res += ele
            }).mapResult(_.map(_.get)).result().asInstanceOf[ITRONCE[E]]
          )
        )
      }

    }

  // Must have another one for Sets, because they are not covariant. Will need to handle Good/Nothing case specially therefore, and plan to do that
  // with another implicit here. Or just don't support Nothing.

  /**
   * Implicitly converts a <code>Set</code> containing accumulating <code>Or</code>s to an instance of <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
   * enables the <code>combined</code> method to be invoked on it.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section of the main documentation for class <code>Or</code>.
   * </p>
   *
   * <p>
   * Note: This implicit is required for <code>Set</code>s because although <code>Set</code>s are <code>IterableOnce</code>s, they aren't covariant, so 
   * the implicit conversion provided by <code>convertIterableOnceToCombinable</code> will not be applied, because it only works on <em>covariant</em>
   * <code>IterableOnce</code>s.
   * </p>
   */
  implicit def convertGenSetToCombinable[G, ERR, X, EVERY[b] <: Every[b], SET[e] <: GenSet[e]](xs: SET[X with (G Or EVERY[ERR])])(implicit cbf: CanBuildFrom[SET[X with (G Or EVERY[ERR])], G, SET[G]]): Combinable[G, ERR, SET] = 

    new Combinable[G, ERR, SET] {

      def combined: SET[G] Or Every[ERR] = {
        // So now I have an empty builder
        val emptySETOfGBuilder: Builder[G, SET[G]] = cbf(xs)
        // So now I want to foldLeft across my SET[G Or EVERY[ERR]], starting with an empty SETOfGBuilder, and each step along the way, I'll
        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        val tempOr: Builder[G, SET[G]] Or Every[ERR] = 
          xs.foldLeft((Good(emptySETOfGBuilder): Builder[G, SET[G]] Or Every[ERR])) { (accumulator: Builder[G, SET[G]] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
            (accumulator, nextElem) match {
              case (Good(bldr), Good(ele)) => Good(bldr += ele)
              case (Good(_), Bad(err)) => Bad(err)
              case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
              case (Bad(errA), Good(_)) => Bad(errA)
            }
          }
        tempOr map (_.result())
      }
    }

  /**
   * Implicitly converts a <code>Set</code> containing accumulating <code>Or</code>s whose <code>Good</code> type is inferred as <code>Nothing</code> to an
   * instance of <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
   * enables the <code>combined</code> method to be invoked on it.
   *
   * <p>
   * Note: This implicit is required for <code>Set</code>s because although <code>Set</code>s are <code>IterableOnce</code>s, they aren't covariant, so 
   * the implicit conversion provided by <code>convertIterableOnceToCombinableNothing</code> will not be applied, because it only works on <em>covariant</em>
   * <code>IterableOnce</code>s.
   * </p>
   */
  implicit def convertGenSetOnceToCombinable2[E, SET[e] <: GenSet[e]](xs: SET[Good[E]])(implicit cbf: CanBuildFrom[SET[Good[E]], Good[E], SET[Good[E]]]): Combinable[E, Nothing, SET] =
    new Combinable[E, Nothing, SET] {
      override def combined: Or[SET[E], Every[Nothing]] = {
        // So now I have an empty builder
        val emptyITRONCEOfGBuilder: Builder[Good[E], SET[Good[E]]] = cbf(xs)
        Or.from(
          Right(
            (xs.foldLeft(emptyITRONCEOfGBuilder) { case (res, ele) =>
              res += ele
            }).mapResult(_.map(_.get)).result().asInstanceOf[SET[E]]
          )
        )
      }

    }

  /**
    * Implicitly converts a <code>Set</code> containing accumulating <code>Or</code>s whose <code>Bad</code> type is inferred as <code>Nothing</code> to an
    * instance of <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which
    * enables the <code>combined</code> method to be invoked on it.
    *
    * <p>
    * Note: This implicit is required for <code>Set</code>s because although <code>Set</code>s are <code>IterableOnce</code>s, they aren't covariant, so
    * the implicit conversion provided by <code>convertGenSetToCombinableNothing</code> will not be applied, because it only works on <em>covariant</em>
    * <code>IterableOnce</code>s.
    * </p>
    */
  implicit def convertGenSetOnceToCombinable3[E, SET[e] <: GenSet[e], EVERY[f] <: Every[f]](xs: SET[Bad[EVERY[E]]]): Combinable[Nothing, E, SET] =
    new Combinable[Nothing, E, SET] {
      override def combined: Or[SET[Nothing], EVERY[E]] = {
        val either: Either[EVERY[E], SET[Nothing]] =
          if (xs.isEmpty)
            Right(Vector.empty[Nothing].asInstanceOf[SET[Nothing]])
          else {
            val i = xs.toIterable
            Left(
              (i.tail.foldLeft(i.head.b) { case (res, ele) =>
                (res ++ ele.b).asInstanceOf[EVERY[E]]
              })
            )
          }

        Or.from(either)
      }
    }

  /**
   * Implicitly converts an <code>Every</code> containing accumulating <code>Or</code>s to an instance of
   * <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which enables the <code>combined</code> method to be invoked on it.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section of the main documentation for class <code>Or</code>.
   * </p>
   */
  implicit def convertEveryToCombinable[G, ERR](oneToMany: Every[G Or Every[ERR]]): Combinable[G, ERR, Every] = 

    new Combinable[G, ERR, Every] {

      def combined: Every[G] Or Every[ERR] = {
        val vec = oneToMany.toVector
        val z: Every[G] Or Every[ERR] =
          vec.head match {
            case Good(g) => Good(One(g))
            case Bad(err) => Bad(err)
          }
        vec.tail.foldLeft(z) { (accumulator: Every[G] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
          (accumulator, nextElem) match {
            case (Good(everyG), Good(g)) => Good(everyG :+ g)
            case (Good(_), Bad(err)) => Bad(err)
            case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
            case (Bad(errA), Good(_)) => Bad(errA)
          }
        }
      }
    }

  /**
   * Implicitly converts an <code>Option</code> containing accumulating <code>Or</code>s to an instance of
   * <a href="Accumulation$$Combinable.html"><code>Combinable</code></a>, which enables the <code>combined</code> method to be invoked on it.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section of the main documentation for class <code>Or</code>.
   * </p>
   */
  implicit def convertOptionToCombinable[G, ERR](option: Option[G Or Every[ERR]]): Combinable[G, ERR, Option] = 
    new Combinable[G, ERR, Option] {
      def combined: Option[G] Or Every[ERR] =
        option match {
          case Some(Good(g)) => Good(Some(g))
          case Some(Bad(err)) => Bad(err)
          case None => Good(None)
        }
    }

  /**
   * Implicitly converts a <code>IterableOnce</code> to an instance of <code>Validatable</code>, which
   * enables the <code>validatedBy</code> method to be invoked on it.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingValidatedBy">Using <code>validatedBy</code></a> section of
   * the main documentation for class <code>Or</code>.
   * </p>
   */
  implicit def convertIterableOnceToValidatable[G, ITRONCE[e] <: IterableOnce[e]](xs: ITRONCE[G]): TravValidatable[G, ITRONCE] = 

    new TravValidatable[G, ITRONCE] {

      def validatedBy[H, ERR, EVERY[e] <: Every[e]](fn: G => H Or EVERY[ERR])(implicit cbf: CanBuildFrom[ITRONCE[G], H, ITRONCE[H]]): ITRONCE[H] Or Every[ERR] = {

        // So now I have an empty builder
        val emptyITRONCEOfGBuilder: Builder[H, ITRONCE[H]] = cbf(xs)
        // So now I want to foldLeft across my ITRONCE[G Or EVERY[ERR]], starting with an empty ITRONCEOfGBuilder, and each step along the way, I'll
        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        val tempOr: Builder[H, ITRONCE[H]] Or Every[ERR] = 
          xs.foldLeft((Good(emptyITRONCEOfGBuilder): Builder[H, ITRONCE[H]] Or Every[ERR])) { (accumulator: Builder[H, ITRONCE[H]] Or Every[ERR],  nextElem: G) =>
            (accumulator, fn(nextElem)) match {
              case (Good(bldr), Good(ele)) => Good(bldr += ele)
              case (Good(bldr), Bad(err)) => Bad(err)
              case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
              case (Bad(errA), Good(ele)) => Bad(errA)
            }
          }
        tempOr map (_.result())
      }
    }

  /**
   * Implicitly converts an <code>Every</code> to an instance of <code>Validatable</code>, which
   * enables the <code>validatedBy</code> method to be invoked on it.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingValidatedBy">Using <code>validatedBy</code></a> section of
   * the main documentation for class <code>Or</code>.
   * </p>
   */
  implicit def convertEveryToValidatable[G](oneToMany: Every[G]): Validatable[G, Every] = 
    new Validatable[G, Every] {
      def validatedBy[H, ERR, EVERY[e] <: Every[e]](fn: G => H Or EVERY[ERR]): Every[H] Or Every[ERR] = {
        val vec = oneToMany.toVector
        val z: Every[H] Or Every[ERR] =
          fn(vec.head) match {
            case Good(h) => Good(One(h))
            case Bad(err) => Bad(err)
          }
        vec.tail.foldLeft(z) { (accumulator: Every[H] Or Every[ERR],  nextElem: G) =>
          (accumulator, fn(nextElem)) match {
            case (Good(everyG), Good(h)) => Good(everyG :+ h)
            case (Good(_), Bad(err)) => Bad(err)
            case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
            case (Bad(errA), Good(_)) => Bad(errA)
          }
        }
      }
    }

  /**
   * Implicitly converts an <code>Option</code> to an instance of <code>Validatable</code>, which
   * enables the <code>validatedBy</code> method to be invoked on it.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingValidatedBy">Using <code>validatedBy</code></a> section of
   * the main documentation for class <code>Or</code>.
   * </p>
   */
  implicit def convertOptionToValidatable[G](option: Option[G]): Validatable[G, Option] = 
    new Validatable[G, Option] {
      def validatedBy[H, ERR, EVERY[e] <: Every[e]](fn: G => H Or EVERY[ERR]): Option[H] Or Every[ERR] = {
        option.map(fn) match {
          case Some(Good(h)) => Good(Some(h))
          case Some(Bad(err)) => Bad(err)
          case None => Good(None)
        }
      }
    }

  /**
   * Given 2 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR]
  )(
    fn: (A, B) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b)(fn.curried)

  private def withGoodCurried[A, B, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR]
  )(
    fn: A => B => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (B => RESULT) Or Every[ERR] = a.map(fn) // withGood[A, ERR, B => RESULT](a)(fn)
    b match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) => 
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }

  /**
   * Given 3 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR]
  )(
    fn: (A, B, C) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c)(fn.curried)

  private def withGoodCurried[A, B, C, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR]
  )(
    fn: A => B => C => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (C => RESULT) Or Every[ERR] = withGoodCurried[A, B, ERR, C => RESULT](a, b)(fn)
    c match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) => 
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }

  /**
   * Given 4 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR]
  )(
    fn: (A, B, C, D) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d)(fn.curried)

  private def withGoodCurried[A, B, C, D, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR]
  )(
    fn: A => B => C => D => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (D => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, ERR, D => RESULT](a, b, c)(fn)
    d match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) => 
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }

  /**
   * Given 5 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR]
  )(
    fn: (A, B, C, D, E) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR]
  )(
    fn: A => B => C => D => E => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (E => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, ERR, E => RESULT](a, b, c, d)(fn)
    e match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 6 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (F => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, ERR, F => RESULT](a, b, c, d, e)(fn)
    f match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 7 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (G => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, ERR, G => RESULT](a, b, c, d, e, f)(fn)
    g match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 8 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (H => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, ERR, H => RESULT](a, b, c, d, e, f, g)(fn)
    h match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 9 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (I => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, ERR, I => RESULT](a, b, c, d, e, f, g, h)(fn)
    i match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 10 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (J => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, ERR, J => RESULT](a, b, c, d, e, f, g, h, i)(fn)
    j match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 11 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (K => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, ERR, K => RESULT](a, b, c, d, e, f, g, h, i, j)(fn)
    k match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 12 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (L => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, ERR, L => RESULT](a, b, c, d, e, f, g, h, i, j, k)(fn)
    l match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 13 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (M => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, ERR, M => RESULT](a, b, c, d, e, f, g, h, i, j, k, l)(fn)
    m match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 14 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (N => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, ERR, N => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m)(fn)
    n match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 15 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (O => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, ERR, O => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n)(fn)
    o match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 16 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (P => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, ERR, P => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)(fn)
    p match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 17 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (Q => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, ERR, Q => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)(fn)
    q match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 18 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (R => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, ERR, R => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)(fn)
    r match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 19 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (S => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, ERR, S => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)(fn)
    s match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 20 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => T => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (T => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, ERR, T => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)(fn)
    t match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 21 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => T => U => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (U => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, ERR, U => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)(fn)
    u match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }


  /**
   * Given 22 <code>Good</code> accumulating <code>Or</code>s, apply them to the given function and return the result, wrapped in a <code>Good</code>;
   * else return a <code>Bad</code> containing every error (<em>i.e.</em>, a <code>Bad</code> whose <code>Every</code> includes every value that
   * appears in any <code>Bad</code>s passed to <code>withGood</code>).
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   *
   * @return a <code>Good</code> result, if all passed <code>Or</code>s were <code>Good</code>; else a <code>Bad</code> containing every error.
   */
  def withGood[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR],
    v: V Or Every[ERR]
  )(
    fn: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => RESULT
  ): RESULT Or Every[ERR] = withGoodCurried(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)(fn.curried)

  private def withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, ERR, RESULT](
    a: A Or Every[ERR],
    b: B Or Every[ERR],
    c: C Or Every[ERR],
    d: D Or Every[ERR],
    e: E Or Every[ERR],
    f: F Or Every[ERR],
    g: G Or Every[ERR],
    h: H Or Every[ERR],
    i: I Or Every[ERR],
    j: J Or Every[ERR],
    k: K Or Every[ERR],
    l: L Or Every[ERR],
    m: M Or Every[ERR],
    n: N Or Every[ERR],
    o: O Or Every[ERR],
    p: P Or Every[ERR],
    q: Q Or Every[ERR],
    r: R Or Every[ERR],
    s: S Or Every[ERR],
    t: T Or Every[ERR],
    u: U Or Every[ERR],
    v: V Or Every[ERR]
  )(
    fn: A => B => C => D => E => F => G => H => I => J => K => L => M => N => O => P => Q => R => S => T => U => V => RESULT
  ): RESULT Or Every[ERR] = {
    val funOrError: (V => RESULT) Or Every[ERR] = withGoodCurried[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, ERR, V => RESULT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)(fn)
    v match {
      case Good(valid) =>
        funOrError match {
          case Good(validFun) => Good(validFun(valid))
          case Bad(every) => Bad(every)
        }
      case Bad(every) =>
        funOrError match {
          case Good(_) => Bad(every)
          case Bad(funEvery) => Bad(funEvery ++ every)
        }
    }
  }
}

/**
 * Companion object to trait <code>Accumulation</code> that allows <code>Accumulation</code>'s members to be imported
 * rather than mixed in, and also contains nested traits used by implicit conversions declared in
 * trait <code>Accumulations</code>.
 *
 * <p>
 * For more information and examples, see the <a href="Or.html#accumulatingErrors">Accumulating errors with <code>Or</code></a> section
 * of the main documentation for class <code>Or</code>.
 * </p>
 */
object Accumulation extends Accumulation {

  import scala.language.higherKinds

  /**
   * Adds a <code>combined</code> method to &ldquo;collections&rdquo; of accumulating <code>Or</code>s via an implicit conversion provided by
   * trait <a href="Accumulation.html"><code>Accumulation</code></a>.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section
   * of the main documentation for class <code>Or</code>.
   * </p>
   */
  trait Combinable[G, ERR, COLL[_]] {

    /**
     * Combines a collection <code>COLL</code> of <code>Or</code>s of type <code>G</code> <code>Or</code> <code>EVERY[ERR]</code> (where <code>EVERY</code>
     * is some subtype of <code>Every</code>) into a single <code>Or</code> of
     * type <code>COLL[G]</code> <code>Or</code> <code>Every[ERR]</code>.
     *
     * <p>
     * Note: this process implemented by this method is sometimes called a &ldquo;sequence.&rdquo;
     * </p>
     *
     * <p>
     * For more information and examples, see the <a href="Or.html#usingCombined">Using <code>combined</code></a> section
     * of the main documentation for class <code>Or</code>.
     * </p>
     */
    def combined: COLL[G] Or Every[ERR]
  }

  /**
   * Adds a <code>validatedBy</code> method to (non-<code>IterableOnce</code>) &ldquo;collections&rdquo; via an implicit conversion provided by
   * trait <a href="Accumulation.html"><code>Accumulation</code></a>.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingValidatedBy">Using <code>validatedBy</code></a> section of
   * the main documentation for class <code>Or</code>.
   * </p>
   */
  trait Validatable[G, COLL[_]] {

    /**
     * Maps a collection <code>COLL</code> of <code>G</code>s into <code>Or</code>s of type <code>H</code> <code>Or</code> <code>EVERY[ERR]</code> (where <code>EVERY</code>
     * is some subtype of <code>Every</code>) using the passed function <code>fn</code>, then combines the resulting <code>Or</code>s into a single <code>Or</code> of
     * type <code>COLL[H]</code> <code>Or</code> <code>Every[ERR]</code>.
     *
     * <p>
     * Note: this process implemented by this method is sometimes called a &ldquo;traverse.&rdquo;
     * </p>
     *
     * <p>
     * For more information and examples, see the <a href="Or.html#usingValidatedBy">Using <code>validatedBy</code></a> section of
     * the main documentation for class <code>Or</code>.
     * </p>
     */
    def validatedBy[H, ERR, EVERY[e] <: Every[e]](fn: G => H Or EVERY[ERR]): COLL[H] Or Every[ERR]
  }

  /**
   * Adds a <code>validatedBy</code> method to <code>IterableOnce</code> via an implicit conversion provided by
   * trait <a href="Accumulation.html"><code>Accumulation</code></a>.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingValidatedBy">Using <code>validatedBy</code></a> section of
   * the main documentation for class <code>Or</code>.
   * </p>
   */
  trait TravValidatable[G, ITRONCE[e] <: IterableOnce[e]] {

    /**
     * Maps a <code>IterableOnce</code> of <code>G</code>s into <code>Or</code>s of type <code>H</code> <code>Or</code> <code>EVERY[ERR]</code> (where <code>EVERY</code>
     * is some subtype of <code>Every</code>) using the passed function <code>fn</code>, then combines the resulting <code>Or</code>s into a single <code>Or</code> of
     * type <code>COLL[H]</code> <code>Or</code> <code>Every[ERR]</code>.
     *
     * <p>
     * Note: this process implemented by this method is sometimes called a &ldquo;traverse.&rdquo;
     * </p>
     *
     * <p>
     * For more information and examples, see the <a href="Or.html#usingValidatedBy">Using <code>validatedBy</code></a> section of
     * the main documentation for class <code>Or</code>.
     * </p>
     */
    def validatedBy[H, ERR, EVERY[e] <: Every[e]](fn: G => H Or EVERY[ERR])(implicit cbf: CanBuildFrom[ITRONCE[G], H, ITRONCE[H]]): ITRONCE[H] Or Every[ERR]
  }

  /**
   * Adds <code>zip</code> and <code>when</code> methods to <a href="Or.html"><code>Or</code></a>s vai an implicit conversion provided by
   * trait <a href="Accumulation.html"><code>Accumulation</code></a>.
   *
   * <p>
   * For more information and examples, see the <a href="Or.html#usingZip">Using <code>zip</code></a> and <a href="Or.html#usingWhen">Using <code>when</code></a>
   * sections of the main documentation for class <code>Or</code>.
   * </p>
   */
  trait Accumulatable[G, ERR, EVERY[b] <: Every[b]] {

    /**
     * Zips two accumulating <code>Or</code>s together into a <code>Good</code> pair (<code>Tuple2[G, H]</code>) if both <code>Or</code>s are
     * <code>Good</code>, else a <code>Bad</code> containing every error.
     *
     * <p>
     * For more information and examples, see the <a href="Or.html#usingZip">Using <code>zip</code></a> 
     * section of the main documentation for class <code>Or</code>.
     * </p>
     */
    def zip[H, OTHERERR >: ERR, OTHEREVERY[c] <: Every[c]](other: H Or OTHEREVERY[OTHERERR]): (G, H) Or Every[OTHERERR]

    /**
     * Given a <code>Good</code> accumulating <code>Or</code>, applies the given validation functions to the <code>Good</code> value and returns
     * either the same <code>Good</code>, if all validations resulted in <code>Pass</code>, else returns a <code>Bad</code> containing every
     * error reported by validation <code>Fail</code> results; Given a <code>Bad</code> accumualting <code>Or</code>, returns the same <code>Bad</code>.
     *
     * <p>
     * For more information and examples, see the <a href="Or.html#usingWhen">Using <code>when</code></a>
     * section of the main documentation for class <code>Or</code>.
     * </p>
     */
    def when[OTHERERR >: ERR](validations: (G => Validation[OTHERERR])*): G Or Every[OTHERERR]
  }
}

