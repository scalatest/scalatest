/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.prop

import org.scalactic.anyvals._
import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

trait CommonGenerators {

  // bytesBetween
  // shortsBetween

  def intsBetween(from: Int, to: Int): Generator[Int] =
    new Generator[Int] { thisIntGenerator =>
      private val fromToEdges = List(from, to).distinct // distinct in case from equals to
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[Int], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[Int], rnd: Randomizer): (Int, List[Int], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(from, to)
            (nextInt, Nil, nextRandomizer)
        }
      }
    }

  // longsBetween
  // charsBetween

  def posIntsBetween(from: PosInt, to: PosInt): Generator[PosInt] =
    new Generator[PosInt] { thisPosIntGenerator =>
      private val fromToEdges = List(from, to).distinct // distinct in case from equals to
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosInt], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosInt], rnd: Randomizer): (PosInt, List[PosInt], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosInt, nextRandomizer) = rnd.choosePosInt(from, to)
            (nextPosInt, Nil, nextRandomizer)
        }
      }
    }
  def posZIntsBetween(from: PosZInt, to: PosZInt): Generator[PosZInt] =
    // Probably disallow from >= to, and if =, then say use some alternative? constantValues(x) ?
    new Generator[PosZInt] { thisPosZIntGenerator =>
      private val fromToEdges = List(from, to).distinct // distinct in case from equals to
      override def initEdges(maxLength: Int, rnd: Randomizer): (List[PosZInt], Randomizer) = {
        require(maxLength >= 0, "; the maxLength passed to next must be >= 0")
        val (allEdges, nextRnd) = Randomizer.shuffle(fromToEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(size: Int, edges: List[PosZInt], rnd: Randomizer): (PosZInt, List[PosZInt], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail => (head, tail, rnd)
          case _ =>
            val (nextPosZInt, nextRandomizer) = rnd.choosePosZInt(from, to)
            (nextPosZInt, Nil, nextRandomizer)
        }
      }
    }

  // posZIntsBetween
  // posLongsBetween
  // posZLongsBetween
  // posFloatsBetween
  // posZFloatsBetween
  // posDoublesBetween
  // posZDoublesBetween

  def specificValues[T](first: T, second: T, rest: T*): Generator[T] =
    new Generator[T] {
      private val seq: Seq[T] = first +: second +: rest
      def next(size: Int, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(0, seq.length - 1)
            val nextT = seq(nextInt)
            (nextT, Nil, nextRandomizer)
        }
      }
    }

  def specificValue[T](theValue: T): Generator[T] =
    new Generator[T] {
      def next(size: Int, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            (theValue, Nil, rnd)
        }
      }
    }

  // TODO: I wonder if I could get rid of the edges pattern match
  // by moving this to a different method. Then next is just next
  // in the distributed stuff. I could then do the pattern match
  // once and forall in a final method, nextEdge.
  def frequency[T](first: (Int, Generator[T]), second: (Int, Generator[T]), rest: (Int, Generator[T])*): Generator[T] = {
      val distribution: Vector[(Int, Generator[T])] = (first +: second +: rest).toVector
    // Take Int not PosInt, because Scala won't apply  multiple implicit
    // conversions, such as one for PosInt => Int, and another for Int => Generator[Int].
    // So just do a require.
/*
    TODO:

     org.scalactic.Requirements.require {
       distribution forall { case (w, _) => w >= 1 }
     }

[error] /Users/bv/nobkp/delus/st-algebra-and-laws-2/scalatest/src/main/scala/org/scalatest/prop/package.scala:154: exception during macro expansion: 
[error] scala.reflect.macros.TypecheckException: not found: value requirementsHelper
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$typecheck$2$$anonfun$apply$1.apply(Typers.scala:34)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$typecheck$2$$anonfun$apply$1.apply(Typers.scala:28)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$3.apply(Typers.scala:24)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$3.apply(Typers.scala:24)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$withContext$1$1.apply(Typers.scala:25)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$withContext$1$1.apply(Typers.scala:25)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$1.apply(Typers.scala:23)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$1.apply(Typers.scala:23)
[error] 	at scala.reflect.macros.contexts.Typers$class.withContext$1(Typers.scala:25)
[error] 	at scala.reflect.macros.contexts.Typers$$anonfun$typecheck$2.apply(Typers.scala:28)

*/
    // I think we actually need to say org.scalactic.Requirements.requirementsHelper in the thing not requirementsHelper
    // Oh, maybe that won't work. Anyway, see what's up.
    import org.scalactic.Requirements._
    require {
      distribution forall { case (w, _) => w >= 1 }
    }
    new Generator[T] {
      private val totalWeight: Int = distribution.toMap.keys.sum
      // gens contains, for each distribution pair, weight generators.
      private val gens: Vector[Generator[T]] =
        distribution.toVector flatMap { case (w, g) =>
          Vector.fill(w)(g)
        }
      def next(size: Int, edges: List[T], rnd: Randomizer): (T, List[T], Randomizer) = {
        require(size >= 0, "; the size passed to next must be >= 0")
        edges match {
          case head :: tail =>
            (head, tail, rnd)
          case _ =>
            val (nextInt, nextRandomizer) = rnd.chooseInt(0, gens.length - 1)
            val nextGen = gens(nextInt)
            nextGen.next(size, Nil, nextRandomizer)
        }
      }
    }
  }

  val bytes: Generator[Byte] = Generator.byteGenerator
  val shorts: Generator[Short] = Generator.shortGenerator
  val ints: Generator[Int] = Generator.intGenerator
  val longs: Generator[Long] = Generator.longGenerator
  val chars: Generator[Char] = Generator.charGenerator
  val floats: Generator[Float] = Generator.floatGenerator
  val doubles: Generator[Double] = Generator.doubleGenerator
  val strings: Generator[String] = Generator.stringGenerator
  def lists[T](implicit genOfT: Generator[T]): Generator[List[T]] with HavingLength[List[T]] = Generator.listGenerator[T]
  def tuple2s[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] = Generator.tuple2Generator[A, B]
  def tuple3s[A, B, C](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[(A, B, C)] = Generator.tuple3Generator[A, B, C]
  def tuple4s[A, B, C, D](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[(A, B, C, D)] = Generator.tuple4Generator[A, B, C, D]
  def tuple5s[A, B, C, D, E](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[(A, B, C, D, E)] = Generator.tuple5Generator[A, B, C, D, E]
  def tuple6s[A, B, C, D, E, F](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[(A, B, C, D, E, F)] = Generator.tuple6Generator[A, B, C, D, E, F]
  def tuple7s[A, B, C, D, E, F, G](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G]): Generator[(A, B, C, D, E, F, G)] = Generator.tuple7Generator[A, B, C, D, E, F, G]
  def tuple8s[A, B, C, D, E, F, G, H](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H]): Generator[(A, B, C, D, E, F, G, H)] = Generator.tuple8Generator[A, B, C, D, E, F, G, H]
  def tuple9s[A, B, C, D, E, F, G, H, I](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[(A, B, C, D, E, F, G, H, I)] = Generator.tuple9Generator[A, B, C, D, E, F, G, H, I]
  def tuple10s[A, B, C, D, E, F, G, H, I, J](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                             genOfJ: Generator[J]): Generator[(A, B, C, D, E, F, G, H, I, J)] = Generator.tuple10Generator[A, B, C, D, E, F, G, H, I, J]
  def tuple11s[A, B, C, D, E, F, G, H, I, J, K](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                             genOfJ: Generator[J], genOfK: Generator[K]): Generator[(A, B, C, D, E, F, G, H, I, J, K)] = Generator.tuple11Generator[A, B, C, D, E, F, G, H, I, J, K]
  def tuple12s[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L)] = Generator.tuple12Generator[A, B, C, D, E, F, G, H, I, J, K, L]
  def tuple13s[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                   genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = Generator.tuple13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M]
  def tuple14s[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                      genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = Generator.tuple14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
  def tuple15s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                         genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = Generator.tuple15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
  def tuple16s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                            genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = Generator.tuple16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
  def tuple17s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                               genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = Generator.tuple17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
  def tuple18s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                  genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = Generator.tuple18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
  def tuple19s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                     genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = Generator.tuple19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  def tuple20s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                           genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                           genOfT: Generator[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = Generator.tuple20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  def tuple21s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                           genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                           genOfT: Generator[T], genOfU: Generator[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = Generator.tuple21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  def tuple22s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                              genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                              genOfT: Generator[T], genOfU: Generator[U], genOfV: Generator[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = Generator.tuple22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
  def function0s[A](implicit genOfA: Generator[A]): Generator[() => A] = Generator.function0Generator[A]
  def function1s[A, B](implicit genOfB: Generator[B], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B]): Generator[A => B] =
    Generator.function1Generator[A, B]
  def function2s[A, B, C](implicit genOfC: Generator[C], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C]): Generator[(A, B) => C] =
    Generator.function2Generator[A, B, C]
  def function3s[A, B, C, D](implicit genOfD: Generator[D], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D]): Generator[(A, B, C) => D] =
    Generator.function3Generator[A, B, C, D]
  def function4s[A, B, C, D, E](implicit genOfE: Generator[E], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E]): Generator[(A, B, C, D) => E] =
    Generator.function4Generator[A, B, C, D, E]
  def function5s[A, B, C, D, E, F](implicit genOfF: Generator[F], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F]): Generator[(A, B, C, D, E) => F] =
    Generator.function5Generator[A, B, C, D, E, F]
  def function6s[A, B, C, D, E, F, G](implicit genOfG: Generator[G], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G]): Generator[(A, B, C, D, E, F) => G] =
    Generator.function6Generator[A, B, C, D, E, F, G]
  def function7s[A, B, C, D, E, F, G, H](implicit genOfH: Generator[H], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H]): Generator[(A, B, C, D, E, F, G) => H] =
    Generator.function7Generator[A, B, C, D, E, F, G, H]
  def function8s[A, B, C, D, E, F, G, H, I](implicit genOfI: Generator[I], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I]): Generator[(A, B, C, D, E, F, G, H) => I] =
    Generator.function8Generator[A, B, C, D, E, F, G, H, I]
  def function9s[A, B, C, D, E, F, G, H, I, J](implicit genOfJ: Generator[J], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J]): Generator[(A, B, C, D, E, F, G, H, I) => J] =
    Generator.function9Generator[A, B, C, D, E, F, G, H, I, J]
  def function10s[A, B, C, D, E, F, G, H, I, J, K](implicit genOfK: Generator[K], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K]): Generator[(A, B, C, D, E, F, G, H, I, J) => K] =
    Generator.function10Generator[A, B, C, D, E, F, G, H, I, J, K]
  def function11s[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfL: Generator[L], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] =
    Generator.function11Generator[A, B, C, D, E, F, G, H, I, J, K, L]
  def function12s[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfM: Generator[M], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] =
    Generator.function12Generator[A, B, C, D, E, F, G, H, I, J, K, L, M]
  def function13s[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfN: Generator[N], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] =
    Generator.function13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N]
  def function14s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfO: Generator[O], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] =
    Generator.function14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]
  def function15s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfP: Generator[P], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] =
    Generator.function15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]
  def function16s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfQ: Generator[Q], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] =
    Generator.function16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]
  def function17s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfR: Generator[R], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] =
    Generator.function17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]
  def function18s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfS: Generator[S], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] =
    Generator.function18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]
  def function19s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfT: Generator[T], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] =
    Generator.function19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]
  def function20s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfU: Generator[U], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T], typeTagOfU: TypeTag[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] =
    Generator.function20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]
  def function21s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfV: Generator[V], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T], typeTagOfU: TypeTag[U], typeTagOfV: TypeTag[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] =
    Generator.function21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]
  def function22s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit genOfW: Generator[W], typeTagOfA: TypeTag[A], typeTagOfB: TypeTag[B], typeTagOfC: TypeTag[C], typeTagOfD: TypeTag[D], typeTagOfE: TypeTag[E], typeTagOfF: TypeTag[F], typeTagOfG: TypeTag[G], typeTagOfH: TypeTag[H], typeTagOfI: TypeTag[I], typeTagOfJ: TypeTag[J], typeTagOfK: TypeTag[K], typeTagOfL: TypeTag[L], typeTagOfM: TypeTag[M], typeTagOfN: TypeTag[N], typeTagOfO: TypeTag[O], typeTagOfP: TypeTag[P], typeTagOfQ: TypeTag[Q], typeTagOfR: TypeTag[R], typeTagOfS: TypeTag[S], typeTagOfT: TypeTag[T], typeTagOfU: TypeTag[U], typeTagOfV: TypeTag[V], typeTagOfW: TypeTag[W]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] =
    Generator.function22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]

  val posInts: Generator[PosInt] = Generator.posIntGenerator
  val posZInts: Generator[PosZInt] = Generator.posZIntGenerator
  val posLongs: Generator[PosLong] = Generator.posLongGenerator
  val posZLongs: Generator[PosZLong] = Generator.posZLongGenerator
  val posFloats: Generator[PosFloat] = Generator.posFloatGenerator
  val posZFloats: Generator[PosZFloat] = Generator.posZFloatGenerator
  val posDoubles: Generator[PosDouble] = Generator.posDoubleGenerator
  val posZDoubles: Generator[PosZDouble] = Generator.posZDoubleGenerator

  val posIntValues: Generator[Int] = Generator.posIntGenerator.map(_.value)
  val posZIntValues: Generator[Int] = Generator.posZIntGenerator.map(_.value)
  val posLongValues: Generator[Long] = Generator.posLongGenerator.map(_.value)
  val posZLongValues: Generator[Long] = Generator.posZLongGenerator.map(_.value)
  val posFloatValues: Generator[Float] = Generator.posFloatGenerator.map(_.value)
  val posZFloatValues: Generator[Float] = Generator.posZFloatGenerator.map(_.value)
  val posDoubleValues: Generator[Double] = Generator.posDoubleGenerator.map(_.value)
  val posZDoubleValues: Generator[Double] = Generator.posZDoubleGenerator.map(_.value)

  def gen[A, B](construct: A => B)(deconstruct: B => A)(implicit genOfA: Generator[A]): Generator[B] =
    new GeneratorFor1[A, B](construct, deconstruct)(genOfA)

  def gen[A, B, C](construct: (A, B) => C)(deconstruct: C => (A, B))(implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[C] =
    new GeneratorFor2[A, B, C](construct, deconstruct)(genOfA, genOfB)

  def gen[A, B, C, D](construct: (A, B, C) => D)(deconstruct: D => (A, B, C))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[D] =
    new GeneratorFor3[A, B, C, D](construct, deconstruct)(genOfA, genOfB, genOfC)

  def gen[A, B, C, D, E](construct: (A, B, C, D) => E)(deconstruct: E => (A, B, C, D))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[E] =
    new GeneratorFor4[A, B, C, D, E](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD)

  def gen[A, B, C, D, E, F](construct: (A, B, C, D, E) => F)(deconstruct: F => (A, B, C, D, E))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[F] =
    new GeneratorFor5[A, B, C, D, E, F](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE)

  def gen[A, B, C, D, E, F, G](construct: (A, B, C, D, E, F) => G)(deconstruct: G => (A, B, C, D, E, F))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[G] =
    new GeneratorFor6[A, B, C, D, E, F, G](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF)

  def gen[A, B, C, D, E, F, G, H](construct: (A, B, C, D, E, F, G) => H)(deconstruct: H => (A, B, C, D, E, F, G))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                  genOfG: Generator[G]): Generator[H] =
    new GeneratorFor7[A, B, C, D, E, F, G, H](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG)

  def gen[A, B, C, D, E, F, G, H, I](construct: (A, B, C, D, E, F, G, H) => I)(deconstruct: I => (A, B, C, D, E, F, G, H))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                  genOfG: Generator[G], genOfH: Generator[H]): Generator[I] =
    new GeneratorFor8[A, B, C, D, E, F, G, H, I](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH)

  def gen[A, B, C, D, E, F, G, H, I, J](construct: (A, B, C, D, E, F, G, H, I) => J)(deconstruct: J => (A, B, C, D, E, F, G, H, I))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                           genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[J] =
    new GeneratorFor9[A, B, C, D, E, F, G, H, I, J](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI)

  def gen[A, B, C, D, E, F, G, H, I, J, K](construct: (A, B, C, D, E, F, G, H, I, J) => K)(deconstruct: K => (A, B, C, D, E, F, G, H, I, J))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                    genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J]): Generator[K] =
    new GeneratorFor10[A, B, C, D, E, F, G, H, I, J, K](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L](construct: (A, B, C, D, E, F, G, H, I, J, K) => L)(deconstruct: L => (A, B, C, D, E, F, G, H, I, J, K))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                             genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K]): Generator[L] =
    new GeneratorFor11[A, B, C, D, E, F, G, H, I, J, K, L](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M](construct: (A, B, C, D, E, F, G, H, I, J, K, L) => M)(deconstruct: M => (A, B, C, D, E, F, G, H, I, J, K, L))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                      genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[M] =
    new GeneratorFor12[A, B, C, D, E, F, G, H, I, J, K, L, M](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N)(deconstruct: N => (A, B, C, D, E, F, G, H, I, J, K, L, M))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                               genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[N] =
    new GeneratorFor13[A, B, C, D, E, F, G, H, I, J, K, L, M, N](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O)(deconstruct: O => (A, B, C, D, E, F, G, H, I, J, K, L, M, N))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                 genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                 genOfN: Generator[N]): Generator[O] =
    new GeneratorFor14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P)(deconstruct: P => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                 genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                 genOfN: Generator[N], genOfO: Generator[O]): Generator[P] =
    new GeneratorFor15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q)(deconstruct: Q => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                          genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                          genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[Q] =
    new GeneratorFor16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R)(deconstruct: R => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                   genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                   genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[R] =
    new GeneratorFor17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S)(deconstruct: S => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                            genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                            genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[S] =
    new GeneratorFor18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T)(deconstruct: T => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                     genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                     genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[T] =
    new GeneratorFor19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U)(deconstruct: U => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                              genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                              genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T]): Generator[U] =
    new GeneratorFor20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V)(deconstruct: V => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                       genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                       genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T],
                                                                                                                                                                                                                                       genOfU: Generator[U]): Generator[V] =
    new GeneratorFor21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU)

  def gen[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W)(deconstruct: W => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                                genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                                genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T],
                                                                                                                                                                                                                                                genOfU: Generator[U], genOfV: Generator[V]): Generator[W] =
    new GeneratorFor22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU, genOfV)

  // classify will need to use the same sizing algo as forAll, and same edges approach
  def classify[A](count: PosInt, genOfA: Generator[A])(pf: PartialFunction[A, String]): Classification = {

    val (initEdges, rnd1) = genOfA.initEdges(100, Randomizer.default())
    @tailrec
    def loop(currentCount: Int, edges: List[A], rnd: Randomizer, acc: Map[String, PosZInt]): Map[String, PosZInt] = {
      if (currentCount >= count) acc
      else {
        val (nextA, nextEdges, nextRnd) = genOfA.next(100, edges, rnd)
        if (pf.isDefinedAt(nextA)) {
          val category = pf(nextA)
          val prevTotal = acc.getOrElse(category, PosZInt(0))
          val nextAcc = acc + (category -> PosZInt.ensuringValid(prevTotal + 1))
          loop(currentCount + 1, nextEdges, nextRnd, nextAcc)
        }
        else {
          loop(currentCount + 1, nextEdges, nextRnd, acc)
        }
      }
    }
    val theMap = loop(0, initEdges, rnd1, Map.empty)
    Classification(count, theMap)
  }
}

object CommonGenerators extends CommonGenerators
