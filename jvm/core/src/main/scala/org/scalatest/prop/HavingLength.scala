/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalactic.anyvals.PosZInt
import org.scalatest.Resources

/**
  * This trait is mixed in to [[Generator]]s that have a well-defined notion of "length".
  *
  * Broadly speaking, this applies when [[T]] is a type that has a `length` method. For
  * example, [[Generator.listGenerator]] (also known as [[CommonGenerators.lists]]) has the
  * [[HavingLength]] trait because [[List]] has a `length` method.
  *
  * Generators with this trait provide several functions that allow you to create
  * more-specialized Generators, with specific length bounds.
  *
  * Note that this trait extends [[HavingSize]], and is quite similar to it, reflecting
  * the relationship between the `length` and `size` methods in many standard library
  * types. The functions in here are basically just a shell around those in [[HavingSize]].
  *
  * @tparam T the type that this [[Generator]] produces
  */
trait HavingLength[T] extends HavingSize[T] {
  /**
    * Create a version of this [[Generator]] that produces values of exactly the specified
    * length.
    *
    * For example, consider:
    * {{{
    *   val stringLists: Generator[List[String]] =
    *     Generator.ListGenerator[String]
    *
    *   val singleStringLists: Generator[List[String]] =
    *     stringLists.havingSize(1)
    * }}}
    * The `singleStringLists` [[Generator]] will always produce [[List]]s of exactly one
    * [[String]].
    *
    * @param len the length of the values to produce
    * @return a new [[Generator]] that produces values of that length
    */
  final def havingLength(len: PosZInt): Generator[T] = havingSize(len)

  /**
    * Create a version of this [[Generator]] that produces values within the specified
    * length range.
    *
    * The bounds are inclusive: the resulting [[Generator]] may produce values with a
    * length of `to` or `from`.
    *
    * For example, consider:
    * {{{
    *   val stringLists: Generator[List[String]] =
    *     Generator.ListGenerator[String]
    *
    *   val smallStringLists: Generator[List[String]] =
    *     stringLists.havingLengthsBetween(0, 3)
    * }}}
    * The `smallStringLists` [[Generator]] will always produce [[List]]s of zero through
    * three [[String]]s.
    *
    * The `from` parameter must be smaller than the `to` parameter, and may not be equal
    * to it. If you want a [[Generator]] with a single specific length, use
    * [[havingLength()]] instead.
    *
    * @param from The smallest desired size to produce
    * @param to The largest desired size to produce
    * @return a [[Generator]] that will only produce values within that length range
    */
  final def havingLengthsBetween(from: PosZInt, to: PosZInt): Generator[T] = {
    require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
    require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
    havingSizesBetween(from, to)
  }

  /**
    * Create a version of this [[Generator]] whose legal lengths are adjusted by a
    * specified function.
    *
    * For example:
    * {{{
    *   def currentLimit: PosZInt = ...
    *
    *   def limitedSize(szp: SizeParam): SizeParam = {
    *     val limit = currentLimit
    *     val sz = if (szp.maxSize < limit) szp.maxSize else limit
    *     szp.copy(size = sz)
    *   }
    *
    *   val sizelimitedLists = lists[Int].havingLengthsDeterminedBy(limitedSize)
    * }}}
    * This doesn't hard-code the length of the `sizeLimitedLists` [[Generator]], but
    * allows the maximum length to be clamped based on a dynamic `currentLimit` function.
    *
    * @param f a transformation function that takes the current [[SizeParam]] and returns a new one
    * @return a [[Generator]] that will base its lengths on the given function
    */
  final def havingLengthsDeterminedBy(f: SizeParam => SizeParam): Generator[T] = havingSizesDeterminedBy(f)
}
