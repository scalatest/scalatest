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
package org.scalatest.prop

import org.scalactic.anyvals.PosZInt

/**
  * This trait is mixed in to [[Generator]]s that have a well-defined notion of "size".
  *
  * Broadly speaking, this applies when [[T]] is a type that has a `size` method. For
  * example, [[Generator.setGenerator]] (also known as [[CommonGenerators.sets]]) has the
  * [[HavingSize]] trait because [[Set]] has a `size` method.
  *
  * Generators with this trait provide several functions that allow you to create
  * more-specialized Generators, with specific size bounds.
  *
  * @tparam T the type that this [[Generator]] produces
  */
trait HavingSize[T] {
  /**
    * Create a version of this [[Generator]] that produces values of exactly the specified
    * size.
    *
    * For example, consider:
    * {{{
    *   val stringSets: Generator[Set[String]] =
    *     Generator.setGenerator[String]
    *
    *   val singleStringSets: Generator[Set[String]] =
    *     stringSets.havingSize(1)
    * }}}
    * The `singleStringSets` [[Generator]] will always produce [[Set]]s of exactly one
    * [[String]].
    *
    * @param len the size of the values to produce
    * @return a new [[Generator]] that produces values of that size
    */
  def havingSize(len: PosZInt): Generator[T]

  /**
    * Create a version of this [[Generator]] that produces values within the specified
    * size range.
    *
    * The bounds are inclusive: the resulting [[Generator]] may produce values with a
    * size of `to` or `from`.
    *
    * For example, consider:
    * {{{
    *   val stringSets: Generator[Set[String]] =
    *     Generator.setGenerator[String]
    *
    *   val smallStringSets: Generator[Set[String]] =
    *     stringSets.havingSizesBetween(0, 3)
    * }}}
    * The `smallStringSets` [[Generator]] will always produce [[Set]]s of zero through
    * three [[String]]s.
    *
    * @param from The smallest desired size to produce
    * @param to The largest desired size to produce
    * @return a [[Generator]] that will only produce values within that size range
    */
  def havingSizesBetween(from: PosZInt, to: PosZInt): Generator[T]

  /**
    * Create a version of this [[Generator]] whose legal sizes are adjusted by a
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
    *   val sizelimitedLists = lists[Int].havingSizesDeterminedBy(limitedSize)
    * }}}
    * This doesn't hard-code the size of the `sizeLimitedLists` [[Generator]], but
    * allows the maximum size to be clamped based on a dynamic `currentLimit` function.
    *
    * @param f a transformation function that takes the current [[SizeParam]] and returns a new one
    * @return a [[Generator]] that will base its sizes on the given function
    */
  def havingSizesDeterminedBy(f: SizeParam => SizeParam): Generator[T]
}

