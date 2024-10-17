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

import org.scalactic._
import org.scalactic.anyvals._

import scala.annotation.tailrec
import org.scalactic.source.TypeInfo
import org.scalactic.Requirements._
import org.scalatest.prop.Generator.function1Generator

import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap

import org.scalactic.ColCompatHelper.LazyListOrStream

/**
  * Provides various specialized [[Generator]]s that are often useful.
  *
  * This exists as both a trait that you can mix into your classes, and an object
  * that you can import -- choose whichever best suits your tests. However, you
  * usually should not need to pull this in directly, since it is already mixed into
  * both [[GeneratorDrivenPropertyChecks]] and [[TableDrivenPropertyChecks]].
  *
  * This incorporates the standard [[Generator]]s defined in the [[Generator]] object,
  * so you generally won't need both.
  *
  * @groupprio Specific 10
  * @groupname Specific Creating Generators from Specific Values
  * @groupdesc Specific These functions let you create [[Generator]]s that only return specific
  *            values
  *
  * @groupprio Higher 20
  * @groupname Higher Creating Higher-Order Generators from other Generators
  * @groupdesc Higher These functions let you create [[Generator]]s that are built from more
  *            than one existing [[Generator]].
  *
  * @groupprio Common 30
  * @groupname Common Generators for Many Common Types
  * @groupdesc Common These cover types from both the Scala Standard Library and Scalactic
  *
  * @groupprio Values 40
  * @groupname Values Generators that produce the values from Scalactic Types
  * @groupdesc Values Scalactic has many highly-precise numeric types such as [[NonZeroLong]],
  *            [[PosZFloat]] or [[FiniteDouble]]. These help ensure your code is using
  *            exactly the numbers you intend, and they are very convenient for using with
  *            [[Generator]]s. But if the code under test is ''not'' using Scalactic, you
  *            sometimes find that you need to type `.value` a lot. These Generators do that
  *            for you: you can choose a precise numeric Generator, but get the conventional
  *            numeric type from it.
  *
  * @groupprio Collections 50
  * @groupname Collections Generators for standard Collections
  * @groupdesc Collections These functions take one or more types `T`, and create [[Generator]]s that
  *            produce collections of `T`.
  *
  * @groupprio Betweeners 60
  * @groupname Betweeners Range-based Generator Creation
  * @groupdesc Betweeners Functions that create [[Generator]]s for values in a specific range
  *            of a specific type.
  *
  * @groupprio Functions 70
  * @groupname Functions Generators that Produce Functions
  * @groupdesc Functions These functions create [[Generator]]s that produce random functions with
  *            specified parameter and return types.
  *
  * @groupprio InstancesOf 80
  * @groupname InstancesOf Generators for instances of case classes
  * @groupdesc InstancesOf These functions are one way to create [[Generator]]s for case
  *            class instances.
  *
  * @groupprio Tools 90
  * @groupname Tools Tools for Developing Generators
  *
  * @group Others
  */
trait CommonGenerators {

  /////////////////////////////////////////////////
  //
  // Between-related functions
  //


  /**
    * Create a [[Generator]] that returns values in the specified range.
    *
    * This is the general-purpose function that underlies all other `xxsBetween()` functions in
    * CommonGenerators. It works with any type for which there is an [[Ordering]], a [[Generator]], and
    * a [[Chooser]], making it easy to create [[Generator]]s for ranges within that type.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they ''will'' usually be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`. (However "less than or equal"
    * is defined for this type.)
    *
    * The "edges" -- the edge case values -- for this type will be taken from the implicit
    * [[Generator]]. This function then filters out any that aren't within the specified range,
    * and adds the `from` and `to` values as edges.
    *
    * The implicit [[Chooser]] is used to pick random values of the type. That should do most of
    * the heavy lifting.
    *
    * Since this underlies the more-specific `xxsBetween()` functions, you may use either those
    * or this when appropriate. For example, this:
    * {{{
    *   intsBetween(1, 100)
    * }}}
    * and
    * {{{
    *   between(1, 100)
    * }}}
    * are functionally identical so long as the types of the parameters are clear to the compiler.
    * Use whichever suits your project's coding style better.
    *
    * @param from the lower bound of the range to choose from
    * @param to the upper bound of the range to choose from
    * @param ord an instance of `Ordering[T]`, which should usually be in implicit scope
    * @param chooser an instance of `Chooser[T]`, which should usually be in implicit scope
    * @param gen an instance of `Generator[T]`, which should usually be in implicit scope
    * @tparam T the type to choose a value from
    * @return a new [[Generator]], that produces values in the specified range
    *
    * @group Betweeners
    */
  def between[T](from: T, to: T)(implicit ord: Ordering[T], chooser: Chooser[T], gen: Generator[T]): Generator[T] = {
    import ord.mkOrderingOps
    require(from <= to)
    new Generator[T] {
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[T], Randomizer) = {
        // Start with the edges of the underlying generator:
        val (base, nextRnd) = gen.initEdges(maxLength, rnd)
        // Snip away anything out of range:
        val valueEdges = base.filter(i => i >= from && i <= to)
        // Add the boundaries as edges for our new filter:
        val fromToEdges = (from :: to :: valueEdges).distinct // distinct in case from equals to, and/or overlaps a value edge
        val (allEdges, nextNextRnd) = Randomizer.shuffle(fromToEdges, nextRnd)
        (allEdges.take(maxLength), nextNextRnd)
      }
      def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer) = {
        val (nextValue, nextRandomizer) = chooser.choose(from, to)(rnd)
        (Rose(nextValue), nextRandomizer)
      }
    }
  }

  /**
    * Create a [[Generator]] that returns [[Byte]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by the returned [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def bytesBetween(from: Byte, to: Byte): Generator[Byte] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[Short]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def shortsBetween(from: Short, to: Short): Generator[Short] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[Int]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def intsBetween(from: Int, to: Int): Generator[Int] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[Long]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def longsBetween(from: Long, to: Long): Generator[Long] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[Char]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def charsBetween(from: Char, to: Char): Generator[Char] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[Float]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def floatsBetween(from: Float, to: Float): Generator[Float] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[Double]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def doublesBetween(from: Double, to: Double): Generator[Double] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosInt]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posIntsBetween(from: PosInt, to: PosInt): Generator[PosInt] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosLong]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posLongsBetween(from: PosLong, to: PosLong): Generator[PosLong] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posFloatsBetween(from: PosFloat, to: PosFloat): Generator[PosFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosFiniteFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posFiniteFloatsBetween(from: PosFiniteFloat, to: PosFiniteFloat): Generator[PosFiniteFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posDoublesBetween(from: PosDouble, to: PosDouble): Generator[PosDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosFiniteDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posFiniteDoublesBetween(from: PosFiniteDouble, to: PosFiniteDouble): Generator[PosFiniteDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosZInt]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posZIntsBetween(from: PosZInt, to: PosZInt): Generator[PosZInt] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosZLong]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posZLongsBetween(from: PosZLong, to: PosZLong): Generator[PosZLong] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosZFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posZFloatsBetween(from: PosZFloat, to: PosZFloat): Generator[PosZFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosZFiniteFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posZFiniteFloatsBetween(from: PosZFiniteFloat, to: PosZFiniteFloat): Generator[PosZFiniteFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosZDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posZDoublesBetween(from: PosZDouble, to: PosZDouble): Generator[PosZDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[PosZFiniteDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def posZFiniteDoublesBetween(from: PosZFiniteDouble, to: PosZFiniteDouble): Generator[PosZFiniteDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegInt]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negIntsBetween(from: NegInt, to: NegInt): Generator[NegInt] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegLong]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negLongsBetween(from: NegLong, to: NegLong): Generator[NegLong] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negFloatsBetween(from: NegFloat, to: NegFloat): Generator[NegFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegFiniteFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negFiniteFloatsBetween(from: NegFiniteFloat, to: NegFiniteFloat): Generator[NegFiniteFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negDoublesBetween(from: NegDouble, to: NegDouble): Generator[NegDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegFiniteDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negFiniteDoublesBetween(from: NegFiniteDouble, to: NegFiniteDouble): Generator[NegFiniteDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegZInt]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negZIntsBetween(from: NegZInt, to: NegZInt): Generator[NegZInt] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegZLong]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negZLongsBetween(from: NegZLong, to: NegZLong): Generator[NegZLong] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegZFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negZFloatsBetween(from: NegZFloat, to: NegZFloat): Generator[NegZFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegZFiniteFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negZFiniteFloatsBetween(from: NegZFiniteFloat, to: NegZFiniteFloat): Generator[NegZFiniteFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegZDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negZDoublesBetween(from: NegZDouble, to: NegZDouble): Generator[NegZDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NegZFiniteDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def negZFiniteDoublesBetween(from: NegZFiniteDouble, to: NegZFiniteDouble): Generator[NegZFiniteDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NonZeroInt]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def nonZeroIntsBetween(from: NonZeroInt, to: NonZeroInt): Generator[NonZeroInt] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NonZeroLong]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def nonZeroLongsBetween(from: NonZeroLong, to: NonZeroLong): Generator[NonZeroLong] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NonZeroFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def nonZeroFloatsBetween(from: NonZeroFloat, to: NonZeroFloat): Generator[NonZeroFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NonZeroFiniteFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def nonZeroFiniteFloatsBetween(from: NonZeroFiniteFloat, to: NonZeroFiniteFloat): Generator[NonZeroFiniteFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NonZeroDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def nonZeroDoublesBetween(from: NonZeroDouble, to: NonZeroDouble): Generator[NonZeroDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[NonZeroFiniteDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def nonZeroFiniteDoublesBetween(from: NonZeroFiniteDouble, to: NonZeroFiniteDouble): Generator[NonZeroFiniteDouble] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[FiniteFloat]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def finiteFloatsBetween(from: FiniteFloat, to: FiniteFloat): Generator[FiniteFloat] = between(from, to)

  /**
    * Create a [[Generator]] that returns [[FiniteDouble]]s in the specified range.
    *
    * The range is inclusive: both ''from'' and ''to'' may be produced by this [[Generator]].
    * Moreover, ''from'' and ''to'' are considered to be edge cases, so they usually ''will'' be
    * produced in a typical run.
    *
    * The value of `from` must be less than or equal to the value of `to`.
    *
    * @param from one end of the desired range
    * @param to the other end of the desired range
    * @return a value within that range, inclusive of the bounds
    *
    * @group Betweeners
    */
  def finiteDoublesBetween(from: FiniteDouble, to: FiniteDouble): Generator[FiniteDouble] = between(from, to)



  /////////////////////////////////////////////////
  //
  // Other interesting Generator-creating functions
  //

  /**
    * Given a list of values of type [[T]], this creates a [[Generator]] that will only
    * produce those values.
    *
    * The order in which the values are produced is random, based on the [[Randomizer]] passed
    * in to the `next` function. It may produce the same value multiple times.
    *
    * @param first a value of type [[T]]
    * @param second another value of type [[T]]
    * @param rest more values of type [[T]], as many as you wish
    * @tparam T the type that will be produced by the resulting [[Generator]]
    * @return a [[Generator]] that produces exactly the specified values
    *
    * @group Specific
    */
  def specificValues[T](first: T, second: T, rest: T*): Generator[T] =
    new Generator[T] {
      private val seq: Seq[T] = first +: second +: rest
      def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer) = {
        val (nextInt, nextRandomizer) = rnd.chooseInt(0, seq.length - 1)
        val nextT = seq(nextInt)
        (Rose(nextT), nextRandomizer)
      }
    }

  /**
    * Creates a [[Generator]] that will always return exactly the same value.
    *
    * This is specialized, but occasionally useful. It is mainly appropriate when you have
    * a function that requires a [[Generator]], but only one value makes sense for the
    * Property you are evaluating.
    *
    * @param theValue the value to produce
    * @tparam T the type of that value
    * @return a [[Generator]] that will always produce that value
    *
    * @group Specific
    */
  def specificValue[T](theValue: T): Generator[T] =
    new Generator[T] {
      def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer) = {
        (Rose(theValue), rnd)
      }
    }

  // TODO: I wonder if I could get rid of the edges pattern match
  // by moving this to a different method. Then next is just next
  // in the distributed stuff. I could then do the pattern match
  // once and forall in a final method, nextEdge.
  /**
    * Given a number of [[Generator]]s, and the weightings for each one, this creates a [[Generator]]
    * that invokes each of its components according to its weighting.
    *
    * For example, consider this:
    * {{{
    *   val evens: Generator[Int] = ... // generates even Ints
    *   val odds: Generator[Int] = ... // generates odd Ints
    *   val zeros: Generator[Int] = specificValue(0)
    *
    *   val mixed: Generator[Int] = frequency(
    *     (5, evens),
    *     (4, odds),
    *     (1, zeros)
    *   )
    * }}}
    * The total weighting is (5 + 4 + 1) = 10. So the resulting [[Generator]] will produce
    * an even number (10 / 5) = 50% the time, an odd number (10 / 4) = 40% of the time, and zero
    * (10 / 1) = 10% of the time.
    *
    * Keep in mind that the distribution is invoked randomly, so these are rough proportions. As you
    * invoke the [[Generator]] more times, you should see results that are closer and closer to the
    * specified proportions, but the random element will generally keep it inexact.
    *
    * As usual, the resulting [[Generator]] will use the [[Randomizer]] passed in to [[Generator.next()]] to
    * choose which of the constituent [[Generator]]s to invoke. So if you use the same seed to initialize
    * your [[Randomizer]], you should get the same results.
    *
    * Note that all of the constituent [[Generator]]s must produce the same type.
    *
    * @param first a [[Generator]] and its weight
    * @param second another [[Generator]] and its weight
    * @param rest as many more [[Generator]] and weight pairs as you like
    * @tparam T the type being produced by all of these [[Generator]]s
    * @return a single [[Generator]], that invokes its constituents according to their weights
    *
    * @group Higher
    */
  def frequency[T](first: (Int, Generator[T]), second: (Int, Generator[T]), rest: (Int, Generator[T])*): Generator[T] = {
    val distribution: Vector[(Int, Generator[T])] = (first +: second +: rest).toVector
    // Take Int not PosInt, because Scala won't apply  multiple implicit
    // conversions, such as one for PosInt => Int, and another for Int => Generator[Int].
    // So just do a require.
    
    import org.scalactic.Requirements._
    require {
      distribution forall { case (w, _) => w >= 1 }
    }

    new Generator[T] {
      private val totalWeight: Int = distribution.toMap.keys.sum
      // gens contains, for each distribution pair, weight generators.
      private val gens: Vector[Generator[T]] =
        distribution flatMap { case (w, g) =>
          Vector.fill(w)(g)
        }
      def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer) = {
        val (nextInt, nextRandomizer) = rnd.chooseInt(0, gens.length - 1)
        val nextGen = gens(nextInt)
        nextGen.nextImpl(szp, isValidFun, nextRandomizer)
      }
    }
  }

  /**
    * Given a number of [[Generator]]s, this creates one that invokes each of its constituents with
    * roughly the same frequency.
    *
    * Consider this example:
    * {{{
    *   val numbers: Generator[Char] = ... // generates only digits
    *   val letters: Generator[Char] = ... // generates only letters
    *   val punct: Generator[Char]   = ... // generates only punctuation
    *
    *   val chars: Generator[Char] = evenly(numbers, letters, punct)
    * }}}
    * The `chars` [[Generator]] should produce numbers, letters and punctuation, each about a third
    * of the time.
    *
    * Keep in mind that the distribution is invoked randomly, so these are rough proportions. As you
    * invoke the [[Generator]] more times, you should see results that are closer and closer to an
    * equal distribution, but the random element will generally keep it inexact.
    *
    * As usual, the resulting [[Generator]] will use the [[Randomizer]] passed in to [[Generator.next()]] to
    * choose which of the constituent [[Generator]]s to invoke. So if you use the same seed to initialize
    * your [[Randomizer]], you should get the same results.
    *
    * Note that all of the constituent [[Generator]]s must produce the same type.
    *
    * @param first a [[Generator]] to choose from
    * @param second another [[Generator]] to choose from
    * @param rest any number of additional [[Generator]]s to choose from
    * @tparam T the type to be produced
    * @return a single [[Generator]] that invokes each of its constituents roughly the same number of times
    *
    * @group Higher
    */
  def evenly[T](first: Generator[T], second: Generator[T], rest: Generator[T]*): Generator[T] = {
    val distributees: Vector[Generator[T]] = (first +: second +: rest).toVector
    new Generator[T] {
      // gens contains, for each distribution pair, weight generators.
      def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer) = {
        val (nextInt, nextRandomizer) = rnd.chooseInt(0, distributees.length - 1)
        val nextGen = distributees(nextInt)
        nextGen.nextImpl(szp, isValidFun, nextRandomizer) // TODO: Is it correct to pass size and maxSize here?
      }
    }
  }



  /////////////////////////////////////////////////
  //
  // Functions that are just shells around those in Generator
  //

  /**
    * A [[Generator]] that produces [[Boolean]] values.
    *
    * @group Common
    */
  val booleans: Generator[Boolean] = Generator.booleanGenerator

  /**
    * A [[Generator]] that produces [[Byte]] values.
    *
    * @group Common
    */
  val bytes: Generator[Byte] = Generator.byteGenerator

  /**
    * A [[Generator]] that produces [[Short]] values.
    *
    * @group Common
    */
  val shorts: Generator[Short] = Generator.shortGenerator

  /**
    * A [[Generator]] that produces [[Int]] values.
    *
    * @group Common
    */
  val ints: Generator[Int] = Generator.intGenerator

  /**
    * A [[Generator]] that produces [[Long]] values.
    *
    * @group Common
    */
  val longs: Generator[Long] = Generator.longGenerator

  /**
    * A [[Generator]] that produces [[Char]] values.
    *
    * @group Common
    */
  val chars: Generator[Char] = Generator.charGenerator

  /**
    * A [[Generator]] that produces [[Float]] values.
    *
    * @group Common
    */
  val floats: Generator[Float] = Generator.floatGenerator

  /**
    * A [[Generator]] that produces [[Double]] values.
    *
    * @group Common
    */
  val doubles: Generator[Double] = Generator.doubleGenerator

  /**
    * A [[Generator]] that produces [[String]] values.
    *
    * @group Common
    */
  val strings: Generator[String] = Generator.stringGenerator

  /**
    * A [[Generator]] that produces [[PosInt]] values.
    *
    * @group Common
    */
  val posInts: Generator[PosInt] = Generator.posIntGenerator

  /**
    * A [[Generator]] that produces [[PosZInt]] values.
    *
    * @group Common
    */
  val posZInts: Generator[PosZInt] = Generator.posZIntGenerator

  /**
    * A [[Generator]] that produces [[PosLong]] values.
    *
    * @group Common
    */
  val posLongs: Generator[PosLong] = Generator.posLongGenerator

  /**
    * A [[Generator]] that produces [[PosZLong]] values.
    *
    * @group Common
    */
  val posZLongs: Generator[PosZLong] = Generator.posZLongGenerator

  /**
    * A [[Generator]] that produces [[PosFloat]] values.
    *
    * @group Common
    */
  val posFloats: Generator[PosFloat] = Generator.posFloatGenerator

  /**
    * A [[Generator]] that produces [[PosFiniteFloat]] values.
    *
    * @group Common
    */
  val posFiniteFloats: Generator[PosFiniteFloat] = Generator.posFiniteFloatGenerator

  /**
    * A [[Generator]] that produces [[PosZFloat]] values.
    *
    * @group Common
    */
  val posZFloats: Generator[PosZFloat] = Generator.posZFloatGenerator

  /**
    * A [[Generator]] that produces [[PosZFiniteFloat]] values.
    *
    * @group Common
    */
  val posZFiniteFloats: Generator[PosZFiniteFloat] = Generator.posZFiniteFloatGenerator

  /**
    * A [[Generator]] that produces [[PosDouble]] values.
    *
    * @group Common
    */
  val posDoubles: Generator[PosDouble] = Generator.posDoubleGenerator

  /**
    * A [[Generator]] that produces [[PosFiniteDouble]] values.
    *
    * @group Common
    */
  val posFiniteDoubles: Generator[PosFiniteDouble] = Generator.posFiniteDoubleGenerator

  /**
    * A [[Generator]] that produces [[PosZDouble]] values.
    *
    * @group Common
    */
  val posZDoubles: Generator[PosZDouble] = Generator.posZDoubleGenerator

  /**
    * A [[Generator]] that produces [[PosZFiniteDouble]] values.
    *
    * @group Common
    */
  val posZFiniteDoubles: Generator[PosZFiniteDouble] = Generator.posZFiniteDoubleGenerator

  /**
    * A [[Generator]] that produces [[NegInt]] values.
    *
    * @group Common
    */
  val negInts: Generator[NegInt] = Generator.negIntGenerator

  /**
    * A [[Generator]] that produces [[NegZInt]] values.
    *
    * @group Common
    */
  val negZInts: Generator[NegZInt] = Generator.negZIntGenerator

  /**
    * A [[Generator]] that produces [[NegLong]] values.
    *
    * @group Common
    */
  val negLongs: Generator[NegLong] = Generator.negLongGenerator

  /**
    * A [[Generator]] that produces [[NegZLong]] values.
    *
    * @group Common
    */
  val negZLongs: Generator[NegZLong] = Generator.negZLongGenerator

  /**
    * A [[Generator]] that produces [[NegFloat]] values.
    *
    * @group Common
    */
  val negFloats: Generator[NegFloat] = Generator.negFloatGenerator

  /**
    * A [[Generator]] that produces [[NegFiniteFloat]] values.
    *
    * @group Common
    */
  val negFiniteFloats: Generator[NegFiniteFloat] = Generator.negFiniteFloatGenerator

  /**
    * A [[Generator]] that produces [[NegZFloat]] values.
    *
    * @group Common
    */
  val negZFloats: Generator[NegZFloat] = Generator.negZFloatGenerator

  /**
    * A [[Generator]] that produces [[NegZFiniteFloat]] values.
    *
    * @group Common
    */
  val negZFiniteFloats: Generator[NegZFiniteFloat] = Generator.negZFiniteFloatGenerator

  /**
    * A [[Generator]] that produces [[NegDouble]] values.
    *
    * @group Common
    */
  val negDoubles: Generator[NegDouble] = Generator.negDoubleGenerator

  /**
    * A [[Generator]] that produces [[NegFiniteDouble]] values.
    *
    * @group Common
    */
  val negFiniteDoubles: Generator[NegFiniteDouble] = Generator.negFiniteDoubleGenerator

  /**
    * A [[Generator]] that produces [[NegZDouble]] values.
    *
    * @group Common
    */
  val negZDoubles: Generator[NegZDouble] = Generator.negZDoubleGenerator

  /**
    * A [[Generator]] that produces [[NegZFiniteDouble]] values.
    *
    * @group Common
    */
  val negZFiniteDoubles: Generator[NegZFiniteDouble] = Generator.negZFiniteDoubleGenerator

  /**
    * A [[Generator]] that produces [[NonZeroInt]] values.
    *
    * @group Common
    */
  val nonZeroInts: Generator[NonZeroInt] = Generator.nonZeroIntGenerator

  /**
    * A [[Generator]] that produces [[NonZeroLong]] values.
    *
    * @group Common
    */
  val nonZeroLongs: Generator[NonZeroLong] = Generator.nonZeroLongGenerator

  /**
    * A [[Generator]] that produces [[NonZeroFloat]] values.
    *
    * @group Common
    */
  val nonZeroFloats: Generator[NonZeroFloat] = Generator.nonZeroFloatGenerator

  /**
    * A [[Generator]] that produces [[NonZeroFiniteFloat]] values.
    *
    * @group Common
    */
  val nonZeroFiniteFloats: Generator[NonZeroFiniteFloat] = Generator.nonZeroFiniteFloatGenerator

  /**
    * A [[Generator]] that produces [[NonZeroDouble]] values.
    *
    * @group Common
    */
  val nonZeroDoubles: Generator[NonZeroDouble] = Generator.nonZeroDoubleGenerator

  /**
    * A [[Generator]] that produces [[NonZeroFiniteDouble]] values.
    *
    * @group Common
    */
  val nonZeroFiniteDoubles: Generator[NonZeroFiniteDouble] = Generator.nonZeroFiniteDoubleGenerator

  /**
    * A [[Generator]] that produces [[FiniteFloat]] values.
    *
    * @group Common
    */
  val finiteFloats: Generator[FiniteFloat] = Generator.finiteFloatGenerator

  /**
    * A [[Generator]] that produces [[FiniteDouble]] values.
    *
    * @group Common
    */
  val finiteDoubles: Generator[FiniteDouble] = Generator.finiteDoubleGenerator

  /**
    * A [[Generator]] that produces [[NumericChar]] values.
    *
    * @group Common
    */
  val numericChars: Generator[NumericChar] = Generator.numericCharGenerator

  /**
    * A [[Generator]] that produces positive [[Int]] values, not including zero.
    *
    * @group Values
    */
  val posIntValues: Generator[Int] = Generator.posIntGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Int]] values, including zero.
    *
    * @group Values
    */
  val posZIntValues: Generator[Int] = Generator.posZIntGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Long]] values, not including zero.
    *
    * @group Values
    */
  val posLongValues: Generator[Long] = Generator.posLongGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Long]] values, including zero.
    *
    * @group Values
    */
  val posZLongValues: Generator[Long] = Generator.posZLongGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Float]] values, not including zero
    * but including infinities and `NaN`.
    *
    * @group Values
    */
  val posFloatValues: Generator[Float] = Generator.posFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Float]] values, not including zero,
    * infinity or `NaN`.
    *
    * @group Values
    */
  val posFiniteFloatValues: Generator[Float] = Generator.posFiniteFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Float]] values, including zero, infinity
    * and `NaN`.
    *
    * @group Values
    */
  val posZFloatValues: Generator[Float] = Generator.posZFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Float]] values, including zero but not
    * including infinity or `NaN`.
    *
    * @group Values
    */
  val posZFiniteFloatValues: Generator[Float] = Generator.posZFiniteFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Double]] values, not including zero but
    * including infinity and `NaN`.
    *
    * @group Values
    */
  val posDoubleValues: Generator[Double] = Generator.posDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Double]] values, not including zero,
    * infinity or `NaN`.
    *
    * @group Values
    */
  val posFiniteDoubleValues: Generator[Double] = Generator.posFiniteDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Double]] values, including zero, infinity
    * and `NaN`.
    *
    * @group Values
    */
  val posZDoubleValues: Generator[Double] = Generator.posZDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces positive [[Int]] values.
    *
    * @group Values
    */
  val posZFiniteDoubleValues: Generator[Double] = Generator.posZFiniteDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Int]] values, not including zero.
    *
    * @group Values
    */
  val negIntValues: Generator[Int] = Generator.negIntGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Int]] values, including zero.
    *
    * @group Values
    */
  val negZIntValues: Generator[Int] = Generator.negZIntGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Long]] values, not including zero.
    *
    * @group Values
    */
  val negLongValues: Generator[Long] = Generator.negLongGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Long]] values, including zero.
    *
    * @group Values
    */
  val negZLongValues: Generator[Long] = Generator.negZLongGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Float]] values, not including zero but including
    * infinity and `NaN`.
    *
    * @group Values
    */
  val negFloatValues: Generator[Float] = Generator.negFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Float]] values, not including zero, infinity
    * or `NaN`.
    *
    * @group Values
    */
  val negFiniteFloatValues: Generator[Float] = Generator.negFiniteFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Float]] values, including zero, infinity
    * and `NaN`.
    *
    * @group Values
    */
  val negZFloatValues: Generator[Float] = Generator.negZFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Float]] values, including zero but not
    * including infinity or `NaN`.
    *
    * @group Values
    */
  val negZFiniteFloatValues: Generator[Float] = Generator.negZFiniteFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Double]] values, not including zero but including
    * infinity and `NaN`.
    *
    * @group Values
    */
  val negDoubleValues: Generator[Double] = Generator.negDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Double]] values, not including zero, infinity or
    * `NaN`.
    *
    * @group Values
    */
  val negFiniteDoubleValues: Generator[Double] = Generator.negFiniteDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Double]] values, including zero, infinity and `NaN`.
    *
    * @group Values
    */
  val negZDoubleValues: Generator[Double] = Generator.negZDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces negative [[Double]] values, including zero but not including
    * infinity or `NaN`.
    *
    * @group Values
    */
  val negZFiniteDoubleValues: Generator[Double] = Generator.negZFiniteDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces non-zero [[Int]] values.
    *
    * @group Values
    */
  val nonZeroIntValues: Generator[Int] = Generator.nonZeroIntGenerator.map(_.value)

  /**
    * A [[Generator]] that produces non-zero [[Long]] values.
    *
    * @group Values
    */
  val nonZeroLongValues: Generator[Long] = Generator.nonZeroLongGenerator.map(_.value)

  /**
    * A [[Generator]] that produces non-zero [[Float]] values, including infinity and `NaN`.
    *
    * @group Values
    */
  val nonZeroFloatValues: Generator[Float] = Generator.nonZeroFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces non-zero [[Float]] values, not including infinity
    * or `NaN`.
    *
    * @group Values
    */
  val nonZeroFiniteFloatValues: Generator[Float] = Generator.nonZeroFiniteFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces non-zero [[Double]] values, including infinity and `NaN`.
    *
    * @group Values
    */
  val nonZeroDoubleValues: Generator[Double] = Generator.nonZeroDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces non-zero [[Double]] values, not including infinity and `NaN`.
    *
    * @group Values
    */
  val nonZeroFiniteDoubleValues: Generator[Double] = Generator.nonZeroFiniteDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces [[Float]] values, including zero but not including infinity or `NaN`.
    *
    * @group Values
    */
  val finiteFloatValues: Generator[Float] = Generator.finiteFloatGenerator.map(_.value)

  /**
    * A [[Generator]] that produces [[Double]] values, including zero but not including infinity or `NaN`.
    *
    * @group Values
    */
  val finiteDoubleValues: Generator[Double] = Generator.finiteDoubleGenerator.map(_.value)

  /**
    * A [[Generator]] that produces digit [[Char]]s.
    *
    * @group Values
    */
  val numericCharValues: Generator[Char] = Generator.numericCharGenerator.map(_.value)


  /**
    * Given [[Generator]]s for types [[A]] and [[B]], get one that produces Tuples of those types.
    *
    * [[tuple2s]] (and its variants, up through [[tuple22s]]) will create [[Generator]]s on
    * demand for essentially arbitrary Tuples, so long as you have [[Generator]]s in implicit scope for all
    * of the component types.
    *
    * @param genOfA a [[Generator]] for type [[A]]
    * @param genOfB a [[Generator]] for type [[B]]
    * @tparam A the first type in the Tuple
    * @tparam B the second type in the Tuple
    * @return a [[Generator]] that produces the desired types, Tupled together.
    *
    * @group Collections
    */
  def tuple2s[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] = Generator.tuple2Generator[A, B]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple3s[A, B, C](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[(A, B, C)] = Generator.tuple3Generator[A, B, C]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple4s[A, B, C, D](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[(A, B, C, D)] = Generator.tuple4Generator[A, B, C, D]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple5s[A, B, C, D, E](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[(A, B, C, D, E)] = Generator.tuple5Generator[A, B, C, D, E]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple6s[A, B, C, D, E, F](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[(A, B, C, D, E, F)] = Generator.tuple6Generator[A, B, C, D, E, F]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple7s[A, B, C, D, E, F, G](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G]): Generator[(A, B, C, D, E, F, G)] = Generator.tuple7Generator[A, B, C, D, E, F, G]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple8s[A, B, C, D, E, F, G, H](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H]): Generator[(A, B, C, D, E, F, G, H)] = Generator.tuple8Generator[A, B, C, D, E, F, G, H]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple9s[A, B, C, D, E, F, G, H, I](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[(A, B, C, D, E, F, G, H, I)] = Generator.tuple9Generator[A, B, C, D, E, F, G, H, I]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple10s[A, B, C, D, E, F, G, H, I, J](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                             genOfJ: Generator[J]): Generator[(A, B, C, D, E, F, G, H, I, J)] = Generator.tuple10Generator[A, B, C, D, E, F, G, H, I, J]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple11s[A, B, C, D, E, F, G, H, I, J, K](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                genOfJ: Generator[J], genOfK: Generator[K]): Generator[(A, B, C, D, E, F, G, H, I, J, K)] = Generator.tuple11Generator[A, B, C, D, E, F, G, H, I, J, K]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple12s[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                   genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L)] = Generator.tuple12Generator[A, B, C, D, E, F, G, H, I, J, K, L]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple13s[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                      genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = Generator.tuple13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple14s[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                         genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = Generator.tuple14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple15s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                            genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = Generator.tuple15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple16s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                               genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = Generator.tuple16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple17s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                  genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = Generator.tuple17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple18s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                     genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = Generator.tuple18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple19s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                        genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = Generator.tuple19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple20s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                           genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                           genOfT: Generator[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = Generator.tuple20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple21s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                              genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                              genOfT: Generator[T], genOfU: Generator[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = Generator.tuple21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]

  /**
    * See [[tuple2s]] for information on tuple generators.
    *
    * @group Collections
    */
  def tuple22s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I],
                                                                                 genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S],
                                                                                 genOfT: Generator[T], genOfU: Generator[U], genOfV: Generator[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = Generator.tuple22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]

  /**
    * Given a [[Generator]] for type [[T]], this creates a [[Generator]] for [[Vector]] of [[T]].
    *
    * Note that the [[Vector]] type is considered to have a "size", so you can use the configuration parameters
    * [[Configuration.minSize]] and [[Configuration.sizeRange]] to constrain the sizes of the resulting `Vector`s
    * when you use this [[Generator]].
    *
    * The resulting [[Generator]] also has the [[HavingLength]] trait, so you can use it to generate [[Vector]]s
    * with specific lengths.
    *
    * @param genOfT a [[Generator]] that produces values of type [[T]]
    * @tparam T the type to produce
    * @return a [[Generator]] that produces values of type `Vector[T]`
    *
    * @group Collections
    */
  def vectors[T](implicit genOfT: Generator[T]): Generator[Vector[T]] with HavingLength[Vector[T]] = Generator.vectorGenerator

  /**
    * Given a `Generator[T]`, this creates a `Generator[Option[T]]`.
    *
    * @param genOfT a [[Generator]] that produces values of type [[T]]
    * @tparam T the type that we are producing an Option of
    * @return a Generator that produces `Option[T]`
    *
    * @group Collections
    */
  def options[T](implicit genOfT: Generator[T]): Generator[Option[T]] = Generator.optionGenerator

  /**
    * Given [[Generator]]s for two types, [[G]] and [[B]], this creates a [[Generator]] for `G Or B`.
    *
    * @param genOfG a [[Generator]] that produces type [[G]]
    * @param genOfB a [[Generator]] that produces type [[B]]
    * @tparam G the "good" type for an [[Or]]
    * @tparam B the "bad" type for an [[Or]]
    * @return a [[Generator]] that produces `G Or B`
    */
  def ors[G, B](implicit genOfG: Generator[G], genOfB: Generator[B]): Generator[G Or B] = Generator.orGenerator

  /**
    * Given [[Generator]]s for two types, [[L]] and [[R]], this creates a [[Generator]] for `Either[L, R]`.
    *
    * @param genOfL a [[Generator]] that produces type [[L]]
    * @param genOfR a [[Generator]] that produces type [[R]]
    * @tparam L the Left type for an [[Either]]
    * @tparam R the Right type for an [[Either]]
    * @return a [[Generator]] that produces `Either[L, R]`
    */
  def eithers[L, R](implicit genOfL: Generator[L], genOfR: Generator[R]): Generator[Either[L, R]] = Generator.eitherGenerator

  /**
    * Given an existing `Generator[T]`, this creates a `Generator[List[T]]`.
    *
    * @param genOfT a [[Generator]] that produces values of type [[T]]
    * @tparam T the type that we are producing a List of
    * @return a List of values of type [[T]]
    *
    * @group Collections
    */
  def lists[T](implicit genOfT: Generator[T]): Generator[List[T]] with HavingLength[List[T]] = Generator.listGenerator[T]

  /**
    * Given a [[Generator]] that produces values of type [[T]], this creates one for a [[Set]] of [[T]].
    *
    * Note that the [[Set]] type is considered to have a "size", so you can use the configuration parameters
    * [[Configuration.minSize]] and [[Configuration.sizeRange]] to constrain the sizes of the resulting `Set`s
    * when you use this [[Generator]].
    *
    * The resulting [[Generator]] also has the [[HavingSize]] trait, so you can use it to generate [[Set]]s
    * with specific sizes.
    *
    * @param genOfT a [[Generator]] that produces values of type [[T]]
    * @tparam T the type to produce
    * @return a [[Generator]] that produces `Set[T]`.
    *
    * @group Collections
    */
  def sets[T](implicit genOfT: Generator[T]): Generator[Set[T]] with HavingSize[Set[T]] = Generator.setGenerator

  /**
    * Given a [[Generator]] that produces values of type [[T]], this creates one for a [[SortedSet]] of [[T]].
    *
    * Note that the [[SortedSet]] type is considered to have a "size", so you can use the configuration parameters
    * [[Configuration.minSize]] and [[Configuration.sizeRange]] to constrain the sizes of the resulting `SortedSet`s
    * when you use this [[Generator]].
    *
    * The resulting [[Generator]] also has the [[HavingSize]] trait, so you can use it to generate [[SortedSet]]s
    * with specific sizes.
    *
    * @param genOfT a [[Generator]] that produces values of type [[T]]
    * @tparam T the type to produce
    * @return a [[Generator]] that produces `SortedSet[T]`.
    *
    * @group Collections
    */
  def sortedSets[T](implicit genOfT: Generator[T], ordering: Ordering[T]): Generator[SortedSet[T]] with HavingSize[SortedSet[T]] = Generator.sortedSetGenerator

  /**
    * Given a [[Generator]] that produces Tuples of key/value pairs, this gives you one that produces [[Map]]s
    * with those pairs.
    *
    * If you are simply looking for random-pairing of the key and value types, this is pretty easy to use:
    * if both the key and value types have corresponding [[Generator]]s, then the Tuple and Map ones will be automatically
    * and implicitly created when you need them.
    *
    * The resulting [[Generator]] also has the [[HavingSize]] trait, so you can use it to generate [[Map]]s
    * with specific sizes.
    *
    * @param genOfTuple2KV a [[Generator]] that produces Tuples of [[K]] and [[V]]
    * @tparam K the type of the keys for the [[Map]]
    * @tparam V the type of the values for the [[Map]]
    * @return a [[Generator]] of [[Map]]s from [[K]] to [[V]]
    *
    * @group Collections
    */
  def maps[K, V](implicit genOfTupleKV: Generator[(K, V)]): Generator[Map[K, V]] with HavingSize[Map[K, V]] = Generator.mapGenerator

  /**
    * Given a [[Generator]] that produces Tuples of key/value pairs, this gives you one that produces [[SortedMap]]s
    * with those pairs.
    *
    * If you are simply looking for random pairing of the key and value types, this is pretty easy to use:
    * if both the key and value types have [[Generator]]s, then the Tuple and SortedMap ones will be automatically
    * and implicitly created when you need them.
    *
    * The resulting [[Generator]] also has the [[HavingSize]] trait, so you can use it to generate [[SortedMap]]s
    * with specific sizes.
    *
    * @param genOfTuple2KV a [[Generator]] that produces Tuples of [[K]] and [[V]]
    * @tparam K the type of the keys for the [[SortedMap]]
    * @tparam V the type of the values for the [[SortedMap]]
    * @return a [[Generator]] of [[SortedMap]]s from [[K]] to [[V]]
    *
    * @group Collections
    */
  def sortedMaps[K, V](implicit genOfTupleKV: Generator[(K, V)], ordering: Ordering[K]): Generator[SortedMap[K, V]] with HavingSize[SortedMap[K, V]] = Generator.sortedMapGenerator


  /**
    * Given a [[Generator]] that produces values of type [[A]], this returns one that produces ''functions'' that return
    * a T.
    *
    * The functions produced here are nullary -- they take no parameters, they just spew out values of type [[A]].
    *
    * @param genOfT a [[Generator]] that produces functions that return [[A]]
    * @tparam A the type to return from the generated functions
    * @return a [[Generator]] that produces functions that return values of type [[A]]
    *
    * @group Functions
    */
  def function0s[A](implicit genOfA: Generator[A]): Generator[() => A] = Generator.function0Generator[A]

  /**
    * Create a [[Generator]] of functions from type [[A]] to type [[B]].
    *
    * Note that the generated functions are, necessarily, pretty random. In practice, the function you get from a
    * [[function1s]] call (and its variations, up through [[function22s]]) takes the hashes of its input
    * values, combines those with a randomly chosen number, and combines them in order to choose the generated value
    * [[B]].
    *
    * That said, each of the generated functions ''is'' deterministic: given the same input parameters and the same
    * randomly chosen number, you will always get the same [[B]] result. And the `toString` function on the generated
    * function will show the formula you need to use in order to recreate that, which will look something like:
    *
    * {{{
    *   (a: Int, b: String, c: Float) =>
    *     org.scalatest.prop.valueOf[String](a, b, c)(131)
    * }}}
    *
    * The number and type of the `a`, `b`, `c`, etc, parameters, as well as the type parameter of [[valueOf]], will depend
    * on the function type you are generating, but they will always follow this pattern. [[valueOf]] is the underlying
    * function that takes these parameters and the randomly chosen number, and returns a value of the specified type.
    *
    * So if a property evaluation fails, the display of the generated function will tell you how to call [[valueOf]]
    * to recreate the failure.
    *
    * The `typeInfo` parameters are automatically created via macros; you should generally not try to pass them manually.
    *
    * @param genOfB a [[Generator]] for the desired result type [[B]]
    * @param typeInfoA automatically-created type information for type [[A]]
    * @param typeInfoB automatically-created type information for type [[B]]
    * @tparam A the input type for the generated functions
    * @tparam B the result type for the generated functions
    * @return a [[Generator]] that produces functions that take values of [[A]] and returns values of [[B]]
    *
    * @group Functions
    */
  def function1s[A, B](implicit genOfB: Generator[B], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B]): Generator[A => B] =
    Generator.function1Generator[A, B]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function2s[A, B, C](implicit genOfC: Generator[C], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C]): Generator[(A, B) => C] =
    Generator.function2Generator[A, B, C]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function3s[A, B, C, D](implicit genOfD: Generator[D], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D]): Generator[(A, B, C) => D] =
    Generator.function3Generator[A, B, C, D]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function4s[A, B, C, D, E](implicit genOfE: Generator[E], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E]): Generator[(A, B, C, D) => E] =
    Generator.function4Generator[A, B, C, D, E]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function5s[A, B, C, D, E, F](implicit genOfF: Generator[F], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F]): Generator[(A, B, C, D, E) => F] =
    Generator.function5Generator[A, B, C, D, E, F]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function6s[A, B, C, D, E, F, G](implicit genOfG: Generator[G], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G]): Generator[(A, B, C, D, E, F) => G] =
    Generator.function6Generator[A, B, C, D, E, F, G]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function7s[A, B, C, D, E, F, G, H](implicit genOfH: Generator[H], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H]): Generator[(A, B, C, D, E, F, G) => H] =
    Generator.function7Generator[A, B, C, D, E, F, G, H]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function8s[A, B, C, D, E, F, G, H, I](implicit genOfI: Generator[I], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I]): Generator[(A, B, C, D, E, F, G, H) => I] =
    Generator.function8Generator[A, B, C, D, E, F, G, H, I]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function9s[A, B, C, D, E, F, G, H, I, J](implicit genOfJ: Generator[J], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J]): Generator[(A, B, C, D, E, F, G, H, I) => J] =
    Generator.function9Generator[A, B, C, D, E, F, G, H, I, J]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function10s[A, B, C, D, E, F, G, H, I, J, K](implicit genOfK: Generator[K], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K]): Generator[(A, B, C, D, E, F, G, H, I, J) => K] =
    Generator.function10Generator[A, B, C, D, E, F, G, H, I, J, K]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function11s[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfL: Generator[L], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] =
    Generator.function11Generator[A, B, C, D, E, F, G, H, I, J, K, L]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function12s[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfM: Generator[M], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] =
    Generator.function12Generator[A, B, C, D, E, F, G, H, I, J, K, L, M]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function13s[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfN: Generator[N], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] =
    Generator.function13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function14s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfO: Generator[O], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] =
    Generator.function14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function15s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfP: Generator[P], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] =
    Generator.function15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function16s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfQ: Generator[Q], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] =
    Generator.function16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function17s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfR: Generator[R], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] =
    Generator.function17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function18s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfS: Generator[S], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] =
    Generator.function18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function19s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfT: Generator[T], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] =
    Generator.function19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function20s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfU: Generator[U], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] =
    Generator.function20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function21s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfV: Generator[V], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] =
    Generator.function21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V]

  /**
    * See [[function1s]].
    *
    * @group Functions
    */
  def function22s[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit genOfW: Generator[W], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V], typeInfoW: TypeInfo[W]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] =
    Generator.function22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W]


  /**
    * The `instancesOf` function (which has overloads depending on how many parameters you need)
    * is one way to create a [[Generator]] for case classes and other situations where you
    * want to build a type out of other types.
    *
    * To understand how it works, look at this example:
    * {{{
    *   case class Person(name: String, age: Int)
    *   implicit val persons: Generator[Person] =
    *     instancesOf(Person) { p =>
    *       (p.name, p.age)
    *     } (strings, posZIntValues)
    * }}}
    * What's going on here? `instancesOf` is taking two types ([[String]] and [[Int]]),
    * a function (a case class constructor) that turns those types into a third type (`Person`),
    * and a second function that takes a `Person` and deconstructs it back to its component
    * pieces. From those, it creates a [[Generator]].
    *
    * The last parameters -- the `(strings, posZIntValues)` -- are the [[Generator]]s for
    * the component types. If you are good with using the default Generators for those types,
    * you can just let those parameters be resolved implicitly. (But in this case, that
    * could result in negative ages, which doesn't make any sense.)
    *
    * After creating a [[Generator]] this way, you can use it like any other Generator in
    * your property checks.
    *
    * Alternatively, you can construct Generators for case classes using for
    * comprehensions, like this:
    * {{{
    *   implicit val persons: Generator[Person] = for {
    *     name <- strings
    *     age <- posZIntValues
    *   }
    *     yield Person(name, age)
    * }}}
    * Which approach you use is mainly up to personal taste and the coding standards
    * of your project.
    *
    * @param construct a constructor that builds the target type from its constituents;
    *                  most often, a case class constructor
    * @param deconstruct a deconstructor function that takes the target type and breaks
    *                    it down into its constituents
    * @param genOfA a [[Generator]] for the input type
    * @tparam A the input type
    * @tparam B the target type to be generated
    * @return a [[Generator]] for the target type
    *
    * @group InstancesOf
    */
  def instancesOf[A, B](construct: A => B)(deconstruct: B => A)(implicit genOfA: Generator[A]): Generator[B] =
    new GeneratorFor1[A, B](construct, deconstruct)(genOfA)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C](construct: (A, B) => C)(deconstruct: C => (A, B))(implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[C] =
    new GeneratorFor2[A, B, C](construct, deconstruct)(genOfA, genOfB)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D](construct: (A, B, C) => D)(deconstruct: D => (A, B, C))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[D] =
    new GeneratorFor3[A, B, C, D](construct, deconstruct)(genOfA, genOfB, genOfC)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E](construct: (A, B, C, D) => E)(deconstruct: E => (A, B, C, D))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[E] =
    new GeneratorFor4[A, B, C, D, E](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F](construct: (A, B, C, D, E) => F)(deconstruct: F => (A, B, C, D, E))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[F] =
    new GeneratorFor5[A, B, C, D, E, F](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G](construct: (A, B, C, D, E, F) => G)(deconstruct: G => (A, B, C, D, E, F))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[G] =
    new GeneratorFor6[A, B, C, D, E, F, G](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H](construct: (A, B, C, D, E, F, G) => H)(deconstruct: H => (A, B, C, D, E, F, G))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                          genOfG: Generator[G]): Generator[H] =
    new GeneratorFor7[A, B, C, D, E, F, G, H](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I](construct: (A, B, C, D, E, F, G, H) => I)(deconstruct: I => (A, B, C, D, E, F, G, H))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                   genOfG: Generator[G], genOfH: Generator[H]): Generator[I] =
    new GeneratorFor8[A, B, C, D, E, F, G, H, I](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J](construct: (A, B, C, D, E, F, G, H, I) => J)(deconstruct: J => (A, B, C, D, E, F, G, H, I))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                            genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[J] =
    new GeneratorFor9[A, B, C, D, E, F, G, H, I, J](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K](construct: (A, B, C, D, E, F, G, H, I, J) => K)(deconstruct: K => (A, B, C, D, E, F, G, H, I, J))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                     genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J]): Generator[K] =
    new GeneratorFor10[A, B, C, D, E, F, G, H, I, J, K](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L](construct: (A, B, C, D, E, F, G, H, I, J, K) => L)(deconstruct: L => (A, B, C, D, E, F, G, H, I, J, K))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                              genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K]): Generator[L] =
    new GeneratorFor11[A, B, C, D, E, F, G, H, I, J, K, L](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M](construct: (A, B, C, D, E, F, G, H, I, J, K, L) => M)(deconstruct: M => (A, B, C, D, E, F, G, H, I, J, K, L))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                       genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[M] =
    new GeneratorFor12[A, B, C, D, E, F, G, H, I, J, K, L, M](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M) => N)(deconstruct: N => (A, B, C, D, E, F, G, H, I, J, K, L, M))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[N] =
    new GeneratorFor13[A, B, C, D, E, F, G, H, I, J, K, L, M, N](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O)(deconstruct: O => (A, B, C, D, E, F, G, H, I, J, K, L, M, N))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                         genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                         genOfN: Generator[N]): Generator[O] =
    new GeneratorFor14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P)(deconstruct: P => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                  genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                  genOfN: Generator[N], genOfO: Generator[O]): Generator[P] =
    new GeneratorFor15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q)(deconstruct: Q => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                           genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                           genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[Q] =
    new GeneratorFor16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R)(deconstruct: R => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                    genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                    genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[R] =
    new GeneratorFor17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S)(deconstruct: S => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                             genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                             genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[S] =
    new GeneratorFor18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T)(deconstruct: T => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                      genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                      genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[T] =
    new GeneratorFor19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U)(deconstruct: U => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                               genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                               genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T]): Generator[U] =
    new GeneratorFor20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V)(deconstruct: V => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                                        genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                                        genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T],
                                                                                                                                                                                                                                                        genOfU: Generator[U]): Generator[V] =
    new GeneratorFor21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU)

  /**
    * See the simple `[A, B]` version of `instancesOf()` for details.
    *
    * @group InstancesOf
    */
  def instancesOf[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](construct: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W)(deconstruct: W => (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V))(implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F],
                                                                                                                                                                                                                                                                 genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M],
                                                                                                                                                                                                                                                                 genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T],
                                                                                                                                                                                                                                                                 genOfU: Generator[U], genOfV: Generator[V]): Generator[W] =
    new GeneratorFor22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](construct, deconstruct)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU, genOfV)

  /**
    * Generate a bunch of values from a [[Generator]], and distribute them into buckets.
    *
    * This function mainly exists for the purpose of testing your [[Generator]], and making sure that it is actually
    * creating data with the sort of distribution you expect. You provide the [[Generator]], the number of values to
    * create, and a function that "classifies" each value with a String; it returns a [[Classification]] that
    * collates all of the results. You can then look at the [[Classification]] to see if the proportions match
    * your expectations.
    *
    * For example, consider the following simple classification of small numbers:
    * {{{
    * val classification: Classification =
    *   CommonGenerators.classify(10000, CommonGenerators.intsBetween(0, 9))
    *   {
    *     case x if (x % 2) == 0 => "even"
    *     case _ => "odd"
    *   }
    * }}}
    * As expected, the results are distributed evenly:
    * {{{
    * classification: org.scalatest.prop.Classification =
    * 50% odd
    * 50% even
    * }}}
    *
    * The options provided in the PartialFunction do not have to be comprehensive; it is legal for some generated
    * values to not match any of the choices. In this case, those values will not be accounted for in the
    * resulting [[Classification]].
    *
    * @param count the number of values to generate
    * @param genOfA the [[Generator]] to use
    * @param pf a [[PartialFunction]] that takes the generated values, and sorts them into "buckets" by String names
    * @tparam A the type to be generated
    * @return statistics on how many values wound up in each bucket
    *
    * @group Tools
    */
  // classify will need to use the same sizing algo as forAll, and same edges approach
  def classify[A](count: PosInt, genOfA: Generator[A])(pf: PartialFunction[A, String]): Classification = {

    val (initEdges, rnd1) = genOfA.initEdges(100, Randomizer.default)
    @tailrec
    def loop(currentCount: Int, edges: List[A], rnd: Randomizer, acc: Map[String, PosZInt]): Map[String, PosZInt] = {
      if (currentCount >= count) acc
      else {
        val (nextRoseTreeOfA, nextEdges, nextRnd) = genOfA.next(SizeParam(PosZInt(0), PosZInt(100), PosZInt(100)), edges, rnd) // TODO: I think this need to mimic forAll.
        if (pf.isDefinedAt(nextRoseTreeOfA.value)) {
          val category = pf(nextRoseTreeOfA.value)
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

  /**
    * A [[Generator]] of prime numbers.
    *
    * As the name implies, this doesn't try to generate entirely arbitrary prime numbers. Instead,
    * it takes the simpler and more efficient approach of choosing randomly from a hard-coded
    * table of the first 1000 primes. As a result, the largest number that can be produced from
    * this is 7919.
    *
    * @return a [[Generator]] that will produce smallish prime numbers
    *
    * @group Tools
    */
  lazy val first1000Primes: Generator[Int] =
    new Generator[Int] { thisIntGenerator =>
      def nextImpl(szp: SizeParam, isValidFun: (Int, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Int], Randomizer) = {
        import CommonGenerators.primeNumbers
        val (index, nextRandomizer) = rnd.chooseInt(0, primeNumbers.length - 1)
        (Rose(primeNumbers(index)), nextRandomizer)
      }
    }

  def lazily[T](gen: => Generator[T]): Generator[T] = {
    new Generator[T] {
      private lazy val underlying: Generator[T] = gen

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[T], Randomizer) = underlying.initEdges(maxLength, rnd)

      override def canonicals: LazyListOrStream[RoseTree[T]] = underlying.canonicals

      // gens contains, for each distribution pair, weight generators.
      def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer) = {
        gen.nextImpl(szp, isValidFun, rnd)
      }

      override def map[U](f: T => U): Generator[U] = underlying.map(f)

      override def flatMap[U](f: T => Generator[U]): Generator[U] = underlying.flatMap(f)

      override def filter(p: T => Boolean): Generator[T] = underlying.filter(p)

      override def withFilter(p: T => Boolean): Generator[T] = underlying.withFilter(p)
    }
  }
}

/**
  * An import-able version of [[CommonGenerators]].
  *
  * You should not usually need to import this directly, since it is mixed into
  * [[GeneratorDrivenPropertyChecks]] and [[TableDrivenPropertyChecks]].
  */
object CommonGenerators extends CommonGenerators {
  private val primeNumbers =
    Vector(
      2,     3,     5,     7,    11,    13,    17,    19,    23,    29,
      31,    37,    41,    43,    47,    53,    59,    61,    67,    71,
      73,    79,    83,    89,    97,   101,   103,   107,   109,   113,
      127,   131,   137,   139,   149,   151,   157,   163,   167,   173,
      179,   181,   191,   193,   197,   199,   211,   223,   227,   229,
      233,   239,   241,   251,   257,   263,   269,   271,   277,   281,
      283,   293,   307,   311,   313,   317,   331,   337,   347,   349,
      353,   359,   367,   373,   379,   383,   389,   397,   401,   409,
      419,   421,   431,   433,   439,   443,   449,   457,   461,   463,
      467,   479,   487,   491,   499,   503,   509,   521,   523,   541,
      547,   557,   563,   569,   571,   577,   587,   593,   599,   601,
      607,   613,   617,   619,   631,   641,   643,   647,   653,   659,
      661,   673,   677,   683,   691,   701,   709,   719,   727,   733,
      739,   743,   751,   757,   761,   769,   773,   787,   797,   809,
      811,   821,   823,   827,   829,   839,   853,   857,   859,   863,
      877,   881,   883,   887,   907,   911,   919,   929,   937,   941,
      947,   953,   967,   971,   977,   983,   991,   997,  1009,  1013,
      1019,  1021,  1031,  1033,  1039,  1049,  1051,  1061,  1063,  1069,
      1087,  1091,  1093,  1097,  1103,  1109,  1117,  1123,  1129,  1151,
      1153,  1163,  1171,  1181,  1187,  1193,  1201,  1213,  1217,  1223,
      1229,  1231,  1237,  1249,  1259,  1277,  1279,  1283,  1289,  1291,
      1297,  1301,  1303,  1307,  1319,  1321,  1327,  1361,  1367,  1373,
      1381,  1399,  1409,  1423,  1427,  1429,  1433,  1439,  1447,  1451,
      1453,  1459,  1471,  1481,  1483,  1487,  1489,  1493,  1499,  1511,
      1523,  1531,  1543,  1549,  1553,  1559,  1567,  1571,  1579,  1583,
      1597,  1601,  1607,  1609,  1613,  1619,  1621,  1627,  1637,  1657,
      1663,  1667,  1669,  1693,  1697,  1699,  1709,  1721,  1723,  1733,
      1741,  1747,  1753,  1759,  1777,  1783,  1787,  1789,  1801,  1811,
      1823,  1831,  1847,  1861,  1867,  1871,  1873,  1877,  1879,  1889,
      1901,  1907,  1913,  1931,  1933,  1949,  1951,  1973,  1979,  1987,
      1993,  1997,  1999,  2003,  2011,  2017,  2027,  2029,  2039,  2053,
      2063,  2069,  2081,  2083,  2087,  2089,  2099,  2111,  2113,  2129,
      2131,  2137,  2141,  2143,  2153,  2161,  2179,  2203,  2207,  2213,
      2221,  2237,  2239,  2243,  2251,  2267,  2269,  2273,  2281,  2287,
      2293,  2297,  2309,  2311,  2333,  2339,  2341,  2347,  2351,  2357,
      2371,  2377,  2381,  2383,  2389,  2393,  2399,  2411,  2417,  2423,
      2437,  2441,  2447,  2459,  2467,  2473,  2477,  2503,  2521,  2531,
      2539,  2543,  2549,  2551,  2557,  2579,  2591,  2593,  2609,  2617,
      2621,  2633,  2647,  2657,  2659,  2663,  2671,  2677,  2683,  2687,
      2689,  2693,  2699,  2707,  2711,  2713,  2719,  2729,  2731,  2741,
      2749,  2753,  2767,  2777,  2789,  2791,  2797,  2801,  2803,  2819,
      2833,  2837,  2843,  2851,  2857,  2861,  2879,  2887,  2897,  2903,
      2909,  2917,  2927,  2939,  2953,  2957,  2963,  2969,  2971,  2999,
      3001,  3011,  3019,  3023,  3037,  3041,  3049,  3061,  3067,  3079,
      3083,  3089,  3109,  3119,  3121,  3137,  3163,  3167,  3169,  3181,
      3187,  3191,  3203,  3209,  3217,  3221,  3229,  3251,  3253,  3257,
      3259,  3271,  3299,  3301,  3307,  3313,  3319,  3323,  3329,  3331,
      3343,  3347,  3359,  3361,  3371,  3373,  3389,  3391,  3407,  3413,
      3433,  3449,  3457,  3461,  3463,  3467,  3469,  3491,  3499,  3511,
      3517,  3527,  3529,  3533,  3539,  3541,  3547,  3557,  3559,  3571,
      3581,  3583,  3593,  3607,  3613,  3617,  3623,  3631,  3637,  3643,
      3659,  3671,  3673,  3677,  3691,  3697,  3701,  3709,  3719,  3727,
      3733,  3739,  3761,  3767,  3769,  3779,  3793,  3797,  3803,  3821,
      3823,  3833,  3847,  3851,  3853,  3863,  3877,  3881,  3889,  3907,
      3911,  3917,  3919,  3923,  3929,  3931,  3943,  3947,  3967,  3989,
      4001,  4003,  4007,  4013,  4019,  4021,  4027,  4049,  4051,  4057,
      4073,  4079,  4091,  4093,  4099,  4111,  4127,  4129,  4133,  4139,
      4153,  4157,  4159,  4177,  4201,  4211,  4217,  4219,  4229,  4231,
      4241,  4243,  4253,  4259,  4261,  4271,  4273,  4283,  4289,  4297,
      4327,  4337,  4339,  4349,  4357,  4363,  4373,  4391,  4397,  4409,
      4421,  4423,  4441,  4447,  4451,  4457,  4463,  4481,  4483,  4493,
      4507,  4513,  4517,  4519,  4523,  4547,  4549,  4561,  4567,  4583,
      4591,  4597,  4603,  4621,  4637,  4639,  4643,  4649,  4651,  4657,
      4663,  4673,  4679,  4691,  4703,  4721,  4723,  4729,  4733,  4751,
      4759,  4783,  4787,  4789,  4793,  4799,  4801,  4813,  4817,  4831,
      4861,  4871,  4877,  4889,  4903,  4909,  4919,  4931,  4933,  4937,
      4943,  4951,  4957,  4967,  4969,  4973,  4987,  4993,  4999,  5003,
      5009,  5011,  5021,  5023,  5039,  5051,  5059,  5077,  5081,  5087,
      5099,  5101,  5107,  5113,  5119,  5147,  5153,  5167,  5171,  5179,
      5189,  5197,  5209,  5227,  5231,  5233,  5237,  5261,  5273,  5279,
      5281,  5297,  5303,  5309,  5323,  5333,  5347,  5351,  5381,  5387,
      5393,  5399,  5407,  5413,  5417,  5419,  5431,  5437,  5441,  5443,
      5449,  5471,  5477,  5479,  5483,  5501,  5503,  5507,  5519,  5521,
      5527,  5531,  5557,  5563,  5569,  5573,  5581,  5591,  5623,  5639,
      5641,  5647,  5651,  5653,  5657,  5659,  5669,  5683,  5689,  5693,
      5701,  5711,  5717,  5737,  5741,  5743,  5749,  5779,  5783,  5791,
      5801,  5807,  5813,  5821,  5827,  5839,  5843,  5849,  5851,  5857,
      5861,  5867,  5869,  5879,  5881,  5897,  5903,  5923,  5927,  5939,
      5953,  5981,  5987,  6007,  6011,  6029,  6037,  6043,  6047,  6053,
      6067,  6073,  6079,  6089,  6091,  6101,  6113,  6121,  6131,  6133,
      6143,  6151,  6163,  6173,  6197,  6199,  6203,  6211,  6217,  6221,
      6229,  6247,  6257,  6263,  6269,  6271,  6277,  6287,  6299,  6301,
      6311,  6317,  6323,  6329,  6337,  6343,  6353,  6359,  6361,  6367,
      6373,  6379,  6389,  6397,  6421,  6427,  6449,  6451,  6469,  6473,
      6481,  6491,  6521,  6529,  6547,  6551,  6553,  6563,  6569,  6571,
      6577,  6581,  6599,  6607,  6619,  6637,  6653,  6659,  6661,  6673,
      6679,  6689,  6691,  6701,  6703,  6709,  6719,  6733,  6737,  6761,
      6763,  6779,  6781,  6791,  6793,  6803,  6823,  6827,  6829,  6833,
      6841,  6857,  6863,  6869,  6871,  6883,  6899,  6907,  6911,  6917,
      6947,  6949,  6959,  6961,  6967,  6971,  6977,  6983,  6991,  6997,
      7001,  7013,  7019,  7027,  7039,  7043,  7057,  7069,  7079,  7103,
      7109,  7121,  7127,  7129,  7151,  7159,  7177,  7187,  7193,  7207,
      7211,  7213,  7219,  7229,  7237,  7243,  7247,  7253,  7283,  7297,
      7307,  7309,  7321,  7331,  7333,  7349,  7351,  7369,  7393,  7411,
      7417,  7433,  7451,  7457,  7459,  7477,  7481,  7487,  7489,  7499,
      7507,  7517,  7523,  7529,  7537,  7541,  7547,  7549,  7559,  7561,
      7573,  7577,  7583,  7589,  7591,  7603,  7607,  7621,  7639,  7643,
      7649,  7669,  7673,  7681,  7687,  7691,  7699,  7703,  7717,  7723,
      7727,  7741,  7753,  7757,  7759,  7789,  7793,  7817,  7823,  7829,
      7841,  7853,  7867,  7873,  7877,  7879,  7883,  7901,  7907,  7919
    )
}
