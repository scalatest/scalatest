/*
 * Copyright 2001-2016 Artima, Inc.
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

import scala.collection.mutable.ListBuffer
import org.scalactic.anyvals._
import org.scalactic.{Bad, Good, Or}
import scala.annotation.tailrec
import org.scalactic.source.TypeInfo
import org.scalatest.Resources
import CommonGenerators.first1000Primes
import scala.collection.immutable.SortedSet
import scala.collection.immutable.SortedMap
import org.scalactic.ColCompatHelper.LazyListOrStream

/**
  * Base type for all Generators.
  *
  * A Generator produces a stream of values of a particular type. This is usually a mix of
  * randomly-created values (generally built using a [[Randomizer]]), as well as some well-known
  * ''edge cases'' that tend to cause bugs in real-world code.
  *
  * For example, consider an ''intGenerator'' that produces a sequence of Ints. Some of these will be taken from
  * [[Randomizer.nextInt]], which may result in any possible Int, so the values will be a very random
  * mix of numbers. But it will also produce the known edge cases of Int:
  *
  *   - [[Int.MinValue]], the smallest possible Int
  *   - [[Int.MaxValue]], the largest possible Int
  *   - -1
  *   - 0
  *   - 1
  *
  * The list of appropriate edge cases will vary from type to type, but they should be chosen so as
  * to exercise the type broadly, and at extremes.
  *
  * ==Creating Your Own Generators==
  *
  * [[Generator.intGenerator]], and Generators for many other basic types, are already built into the
  * system, so you can just use them. You can (and should) define Generators for your own types, as well.
  *
  * In most cases, you do not need to write Generators from scratch -- Generators for most non-primitive
  * types can be composed using for comprehensions, as described in the section ''Composing Your Own
  * Generators'' in the documentation for the [[org.scalatest.prop]] pacakge. You can also often create
  * them using the [[CommonGenerators.instancesOf]] method. You should only need to write a Generator
  * from scratch for relatively primitive types, that aren't composed of other types.
  *
  * If you decide that you ''do'' need to build a Generator from scratch, here is a rough outline
  * of how to go about it.
  *
  * First, look at the source code for some of the Generators in the [[Generator]] companion object.
  * These follow a pretty standard pattern, that you will likely want to follow.
  *
  * ===Size===
  *
  * Your Generator may optionally have a concept of '''size'''. What this means varies from type to type:
  * for a String it might be the number of characters, whereas for a List it might be the number of
  * elements. The test system will try using the Generator with a variety of sizes; you can control
  * the maximum and minimum sizes via [[Configuration]].
  *
  * Decide whether the concept of ''size'' is relevant for your type. If it is relevant, you should mix the
  * [[HavingSize]] or [[HavingLength]] trait into your Generator, and you'll want to take
  * it into account in your `next` and `shrink` functions.
  *
  * ===Randomization===
  *
  * The Generator should do all of its '''"random" data generation''' using the [[Randomizer]] instance passed
  * in to it, and should return the next Randomizer with its results. [[Randomizer]] produces intentionally
  * pseudo-random data: it ''looks'' reasonably random, but is actually entirely deterministic based on the
  * seed used to initialize it. By consistently using Randomizer, the Generator can be re-run, producing the
  * same values, when given an identically-seeded Randomizer. This can often make debugging much easier,
  * since it allows you to reproduce your "random" failures.
  *
  * So figure out how to create a pseudo-random value of your type using [[Randomizer]]. This will
  * likely involve writing a function similar to the various `nextT()` functions inside of
  * Randomizer itself.
  *
  * ===next()===
  *
  * Using this randomization function, write a first draft of your Generator, filling in the
  * `next()` method. This is the only required method, and should suffice to start playing with
  * your Generator. Once this is working, you have a useful Generator.
  *
  * ===Edges===
  *
  * The '''edges''' are the edge cases for this type. You may have as many or as few edge cases as seem
  * appropriate, but most types involve at least a few. Edges are generally values that are particularly
  * big/full, or particularly small/empty. The test system will prioritize applying the edge cases to
  * the property, since they are assumed to be the values most likely to cause failures.
  *
  * Figure out some appropriate edge cases for your type. Override `initEdges()` to return
  * those, and enhance `next()` to produce them ahead of the random values. Identifying these will tend to make
  * your property checks more effective, by catching these edge cases early.
  *
  * ===Canonicals===
  *
  * Now figure out some canonical values for your type -- a few common, ordinary values that
  * are frequently worth testing. These will be used when shrinking your type in higher-order
  * Generators, so it is helpful to have some. Override the `canonicals()` method to return
  * these.
  *
  * Canonicals should always be in order from "smallest" to less-small, in the shrinking sense.
  * This is ''not'' the same thing as starting with the lowest number, though! For example, the
  * canonicals for [[Generator.byteGenerator]] are:
  * {{{
  * private val byteCanonicals: List[Byte] = List(0, 1, -1, 2, -2, 3, -3)
  * }}}
  * Zero is "smallest" -- the most-shrunk Byte.
  *
  * ===Shrinking===
  *
  * Optionally but preferably, your Generator can have a concept of '''shrinking'''. This starts with a value
  * that is known to cause the property evaluation to fail, and produces a list of smaller/simpler
  * values, to see if those also fail. So for example, if a String of length 15 causes a failure, its
  * Generator could try Strings of length 3, and then 1, and then 0, to see if those also cause failure.
  *
  * You to ''not'' have to implement the [[Generator.shrink]] method, but it is helpful to do so when it makes sense;
  * the test system will use that to produce smaller, easier-to-debug examples when something fails.
  *
  * One important rule: the values returned from `shrink` must always be smaller than -- not equal to --
  * the values passed in. Otherwise, an infinite loop can result. Also, similar to Canonicals, the
  * "smallest" values should be returned at the front of this Iterator, with less-small values later.
  *
  * @tparam T the type that this Generator produces
  */
trait Generator[T] { thisGeneratorOfT =>

  /**
    * Prepare a list of edge-case values ("edges") for testing this type.
    *
    * The contents of this list are entirely up to the Generator. It is allowed to be empty, but it is a good
    * idea to think about whether there are appropriate edge cases for this type. (By default, this is empty,
    * so you can get your Generator working first, and think about edge cases after that.)
    *
    * It is common, but not required, to randomize the order of the edge cases here. If so, you should
    * use the [[Randomizer.shuffle]] function for this, so that the order is reproducible if something fails.
    * If you don't use the [[Randomizer]], just return it unchanged as part of the returned tuple.
    *
    * Note the ''maxLength'' parameter. This is the number of tests to be run in total. So the list of
    * returned edges should be no longer than this.
    *
    * @param maxLength the maximum size of the returned List
    * @param rnd the [[Randomizer]] that should be used if you want randomization of the edges
    * @return a Tuple: the list of edges, and the next [[Randomizer]]
    */
  def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[T], Randomizer) = (Nil, rnd)

  /**
    * Produce the next value for this Generator.
    *
    * This is the heart and soul of Generator -- it is the one function that you are required to
    * implement when creating a new one. It takes several fields describing the state of the current
    * evaluation, and returns the next value to try, along with the new state.
    *
    * The state consists of three fields:
    *
    *   - The ''size'' to generate, if that is meaningful for this Generator.
    *   - The ''remaining'' edge cases that need to be generated. In general, if this List
    *     is non-empty, you should simply return the next item on the List.
    *   - The current [[Randomizer]]. If you need to generate random information, use this to
    *     do so.
    *
    * This function returns a Tuple of three fields:
    *
    *   - The next value of type [[T]] to try evaluating.
    *   - The remaining edges ''without'' the one that you are using. That is, if this function
    *     received a non-empty `edges` List, it should usually return the head as the next
    *     value, and the tail as the remainder after that.
    *   - If you used the passed-in [[Randomizer]], return the one you got back from that
    *     function. (Note that all `Randomizer` functions return a new `Randomizer`. If you
    *     didn't use it, just return the one that was passed in.
    *
    * @param szp the size to generate, if that is relevant for this Generator
    * @param edges the remaining edge cases to be tried
    * @param rnd the [[Randomizer]] to use if you need "random" data
    * @return a Tuple of the next value, the remaining edges, and the resulting [[Randomizer]],
    *         as described above.
    */
  def next(szp: SizeParam, edges: List[T], rnd: Randomizer): (RoseTree[T], List[T], Randomizer)

  /**
    * Given a function from types [[T]] to [[U]], return a new [[Generator]] that produces
    * values of type [[U]].
    *
    * For example, say that you needed a Generator that only creates even Ints. We already
    * have [[Generator.intGenerator]], so one way to write this would be:
    * {{{
    *   val evenGen: Generator[Int] = Generator.intGenerator.map { i =>
    *     val mod = i % 2
    *     if ((mod == 1) || (mod == -1))
    *       // It is odd, so the one before it is even:
    *       i - 1
    *     else
    *       // Already even
    *       i
    *   }
    * }}}
    *
    * This often makes it much easier to create a new Generator, if you have an existing
    * one you can base it on.
    *
    * @param f a function from [[T]] to [[U]]
    * @tparam U the type of Generator you want to create
    * @return a new Generator, based on this one and the given transformation function
    */
  def map[U](f: T => U): Generator[U] =
    new Generator[U] { thisGeneratorOfU => 
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[U], Randomizer) = {
        val (listOfT, nextRnd) = thisGeneratorOfT.initEdges(maxLength, rnd)
        (listOfT.map(f), nextRnd)
      }
      def next(szp: SizeParam, edges: List[U], rnd: Randomizer): (RoseTree[U], List[U], Randomizer) = {
        edges match {
          case head :: tail =>
            (Rose(head), tail, rnd)
          case _ =>
            val (nextRoseTreeOfT, _, nextRandomizer) = thisGeneratorOfT.next(szp, Nil, rnd)
            (nextRoseTreeOfT.map(f), Nil, nextRandomizer)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[U], Randomizer) = {
        val (cansOfT, nextRnd) = thisGeneratorOfT.canonicals(rnd)
        (cansOfT.map(f), nextRnd)
      }
    }

  /**
    * The usual Monad function, to allow Generators to be composed together.
    *
    * This is primarily here to support the ability to create new Generators easily using
    * for comprehensions. For example, say that you needed a Generator that produces a Tuple
    * of an Int and a Float. You can write that easily:
    *
    * {{{
    *   val tupleGen =
    *     for {
    *       a <- Generator.intGenerator
    *       b <- Generator.floatGenerator
    *     }
    *       yield (a, b)
    * }}}
    *
    * That is, flatMap takes a function that returns a Generator, and combines it with ''this''
    * Generator, to produce a new Generator. That function ''may'' make use of a value from
    * this Generator (that is part of the standard contract of `flatMap`), but usually does not.
    *
    * @param f a function that takes a value from this Generator, and returns another Generator
    * @tparam U the type produced by the other Generator
    * @return a Generator that is this one and the other one, composed together
    */
  def flatMap[U](f: T => Generator[U]): Generator[U] = {
    new Generator[U] {
      thisGeneratorOfU =>
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[U], Randomizer) = {
        val (listOfT, nextRnd) = thisGeneratorOfT.initEdges(maxLength, rnd)
        val listOfGenOfU: List[Generator[U]] = listOfT.map(f)
        // We only want at most maxLength edges. In cases where we are composing many generators,
        // The total space of the combinations can get huge. We want to stop as soon as we reach
        // maxLength, but we want over time to include points from the entire space of
        // possible edges. So we shuffle the list of generators before we use them. That way from
        // run to run, different generators can get used.
        val (shuffledListOfGenOfU, nextNextRnd): (List[Generator[U]], Randomizer) =
          if (listOfGenOfU.lengthCompare(1) > 1)
            Randomizer.shuffle(listOfGenOfU, nextRnd)
          else
            (listOfGenOfU, nextRnd)
        val (listOfU, nextNextNextRnd): (List[U], Randomizer) = {
          @tailrec
          def loop(remainingGenOfU: List[Generator[U]], nRnd: Randomizer, acc: Set[U]): (List[U], Randomizer) = {
            val accSize = acc.size
            if (accSize >= maxLength.value) {
              val accList = acc.toList
              val (shuffledListOfU, nnRnd) =
                if (accSize > maxLength.value) {
                  // To try and touch all the possibilities over time, if the List from which we are about
                  // to take maxLength is greater than maxLength, we will shuffle the list prior to the take.
                  Randomizer.shuffle(accList, nRnd)
                }
                else
                  (accList, nRnd)
              (shuffledListOfU.take(maxLength), nnRnd)
            }
            else
              remainingGenOfU match {
                case head :: tail =>
                  val (listOfU, nnRnd) = head.initEdges(maxLength, nRnd)
                  loop(tail, nnRnd, acc ++ listOfU)
                case _ => (acc.toList, nRnd)
              }
          }

          loop(listOfGenOfU, nextRnd, Set.empty)
        }
        (listOfU, nextNextNextRnd)
      }

      def next(szp: SizeParam, edges: List[U], rnd: Randomizer): (RoseTree[U], List[U], Randomizer) = {
        edges match {
          case head :: tail =>
            (Rose(head), tail, rnd)
          case _ =>
            val (nextRoseTreeOfT, _, nextRandomizer) = thisGeneratorOfT.next(szp, Nil, rnd)
            val genOfU: Generator[U] = f(nextRoseTreeOfT.value)
            val (u, _, nextNextRandomizer) = genOfU.next(szp, Nil, nextRandomizer)
            (u, Nil, nextNextRandomizer)
        }
      }

      override def canonicals(rnd: Randomizer): (Iterator[U], Randomizer) = {
        val (cansOfT, rnd1) = thisGeneratorOfT.canonicals(rnd)
        var currentRnd = rnd1 // Local var, one thread; TODO: Do this with a tailrec loop
        def getCanonicals(o: T): Iterator[U] = {
          val genOfU: Generator[U] = f(o)
          val (canonicals, nextRnd) = genOfU.canonicals(currentRnd)
          currentRnd = nextRnd
          canonicals
        }

        (cansOfT.flatMap(getCanonicals), currentRnd)
      }
    }
  }

  /**
    * Support for filtering in for comprehensions.
    *
    * This means the same thing is does for all standard Scala Monads: it applies a filter function
    * to this Generator. If you use an `if` clause in a `for` comprehension, this is the function
    * that will be called.
    *
    * The default implementation of this has a safety check, such that if an enormous number of values
    * (100000 by default) are rejected by the filter in a row, it aborts in order to prevent infinite loops.
    * If this occurs, you should probably rewrite your generator to not use a filter.
    *
    * You generally should not need to override this.
    *
    * @param f the actual filter function, which takes a value of type T and says whether to include it or not
    * @return a Generator that only produces values that pass this filter.
    */
  def withFilter(f: T => Boolean): Generator[T] = filter(f)

  /**
    * Support for filtering in for comprehensions.
    *
    * This means the same thing is does for all standard Scala Monads: it applies a filter function
    * to this Generator. If you use an `if` clause in a `for` comprehension, this is the function
    * that will be called.
    *
    * It is closely related to [[Generator.withFilter]], but is the older form.
    *
    * The default implementation of this has a safety check, such that if an enormous number of values
    * (100000 by default) are rejected by the filter in a row, it aborts in order to prevent infinite loops.
    * If this occurs, you should probably rewrite your generator to not use a filter.
    *
    * You generally should not need to override this.
    *
    * @param f the actual filter function, which takes a value of type T and says whether to include it or not
    * @return a Generator that only produces values that pass this filter.
    */
  def filter(f: T => Boolean): Generator[T] =
    new Generator[T] { thisFilteredGeneratorOfT =>
      private final val MaxLoopCount: Int = 100000
      def next(szp: SizeParam, edges: List[T], rnd: Randomizer): (RoseTree[T], List[T], Randomizer) = {
        @tailrec
        def loop(count: Int, nextEdges: List[T], nextRnd: Randomizer): (RoseTree[T], List[T], Randomizer) = {
          if (count > MaxLoopCount)
            throw new IllegalStateException(s"A Generator produced by calling filter or withFilter on another Generator (possibly by using an 'if' clause in a for expression) has filtered out $MaxLoopCount objects in a row in its next method, so aborting. Please define the Generator without using filter or withFilter.")
          val candidateResult = thisGeneratorOfT.next(szp, nextEdges, nextRnd)
          val (nextRoseTreeOfT, nextNextEdges, nextNextRnd) = candidateResult
          val nextT = nextRoseTreeOfT.value
          if (!(f(nextT))) loop(count + 1, nextNextEdges, nextNextRnd)
          else {
            val (roseTreeOfT, _, lastRnd) = thisGeneratorOfT.next(SizeParam(1, 0, 1), List(nextT), nextNextRnd)
            (roseTreeOfT, nextNextEdges, lastRnd)
          }
        }
        loop(0, edges, rnd)
      }
    }

// XXX
  /**
    * Some simple, "ordinary" values of type [[T]].
    *
    * [[canonicals]] are used for certain higher-order functions, mainly during [[shrink]].
    * For example, when the system is trying to simplify a `List[T]`, it will look for
    * canonical values of [[T]] to try putting into that simpler list, to see if that still
    * causes the property to fail.
    *
    * For example, a few of the common types provide these canonicals:
    *
    *   - `Int`: 0, 1, -1, 2, -2, 3, -3
    *   - `Char`: the letters and digits
    *   - `String`: single-charactor Strings of the letter and digits
    *
    * You do not have to provide canonicals for a Generator. By default, this simply
    * returns an empty [[Iterator]].
    *
    * This function takes a [[Randomizer]] to use as a parameter, in case canonical generation
    * for this type has a random element to it. If you use this [[Randomizer]], return the
    * ''next'' one. If you don't use it, just use the passed-in one.
    *
    * @param rnd a [[Randomizer]] to use if this function requires any random data
    * @return the canonical values for this type (if any), and the next [[Randomizer]]
    */
  def canonicals(rnd: Randomizer): (Iterator[T], Randomizer) = (Iterator.empty, rnd)

  /**
    * Fetch a generated value of type [[T]].
    *
    * [[sample]] allows you to experiment with this [[Generator]] in a convenient, ad-hoc way.
    * Each time you call it, it will create a new [[Randomizer]] and a random size, and
    * then calls [[next]] to generate a value.
    *
    * You should not need to override this method; it is here to let you play with your
    * Generator as you build it, and see what sort of values are actually coming out.
    *
    * @return a generated value of type [[T]]
    */
  final def sample: T = {
    val rnd = Randomizer.default
    val maxSize = PosZInt(100)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 100, inclusive
    val (roseTree, _, _) = next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    roseTree.value
  }

  /**
    * Generate a number of values of type [[T]].
    *
    * This is essentially the same as [[sample]], and all the same comments apply, but this
    * will generate as many values as you ask for.
    *
    * @param length the number of values to generate
    * @return a List of size `length`, of randomly-generated values
    */
  final def samples(length: PosInt): List[T] = {
    @tailrec
    def loop(count: Int, rnd: Randomizer, acc: List[T]): List[T] = {
      if (count == length.value) acc
      else {
        val maxSize = PosZInt(100)
        val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 100, inclusive
        val (roseTree, _, nextNextRnd) = next(SizeParam(PosZInt(0), maxSize, size), Nil, rnd)
        loop(count + 1, nextNextRnd, roseTree.value :: acc)
      }
    }
    loop(0, Randomizer.default, Nil)
  }
}

/**
  * Companion to the [[Generator]] trait, which contains many of the standard implicit Generators.
  *
  * For the most part, you should not need to use the values and functions in here directly; the useful
  * values in here are generally aliased in [[CommonGenerators]] (albeit with different names),
  * which in turn is mixed into [[GeneratorDrivenPropertyChecks]] and [[TableDrivenPropertyChecks]].
  *
  * Note that this provides `Generator`s for the common Scalactic types, as well as the common standard
  * library ones.
  */
object Generator {

  import scala.language.implicitConversions

  /**
    * Allow Generators of a type to be used as Generators of a supertype.
    *
    * Given:
    *
    *   - You have a type `T`
    *   - `T` has a supertype `U`
    *   - You have a [[Generator]] that produces values of `T`
    *
    * This allows you to pass that `Generator[T]` as a `Generator[U]`.
    *
    * We do this instead of making [[Generator]] covariant, because then an implicit search for a
    * `Generator[U]` would always be satisfied if it found just a `Generator[T]`, and would then not
    * generate anything except the subtype. That would be sound, but you wouldn't get a good variety of
    * supertype values. This way, the subtype/supertype conversion is somewhat better-controlled.
    *
    * @param genOfT a [[Generator]] that produces values of [[T]]
    * @param ev implicit evidence that [[T]] is a subtype of [[U]]
    * @tparam T the subtype that we have a [[Generator]] for
    * @tparam U the supertype that we want a [[Generator]] for
    * @return a `Generator[U]` derived from the `Generator[T]`
    */
  implicit def widen[T, U](genOfT: Generator[T])(implicit ev: T <:< U): Generator[U] = genOfT.map(o => (o: U))

  ///////////////////////////////////////
  //
  // The edge cases for the standard Generators. If we add more standard Generators, place their edges here.
  //
  // TBD: should canonicals also be listed here? It seems a bit asymmetrical that edges and canonicals are
  // handled differently.
  //

  private[prop] val byteEdges = List(Byte.MinValue, -1.toByte, 0.toByte, 1.toByte, Byte.MaxValue)
  private[prop] val shortEdges = List(Short.MinValue, -1.toShort, 0.toShort, 1.toShort, Short.MaxValue)
  private[prop] val charEdges = List(Char.MinValue, Char.MaxValue)
  private[prop] val intEdges = List(Int.MinValue, -1, 0, 1, Int.MaxValue)
  private[prop] val longEdges = List(Long.MinValue, -1, 0, 1, Long.MaxValue)
  private[prop] val floatEdges = List(Float.NegativeInfinity, Float.MinValue, -1.0F, -Float.MinPositiveValue, -0.0F, 0.0F, Float.MinPositiveValue, 1.0F, Float.MaxValue, Float.PositiveInfinity)
  private[prop] val doubleEdges = List(Double.NegativeInfinity, Double.MinValue, -1.0, -Double.MinPositiveValue, -0.0, 0.0, Double.MinPositiveValue, 1.0, Double.MaxValue, Double.PositiveInfinity)
  private[prop] val posIntEdges = List(PosInt(1), PosInt.MaxValue)
  private[prop] val posZIntEdges = List(PosZInt(0), PosZInt(1), PosZInt.MaxValue)
  private[prop] val posLongEdges = List(PosLong(1L), PosLong.MaxValue)
  private[prop] val posZLongEdges = List(PosZLong(0L), PosZLong(1L), PosZLong.MaxValue)
  private[prop] val posFloatEdges = List(PosFloat.MinPositiveValue, PosFloat(1.0f), PosFloat.MaxValue, PosFloat.PositiveInfinity)
  private[prop] val posZFloatEdges = List(PosZFloat(-0.0f), PosZFloat(0.0f), PosZFloat.MinPositiveValue, PosZFloat(1.0f), PosZFloat.MaxValue, PosZFloat.PositiveInfinity)
  private[prop] val posFiniteFloatEdges = List(PosFiniteFloat.MinValue, PosFiniteFloat(1.0f), PosFiniteFloat.MaxValue)
  private[prop] val posZFiniteFloatEdges = List(PosZFiniteFloat(-0.0f), PosZFiniteFloat(0.0f), PosZFiniteFloat.MinPositiveValue, PosZFiniteFloat(1.0f), PosZFiniteFloat.MaxValue)
  private[prop] val posDoubleEdges = List(PosDouble.MinPositiveValue, PosDouble(1.0f), PosDouble.MaxValue, PosDouble.PositiveInfinity)
  private[prop] val posFiniteDoubleEdges = List(PosFiniteDouble.MinValue, PosFiniteDouble(1.0), PosFiniteDouble.MaxValue)
  private[prop] val finiteDoubleEdges = List(FiniteDouble.MinValue, FiniteDouble(-1.0), FiniteDouble.ensuringValid(-FiniteDouble.MinPositiveValue), FiniteDouble(0.0), FiniteDouble.MinPositiveValue, FiniteDouble(1.0), FiniteDouble.MaxValue)
  private[prop] val finiteFloatEdges = List(FiniteFloat.MinValue, FiniteFloat(-1.0F), FiniteFloat.ensuringValid(-FiniteFloat.MinPositiveValue), FiniteFloat(0.0F), FiniteFloat.MinPositiveValue, FiniteFloat(1.0F), FiniteFloat.MaxValue)
  private[prop] val posZDoubleEdges = List(PosZDouble(-0.0), PosZDouble(0.0), PosZDouble.MinPositiveValue, PosZDouble(1.0), PosZDouble.MaxValue, PosZDouble.PositiveInfinity)
  private[prop] val posZFiniteDoubleEdges = List(PosZFiniteDouble(-0.0), PosZFiniteDouble(0.0), PosZFiniteDouble.MinPositiveValue, PosZFiniteDouble(1.0), PosZFiniteDouble.MaxValue)
  private[prop] val nonZeroDoubleEdges = List(NonZeroDouble.NegativeInfinity, NonZeroDouble.MinValue, NonZeroDouble(-1.0), -NonZeroDouble.MinPositiveValue, NonZeroDouble.MinPositiveValue, NonZeroDouble(1.0), NonZeroDouble.MaxValue, NonZeroDouble.PositiveInfinity)
  private[prop] val nonZeroFiniteDoubleEdges = List(NonZeroFiniteDouble.MinValue, NonZeroFiniteDouble(-1.0), NonZeroFiniteDouble.ensuringValid(-NonZeroFiniteDouble.MinPositiveValue), NonZeroFiniteDouble.MinPositiveValue, NonZeroFiniteDouble(1.0), NonZeroFiniteDouble.MaxValue)
  private[prop] val nonZeroFloatEdges = List(NonZeroFloat.NegativeInfinity, NonZeroFloat.MinValue, NonZeroFloat(-1.0F), -NonZeroFloat.MinPositiveValue, NonZeroFloat.MinPositiveValue, NonZeroFloat(1.0F), NonZeroFloat.MaxValue, NonZeroFloat.PositiveInfinity)
  private[prop] val nonZeroFiniteFloatEdges = List(NonZeroFiniteFloat.MinValue, NonZeroFiniteFloat(-1.0F), NonZeroFiniteFloat.ensuringValid(-NonZeroFiniteFloat.MinPositiveValue), NonZeroFiniteFloat.MinPositiveValue, NonZeroFiniteFloat(1.0F), NonZeroFiniteFloat.MaxValue)
  private[prop] val nonZeroIntEdges = List(NonZeroInt.MinValue, NonZeroInt(-1), NonZeroInt(1), NonZeroInt.MaxValue)
  private[prop] val nonZeroLongEdges = List(NonZeroLong.MinValue, NonZeroLong(-1L), NonZeroLong(1L), NonZeroLong.MaxValue)
  private[prop] val negDoubleEdges = List(NegDouble.MaxValue, NegDouble(-1.0), NegDouble.MinValue, NegDouble.NegativeInfinity)
  private[prop] val negFiniteDoubleEdges = List(NegFiniteDouble.MaxValue, NegFiniteDouble(-1.0), NegFiniteDouble.MinValue)

  private[prop] val negFloatEdges = List(NegFloat.MaxValue, NegFloat(-1.0f), NegFloat.MinValue, NegFloat.NegativeInfinity)
  private[prop] val negFiniteFloatEdges = List(NegFiniteFloat.MaxValue, NegFiniteFloat(-1.0f), NegFiniteFloat.MinValue)

  private[prop] val negIntEdges = List(NegInt.MinValue, NegInt.MaxValue)
  private[prop] val negLongEdges = List(NegLong.MinValue, NegLong.MaxValue)
  private[prop] val negZDoubleEdges = List(NegZDouble(0.0f), NegZDouble(-0.0f), NegZDouble.ensuringValid(-Double.MinPositiveValue), NegZDouble(-1.0), NegZDouble.MinValue, NegZDouble.NegativeInfinity)
  private[prop] val negZFiniteDoubleEdges = List(NegZFiniteDouble(0.0), NegZFiniteDouble(-0.0), NegZFiniteDouble.ensuringValid(-Double.MinPositiveValue), NegZFiniteDouble(-1.0), NegZFiniteDouble.MinValue)
  private[prop] val negZFloatEdges = List(NegZFloat(0.0f), NegZFloat(-0.0f), NegZFloat.ensuringValid(-Float.MinPositiveValue), NegZFloat(-1.0F), NegZFloat.MinValue, NegZFloat.NegativeInfinity)
  private[prop] val negZFiniteFloatEdges = List(NegZFiniteFloat(0.0f), NegZFiniteFloat(-0.0f), NegZFiniteFloat.ensuringValid(-Float.MinPositiveValue), NegZFiniteFloat(-1.0F), NegZFiniteFloat.MinValue)
  private[prop] val negZIntEdges = List(NegZInt.MinValue, NegZInt(-1), NegZInt.MaxValue)
  private[prop] val negZLongEdges = List(NegZLong.MinValue, NegZLong(-1L), NegZLong.MaxValue)
  private[prop] val numericCharEdges = List(NumericChar('0'), NumericChar('9'))

  ///////////////////////////////////////
  //
  // The standard Generators for central standard library and Scalactic types
  //
  // TBD: we might want to refactor these. There's a ton of boilerplate here, and most of it could probably be
  // reduced to a common function with a type parameter and about half a dozen function parameters, I believe. That would
  // likely be easier to understand and maintain. It's probably worth trying and seeing how it works. (Note: I'm not
  // suggesting changing the signatures of any of these, just merging their implementations.)
  //

  /**
    * A [[Generator]] that produces [[Boolean]] values.
    */
  implicit val booleanGenerator: Generator[Boolean] =
    new Generator[Boolean] {
      def next(szp: SizeParam, edges: List[Boolean], rnd: Randomizer): (RoseTree[Boolean], List[Boolean], Randomizer) = {
        val (bit, nextRnd) = rnd.nextBit
        val bool = if (bit == 1) true else false
        (Rose(bool), Nil, nextRnd)
      }

      override def toString = "Generator[Boolean]"
    }

  /**
    * A [[Generator]] that produces [[Byte]] values.
    */
  implicit val byteGenerator: Generator[Byte] =
    new Generator[Byte] {

      case class NextRoseTree(value: Byte) extends RoseTree[Byte] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Byte]], Randomizer) = {
          def resLazyList(theValue: Byte): LazyListOrStream[RoseTree[Byte]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Byte = (theValue / 2).toByte
              if (half == 0) Rose(0.toByte) #:: LazyListOrStream.empty
              else NextRoseTree((-half).toByte) #:: NextRoseTree(half) #:: resLazyList(half)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Byte], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(byteEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Byte], rnd: Randomizer): (RoseTree[Byte], List[Byte], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case _ =>
            val (b, rnd2) = rnd.nextByte
            (NextRoseTree(b), Nil, rnd2)
        }
      }
      private val byteCanonicals: List[Byte] = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Byte], Randomizer) = (byteCanonicals.iterator, rnd)
      override def toString = "Generator[Byte]"
    }

  /**
    * A [[Generator]] that produces [[Short]] values.
    */
  implicit val shortGenerator: Generator[Short] =
    new Generator[Short] {

      case class NextRoseTree(value: Short) extends RoseTree[Short] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Short]], Randomizer) = {
          def resLazyList(theValue: Short): LazyListOrStream[RoseTree[Short]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Short = (theValue / 2).toShort
              if (half == 0) Rose(0.toShort) #:: LazyListOrStream.empty
              else NextRoseTree((-half).toShort) #:: NextRoseTree(half) #:: resLazyList(half)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Short], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(shortEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Short], rnd: Randomizer): (RoseTree[Short], List[Short], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case _ =>
            val (s, rnd2) = rnd.nextShort
            (NextRoseTree(s), Nil, rnd2)
        }
      }
      private val shortCanonicals: List[Short] = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Short], Randomizer) = (shortCanonicals.iterator, rnd)
      override def toString = "Generator[Short]"
    }

  /**
    * A [[Generator]] that produces [[Char]] values.
    */
  implicit val charGenerator: Generator[Char] =
    new Generator[Char] {

      case class NextRoseTree(value: Char) extends RoseTree[Char] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Char]], Randomizer) = {
          val userFriendlyChars = "9876543210ZYXWVUTSRQPONMLKJIHGFEDCBAzyxwvutsrqponmljkihgfedcba"
          if (userFriendlyChars.indexOf(value) >= 0) (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            def resLazyList(theIndex: Int): LazyListOrStream[RoseTree[Char]] = {
              if (theIndex == userFriendlyChars.length) LazyListOrStream.empty
              else NextRoseTree(userFriendlyChars(theIndex)) #:: resLazyList(theIndex + 1)
            }
            (resLazyList(0), rndPassedToShrinks)
          }
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Char], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(charEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Char], rnd: Randomizer): (RoseTree[Char], List[Char], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case _ =>
            val (c, rnd2) = rnd.nextChar
            (NextRoseTree(c), Nil, rnd2)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[Char], Randomizer) = {
        val lowerAlphaChars = "abcdefghikjlmnopqrstuvwxyz"
        val upperAlphaChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        val numericChars = "0123456789"
        val (lowerCharIndex, rnd1) = rnd.chooseInt(0, lowerAlphaChars.length - 1)
        val (upperCharIndex, rnd2) = rnd1.chooseInt(0, upperAlphaChars.length - 1)
        val (numericCharIndex, rnd3) = rnd1.chooseInt(0, numericChars.length - 1)
        val lowerChar = lowerAlphaChars(lowerCharIndex)
        val upperChar = upperAlphaChars(upperCharIndex)
        val numericChar = numericChars(numericCharIndex)
        (Iterator(lowerChar, upperChar, numericChar), rnd3)
      }
      override def toString = "Generator[Char]"
    }

  /**
    * A [[Generator]] that produces [[Int]] values.
    */
  implicit val intGenerator: Generator[Int] =
    new Generator[Int] {

      case class NextRoseTree(value: Int) extends RoseTree[Int] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Int]], Randomizer) = {
          def resLazyList(theValue: Int): LazyListOrStream[RoseTree[Int]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Int = theValue / 2
              if (half == 0) Rose(0) #:: LazyListOrStream.empty
              else NextRoseTree(-half) #:: NextRoseTree(half) #:: resLazyList(half)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Int], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(intEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Int], rnd: Randomizer): (RoseTree[Int], List[Int], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (i, rnd2) = rnd.nextInt
            (NextRoseTree(i), Nil, rnd2)
        }
      }
      override def toString = "Generator[Int]"
      private val intCanonicals = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Int], Randomizer) = (intCanonicals.iterator, rnd)
    }

  /**
    * A [[Generator]] that produces [[Long]] values.
    */
  implicit val longGenerator: Generator[Long] =
    new Generator[Long] {

      case class NextRoseTree(value: Long) extends RoseTree[Long] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Long]], Randomizer) = {
          def resLazyList(theValue: Long): LazyListOrStream[RoseTree[Long]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Long = (theValue / 2)
              if (half == 0) Rose(0L) #:: LazyListOrStream.empty
              else NextRoseTree(-half) #:: NextRoseTree(half) #:: resLazyList(half)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Long], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(longEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[Long], rnd: Randomizer): (RoseTree[Long], List[Long], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (n, rnd2) = rnd.nextLong
            (NextRoseTree(n), Nil, rnd2)
        }
      }
      private val longCanonicals: List[Long] = List(0, 1, -1, 2, -2, 3, -3)
      override def canonicals(rnd: Randomizer): (Iterator[Long], Randomizer) = (longCanonicals.iterator, rnd)
      override def toString = "Generator[Long]"
    }

  /**
    * A [[Generator]] that produces [[Float]] values.
    */
  implicit val floatGenerator: Generator[Float] =
    new Generator[Float] {

      case class NextRoseTree(value: Float) extends RoseTree[Float] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Float]], Randomizer) = {
          def resLazyList(theValue: Float): LazyListOrStream[RoseTree[Float]] = {
            if (theValue == 0.0f)
              LazyListOrStream.empty
            else if (theValue <= 1.0f && theValue >= -1.0f)
              Rose(0.0f) #:: LazyListOrStream.empty
            else if (!theValue.isWhole) {
              // We need to handle infinity and NaN specially because without it, this method
              // will go into an infinite loop. The reason is floor and ciel give back the same value
              // on these values:
              //
              // scala> val f = Float.PositiveInfinity
              // f: Float = Infinity
              //
              // scala> f.floor
              // res1: Float = Infinity
              //
              // scala> f.ceil
              // res3: Float = Infinity
              //
              // scala> Float.NaN.floor
              // res5: Float = NaN
              //
              // scala> Float.NaN.ceil
              // res6: Float = NaN
              //
              val n =
                if (theValue == Float.PositiveInfinity || theValue.isNaN)
                  Float.MaxValue
                else if (theValue == Float.NegativeInfinity)
                  Float.MinValue
                else theValue
              // Nearest whole numbers closer to zero. We'll try both negative and positive of this,
              // and will put the negative number first, because the positive number is simpler for
              // humans to look at. So if both the negative and positive number fail the test, the
              // positive number will be considered the most shrunk.
              val (nearest, nearestNeg) = if (n > 0.0f) (n.floor, (-n).ceil) else ((-n).floor, n.ceil)
              NextRoseTree(nearestNeg) #:: NextRoseTree(nearest) #:: resLazyList(nearest)
            }  
            else {
              val sqrt: Float = math.sqrt(theValue.abs.toDouble).toFloat
              if (sqrt < 1.0f && sqrt >= -1.0) 
                Rose(0.0f) #:: LazyListOrStream.empty
              else {
                // Try both the negative and postive, negative first because positive is simpler for humans,
                // so more "shrunk."
                val whole: Float = sqrt.floor
                val negWhole: Float = -whole // math.rint((-whole).toDouble).toFloat
                NextRoseTree(negWhole) #:: NextRoseTree(whole) #:: resLazyList(whole)
              }
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Float], Randomizer) = {
        (floatEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[Float], rnd: Randomizer): (RoseTree[Float], List[Float], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (f, rnd2) = rnd.nextFloat
            (NextRoseTree(f), Nil, rnd2)
        }
      }
      private val floatCanonicals: List[Float] = List(0.0f, 1.0f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f)
      override def canonicals(rnd: Randomizer): (Iterator[Float], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[Float]"
    }

  /**
    * A [[Generator]] that produces [[Double]] values.
    */
  implicit val doubleGenerator: Generator[Double] =
    new Generator[Double] {

      case class NextRoseTree(value: Double) extends RoseTree[Double] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Double]], Randomizer) = {
          def resLazyList(theValue: Double): LazyListOrStream[RoseTree[Double]] = {

            if (theValue == 0.0)
              LazyListOrStream.empty
            else if (theValue <= 1.0 && theValue >= -1.0)
              Rose(0.0) #:: LazyListOrStream.empty
            else if (!theValue.isWhole) {
              // We need to handle infinity and NaN specially because without it, this method
              // will go into an infinite loop. The reason is floor and ciel give back the same value
              // on these values:
              //
              // scala> val n = Double.PositiveInfinity
              // n: Double = Infinity
              //
              // scala> n.floor
              // res0: Double = Infinity
              //
              // scala> n.ceil
              // res1: Double = Infinity
              //
              // scala> Double.NaN.floor
              // res3: Double = NaN
              //
              // scala> Double.NaN.ceil
              // res4: Double = NaN
              //
              val n =
                if (theValue == Double.PositiveInfinity || theValue.isNaN)
                  Double.MaxValue
                else if (theValue == Double.NegativeInfinity)
                  Double.MinValue
                else theValue
              // Nearest whole numbers closer to zero. We'll try both negative and positive of this,
              // and will put the negative number first, because the positive number is simpler for
              // humans to look at. So if both the negative and positive number fail the test, the
              // positive number will be considered the most shrunk.
              val (nearest, nearestNeg) = if (n > 0.0) (n.floor, (-n).ceil) else ((-n).floor, n.ceil)
              NextRoseTree(nearestNeg) #:: NextRoseTree(nearest) #:: resLazyList(nearest)
            }  
            else {
              val sqrt: Double = math.sqrt(theValue.abs)
              if (sqrt < 1.0 && sqrt >= -1.0) 
                Rose(0.0) #:: LazyListOrStream.empty
              else {
                // Try both the negative and postive, negative first because positive is simpler for humans,
                // so more "shrunk."
                val whole: Double = sqrt.floor
                val negWhole: Double = -whole // math.rint((-whole).toDouble).toDouble
                NextRoseTree(negWhole) #:: NextRoseTree(whole) #:: resLazyList(whole)
              }
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Double], Randomizer) = {
        (doubleEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[Double], rnd: Randomizer): (RoseTree[Double], List[Double], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (d, rnd2) = rnd.nextDouble
            (NextRoseTree(d), Nil, rnd2)
        }
      }
      private val doubleCanonicals: List[Double] = List(0.0, 1.0, -1.0, 2.0, -2.0, 3.0, -3.0)
      override def canonicals(rnd: Randomizer): (Iterator[Double], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[Double]"
    }

  /**
    * A [[Generator]] that produces positive integers, excluding zero.
    */
  implicit val posIntGenerator: Generator[PosInt] =
    new Generator[PosInt] {

      case class NextRoseTree(value: PosInt) extends RoseTree[PosInt] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosInt]], Randomizer) = {
          def resLazyList(theValue: PosInt): LazyListOrStream[RoseTree[PosInt]] = {
            val half = theValue / 2
            if (half == 0) LazyListOrStream.empty
            else {
              val posIntHalf = PosInt.ensuringValid(half)
              NextRoseTree(posIntHalf) #:: resLazyList(posIntHalf)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosInt], rnd: Randomizer): (RoseTree[PosInt], List[PosInt], Randomizer) = {
        edges match {
          case head :: tail =>
             (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posInt, rnd2) = rnd.nextPosInt
            (NextRoseTree(posInt), Nil, rnd2)
        }
      }
      private val posIntCanonicals = List(1, 2, 3).map(PosInt.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosInt], Randomizer) = (posIntCanonicals.iterator, rnd)
      override def toString = "Generator[PosInt]"
    }

  /**
    * A [[Generator]] that produces positive integers, including zero.
    */
  implicit val posZIntGenerator: Generator[PosZInt] =
    new Generator[PosZInt] {

      case class NextRoseTree(value: PosZInt) extends RoseTree[PosZInt] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosZInt]], Randomizer) = {
          def resLazyList(theValue: PosZInt): LazyListOrStream[RoseTree[PosZInt]] = {
            if (theValue.value == 0) LazyListOrStream.empty
            else {
              val half: Int = theValue / 2
              val posZIntHalf = PosZInt.ensuringValid(half)
              NextRoseTree(posZIntHalf) #:: resLazyList(posZIntHalf)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZInt], rnd: Randomizer): (RoseTree[PosZInt], List[PosZInt], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posZInt, rnd2) = rnd.nextPosZInt
            (NextRoseTree(posZInt), Nil, rnd2)
        }
      }
      private val posZIntCanonicals = List(0, 1, 2, 3).map(PosZInt.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosZInt], Randomizer) = (posZIntCanonicals.iterator, rnd)
      override def toString = "Generator[PosZInt]"
    }

  /**
    * A [[Generator]] that produces positive Longs, excluding zero.
    */
  implicit val posLongGenerator: Generator[PosLong] =
    new Generator[PosLong] {

      case class NextRoseTree(value: PosLong) extends RoseTree[PosLong] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosLong]], Randomizer) = {

          def resLazyList(theValue: PosLong): LazyListOrStream[RoseTree[PosLong]] = {
            val half = theValue / 2
            if (half == 0) LazyListOrStream.empty
            else {
              val posLongHalf = PosLong.ensuringValid(half)
              NextRoseTree(posLongHalf) #:: resLazyList(posLongHalf)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosLong], rnd: Randomizer): (RoseTree[PosLong], List[PosLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posLong, rnd2) = rnd.nextPosLong
            (NextRoseTree(posLong), Nil, rnd2)
        }
      }
      private val posLongCanonicals = List(1, 2, 3).map(PosLong.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosLong], Randomizer) = (posLongCanonicals.iterator, rnd)
      override def toString = "Generator[PosLong]"
    }

  /**
    * A [[Generator]] that produces positive Longs, including zero.
    */
  implicit val posZLongGenerator: Generator[PosZLong] =
    new Generator[PosZLong] {
      
      case class NextRoseTree(value: PosZLong) extends RoseTree[PosZLong] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosZLong]], Randomizer) = {
          def resLazyList(theValue: PosZLong): LazyListOrStream[RoseTree[PosZLong]] = {
            if (theValue.value == 0L) LazyListOrStream.empty
            else {
              val half: Long = theValue / 2
              val posZLongHalf = PosZLong.ensuringValid(half)
              NextRoseTree(posZLongHalf) #:: resLazyList(posZLongHalf)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }
      
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZLong], rnd: Randomizer): (RoseTree[PosZLong], List[PosZLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posZLong, rnd2) = rnd.nextPosZLong
            (NextRoseTree(posZLong), Nil, rnd2)
        }
      }
      private val posZLongCanonicals = List(0, 1, 2, 3).map(PosZLong.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosZLong], Randomizer) = (posZLongCanonicals.iterator, rnd)
      override def toString = "Generator[PosZLong]"
    }

  /**
    * A [[Generator]] that produces positive Floats, excluding zero.
    */
  implicit val posFloatGenerator: Generator[PosFloat] =
    new Generator[PosFloat] {

      case class NextRoseTree(value: PosFloat) extends RoseTree[PosFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosFloat]], Randomizer) = {
          def resLazyList(theValue: PosFloat): LazyListOrStream[RoseTree[PosFloat]] = {
            val fv = theValue.value
            if (fv == 1.0f)
              LazyListOrStream.empty
            else if (fv < 1.0f)
              Rose(PosFloat(1.0f)) #:: LazyListOrStream.empty
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.PositiveInfinity || fv.isNaN)
                  Float.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosFloat.ensuringValid(n.floor)
              NextRoseTree(nearest) #:: resLazyList(nearest)
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              val whole = PosFloat.ensuringValid(sqrt.floor)
              NextRoseTree(whole) #:: resLazyList(whole)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosFloat], rnd: Randomizer): (RoseTree[PosFloat], List[PosFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posFloat, rnd2) = rnd.nextPosFloat
            (NextRoseTree(posFloat), Nil, rnd2)
        }
      }
      private val posFloatCanonicals: List[PosFloat] = List(1.0f, 2.0f, 3.0f).map(PosFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosFloat], Randomizer) = (posFloatCanonicals.iterator, rnd)
      override def toString = "Generator[PosFloat]"
    }

  /**
    * A [[Generator]] that produces positive Floats, excluding zero and infinity.
    */
  implicit val posFiniteFloatGenerator: Generator[PosFiniteFloat] =
    new Generator[PosFiniteFloat] {

      case class NextRoseTree(value: PosFiniteFloat) extends RoseTree[PosFiniteFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosFiniteFloat]], Randomizer) = {
          def resLazyList(theValue: PosFiniteFloat): LazyListOrStream[RoseTree[PosFiniteFloat]] = {
            val fv = theValue.value
            if (fv == 1.0f) LazyListOrStream.empty
            else if (fv < 1.0f) Rose(PosFiniteFloat(1.0f)) #:: LazyListOrStream.empty
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosFiniteFloat.ensuringValid(fv.floor)
              NextRoseTree(nearest) #:: resLazyList(nearest)
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              val whole = PosFiniteFloat.ensuringValid(sqrt.floor)
              NextRoseTree(whole) #:: resLazyList(whole)
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosFiniteFloat], rnd: Randomizer): (RoseTree[PosFiniteFloat], List[PosFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posFiniteFloat, rnd2) = rnd.nextPosFiniteFloat
            (NextRoseTree(posFiniteFloat), Nil, rnd2)
        }
      }
      private val posFloatCanonicals: List[PosFiniteFloat] = List(1.0f, 2.0f, 3.0f).map(PosFiniteFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosFiniteFloat], Randomizer) = (posFloatCanonicals.iterator, rnd)
      override def toString = "Generator[PosFiniteFloat]"
    }

  /**
    * A [[Generator]] that produces Floats, excluding infinity.
    */
  implicit val finiteFloatGenerator: Generator[FiniteFloat] =
    new Generator[FiniteFloat] {

      case class NextRoseTree(value: FiniteFloat) extends RoseTree[FiniteFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[FiniteFloat]], Randomizer) = {
          def resLazyList(theValue: FiniteFloat): LazyListOrStream[RoseTree[FiniteFloat]] = {
            val fv = theValue.value
            if (fv == 0.0f) LazyListOrStream.empty
            else if (fv <= 1.0f && fv >= -1.0f) Rose(FiniteFloat(0.0f)) #:: LazyListOrStream.empty
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (fv > 0.0f) (fv.floor, (-fv).ceil) else (fv.ceil, (-fv).floor)
              NextRoseTree(FiniteFloat.ensuringValid(nearestNeg)) #:: NextRoseTree(FiniteFloat.ensuringValid(nearest)) #:: resLazyList(FiniteFloat.ensuringValid(nearest))
            }
            else {
              val sqrt: Float = math.sqrt(fv.abs.toDouble).toFloat
              if (sqrt < 1.0f) Rose(FiniteFloat(0.0f)) #:: LazyListOrStream.empty
              else {
                val whole: Float = sqrt.floor
                val negWhole: Float = math.rint((-whole).toDouble).toFloat
                val (first, second) = if (fv > 0.0f) (negWhole, whole) else (whole, negWhole)
                NextRoseTree(FiniteFloat.ensuringValid(first)) #:: NextRoseTree(FiniteFloat.ensuringValid(second)) #:: resLazyList(FiniteFloat.ensuringValid(first))
              }
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[FiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(finiteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[FiniteFloat], rnd: Randomizer): (RoseTree[FiniteFloat], List[FiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (finiteFloat, rnd2) = rnd.nextFiniteFloat
            (NextRoseTree(finiteFloat), Nil, rnd2)
        }
      }
      private val floatCanonicals: List[FiniteFloat] = List(0.0f, 1.0f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f).map(FiniteFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[FiniteFloat], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[FiniteFloat]"
    }

  /**
    * A [[Generator]] that produces Doubles, excluding infinity.
    */
  implicit val finiteDoubleGenerator: Generator[FiniteDouble] =
    new Generator[FiniteDouble] {

      case class NextRoseTree(value: FiniteDouble) extends RoseTree[FiniteDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[FiniteDouble]], Randomizer) = {
          def resLazyList(theValue: FiniteDouble): LazyListOrStream[RoseTree[FiniteDouble]] = {
            val dv: Double = theValue.value
            if (dv == 0.0) LazyListOrStream.empty
            else if (dv <= 1.0 && dv >= -1.0) Rose(FiniteDouble(0.0)) #:: LazyListOrStream.empty
            else if (!dv.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (dv > 0.0) (dv.floor, (-dv).ceil) else (dv.ceil, (-dv).floor)
               NextRoseTree(FiniteDouble.ensuringValid(nearestNeg)) #:: resLazyList(FiniteDouble.ensuringValid(nearest))
            }
            else {
              val sqrt: Double = math.sqrt(dv.abs)
              if (sqrt < 1.0) Rose(FiniteDouble(0.0)) #:: LazyListOrStream.empty
              else {
                val whole: Double = sqrt.floor
                val negWhole: Double = math.rint((-whole).toDouble)
                val (first, second) = if (dv > 0.0) (negWhole, whole) else (whole, negWhole)
                NextRoseTree(FiniteDouble.ensuringValid(first)) #:: NextRoseTree(FiniteDouble.ensuringValid(second)) #:: resLazyList(FiniteDouble.ensuringValid(first))
              }
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[FiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(finiteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[FiniteDouble], rnd: Randomizer): (RoseTree[FiniteDouble], List[FiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (finiteDouble, rnd2) = rnd.nextFiniteDouble
            (NextRoseTree(finiteDouble), Nil, rnd2)
        }
      }
      private val doubleCanonicals: List[FiniteDouble] = List(0.0, 1.0, -1.0, 2.0, -2.0, 3.0, -3.0).map(FiniteDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[FiniteDouble], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[FiniteDouble]"
    }

  /**
    * A [[Generator]] that produces positive Floats, including zero and infinity.
    */
  implicit val posZFloatGenerator: Generator[PosZFloat] =
    new Generator[PosZFloat] {

      case class NextRoseTree(value: PosZFloat) extends RoseTree[PosZFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosZFloat]], Randomizer) = {
          def resLazyList(theValue: PosZFloat): LazyListOrStream[RoseTree[PosZFloat]] = {
            val fv: Float = theValue.value
            if (fv == 0.0f) LazyListOrStream.empty
            else if (fv <= 1.0f) Rose(PosZFloat(0.0f)) #:: LazyListOrStream.empty
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.PositiveInfinity || fv.isNaN)
                  Float.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosZFloat.ensuringValid(n.floor)
              NextRoseTree(nearest) #:: resLazyList(nearest)
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              if (sqrt < 1.0f) Rose(PosZFloat(0.0f)) #:: LazyListOrStream.empty
              else {
                val whole = PosZFloat.ensuringValid(sqrt.floor)
                NextRoseTree(whole) #:: resLazyList(whole)
              }
            }
          }
          (resLazyList(value), rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZFloat], rnd: Randomizer): (RoseTree[PosZFloat], List[PosZFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posZFloat, rnd2) = rnd.nextPosZFloat
            (NextRoseTree(posZFloat), Nil, rnd2)
        }
      }
      private val floatCanonicals: List[PosZFloat] = List(0.0f, 1.0f, 2.0f, 3.0f).map(PosZFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosZFloat], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[PosZFloat]"
    }

  /**
    * A [[Generator]] that produces positive Floats, including zero but excluding infinity.
    */
  implicit val posZFiniteFloatGenerator: Generator[PosZFiniteFloat] =
    new Generator[PosZFiniteFloat] {

      case class NextRoseTree(value: PosZFiniteFloat) extends RoseTree[PosZFiniteFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosZFiniteFloat]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: PosZFiniteFloat, acc: LazyListOrStream[RoseTree[PosZFiniteFloat]]): LazyListOrStream[RoseTree[PosZFiniteFloat]] = {
            val fv = f.value
            if (fv == 0.0f) acc
            else if (fv <= 1.0f) Rose(PosZFiniteFloat(0.0f)) #:: acc
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosZFiniteFloat.ensuringValid(fv.floor)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              if (sqrt < 1.0f) Rose(PosZFiniteFloat(0.0f)) #:: acc
              else {
                val whole = PosZFiniteFloat.ensuringValid(sqrt.floor)
                shrinkLoop(whole, NextRoseTree(whole) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZFiniteFloat], rnd: Randomizer): (RoseTree[PosZFiniteFloat], List[PosZFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posZFiniteFloat, rnd2) = rnd.nextPosZFiniteFloat
            (NextRoseTree(posZFiniteFloat), Nil, rnd2)
        }
      }
      private val floatCanonicals: List[PosZFiniteFloat] = List(0.0f, 1.0f, 2.0f, 3.0f).map(PosZFiniteFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosZFiniteFloat], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[PosZFiniteFloat]"
    }

  /**
    * A [[Generator]] that produces positive Doubles, excluding zero but including infinity.
    */
  implicit val posDoubleGenerator: Generator[PosDouble] =
    new Generator[PosDouble] {

      case class NextRoseTree(value: PosDouble) extends RoseTree[PosDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: PosDouble, acc: LazyListOrStream[RoseTree[PosDouble]]): LazyListOrStream[RoseTree[PosDouble]] = {
            val fv = f.value
            if (fv == 1.0) acc
            else if (fv < 1.0) Rose(PosDouble(1.0)) #:: acc
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.PositiveInfinity || fv.isNaN)
                  Double.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosDouble.ensuringValid(n.floor)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              val whole = PosDouble.ensuringValid(sqrt.floor)
              shrinkLoop(whole, NextRoseTree(whole) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosDouble], rnd: Randomizer): (RoseTree[PosDouble], List[PosDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posDouble, rnd2) = rnd.nextPosDouble
            (NextRoseTree(posDouble), Nil, rnd2)
        }
      }
      private val posDoubleCanonicals: List[PosDouble] = List(1.0, 2.0, 3.0).map(PosDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosDouble], Randomizer) = (posDoubleCanonicals.iterator, rnd)
      override def toString = "Generator[PosDouble]"
    }

  /**
    * A [[Generator]] that produces positive Doubles, excluding zero and infinity.
    */
  implicit val posFiniteDoubleGenerator: Generator[PosFiniteDouble] =
    new Generator[PosFiniteDouble] {

      case class NextRoseTree(value: PosFiniteDouble) extends RoseTree[PosFiniteDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosFiniteDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: PosFiniteDouble, acc: LazyListOrStream[RoseTree[PosFiniteDouble]]): LazyListOrStream[RoseTree[PosFiniteDouble]] = {
            val fv = f.value
            if (fv == 1.0) acc
            else if (fv < 1.0) Rose(PosFiniteDouble(1.0)) #:: acc
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosFiniteDouble.ensuringValid(fv.floor)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              val whole = PosFiniteDouble.ensuringValid(sqrt.floor)
              shrinkLoop(whole, NextRoseTree(whole) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosFiniteDouble], rnd: Randomizer): (RoseTree[PosFiniteDouble], List[PosFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posFiniteDouble, rnd2) = rnd.nextPosFiniteDouble
            (NextRoseTree(posFiniteDouble), Nil, rnd2)
        }
      }
      private val posDoubleCanonicals: List[PosFiniteDouble] = List(1.0, 2.0, 3.0).map(PosFiniteDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosFiniteDouble], Randomizer) = (posDoubleCanonicals.iterator, rnd)
      override def toString = "Generator[PosFiniteDouble]"
    }

  /**
    * A [[Generator]] that produces positive Doubles, including zero and infinity.
    */
  implicit val posZDoubleGenerator: Generator[PosZDouble] =
    new Generator[PosZDouble] {

      case class NextRoseTree(value: PosZDouble) extends RoseTree[PosZDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosZDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: PosZDouble, acc: LazyListOrStream[RoseTree[PosZDouble]]): LazyListOrStream[RoseTree[PosZDouble]] = {
            val fv = f.value
            if (fv == 0.0) acc
            else if (fv <= 1.0) Rose(PosZDouble(0.0)) #:: acc
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.PositiveInfinity || fv.isNaN)
                  Double.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosZDouble.ensuringValid(n.floor)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              if (sqrt < 1.0) Rose(PosZDouble(0.0)) #:: acc
              else {
                val whole = PosZDouble.ensuringValid(sqrt.floor)
                shrinkLoop(whole, NextRoseTree(whole) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZDouble], rnd: Randomizer): (RoseTree[PosZDouble], List[PosZDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posZDouble, rnd2) = rnd.nextPosZDouble
            (NextRoseTree(posZDouble), Nil, rnd2)
        }
      }
      private val doubleCanonicals: List[PosZDouble] = List(0.0, 1.0, 2.0, 3.0).map(PosZDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosZDouble], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[PosZDouble]"
    }

  /**
    * A [[Generator]] that produces positive Doubles, including zero but excluding infinity.
    */
  implicit val posZFiniteDoubleGenerator: Generator[PosZFiniteDouble] =
    new Generator[PosZFiniteDouble] {

      case class NextRoseTree(value: PosZFiniteDouble) extends RoseTree[PosZFiniteDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[PosZFiniteDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: PosZFiniteDouble, acc: LazyListOrStream[RoseTree[PosZFiniteDouble]]): LazyListOrStream[RoseTree[PosZFiniteDouble]] = {
            val fv = f.value
            if (fv == 0.0) acc
            else if (fv <= 1.0) Rose(PosZFiniteDouble(0.0)) #:: acc
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosZFiniteDouble.ensuringValid(fv.floor)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              if (sqrt < 1.0) Rose(PosZFiniteDouble(0.0)) #:: acc
              else {
                val whole = PosZFiniteDouble.ensuringValid(sqrt.floor)
                shrinkLoop(whole, NextRoseTree(whole) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[PosZFiniteDouble], rnd: Randomizer): (RoseTree[PosZFiniteDouble], List[PosZFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posZFiniteDouble, rnd2) = rnd.nextPosZFiniteDouble
            (NextRoseTree(posZFiniteDouble), Nil, rnd2)
        }
      }
      private val doubleCanonicals: List[PosZFiniteDouble] = List(0.0, 1.0, 2.0, 3.0).map(PosZFiniteDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[PosZFiniteDouble], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[PosZFiniteDouble]"
    }

  /**
    * A [[Generator]] that produces Doubles, excluding zero but including infinity.
    */
  implicit val nonZeroDoubleGenerator: Generator[NonZeroDouble] =
    new Generator[NonZeroDouble] {

      case class NextRoseTree(value: NonZeroDouble) extends RoseTree[NonZeroDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NonZeroDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(raw: NonZeroDouble, acc: LazyListOrStream[RoseTree[NonZeroDouble]]): LazyListOrStream[RoseTree[NonZeroDouble]] = {
            val d = raw.value
            if (d <= 1.0 && d >= -1.0) {
               if (acc.isEmpty)
                 Rose(NonZeroDouble.ensuringValid(-1.0)) #:: Rose(NonZeroDouble.ensuringValid(1.0)) #:: LazyListOrStream.empty
               else acc
            } else if (!d.isWhole) {
              val n =
                if (d == Double.PositiveInfinity || d.isNaN)
                  Double.MaxValue
                else if (d == Double.NegativeInfinity)
                  Double.MinValue
                else d
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (n > 0.0) (n.floor, (-n).ceil) else (n.ceil, (-n).floor)
              shrinkLoop(NonZeroDouble.ensuringValid(nearest), NextRoseTree(NonZeroDouble.ensuringValid(nearestNeg)) #:: NextRoseTree(NonZeroDouble.ensuringValid(nearest)) #:: acc)
            }
            else {
              val sqrt: Double = math.sqrt(d.abs)
              if (sqrt < 1.0) acc
              else {
                val whole: NonZeroDouble = NonZeroDouble.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroDouble = -whole  //math.rint(-whole)
                val (first, second) = if (d > 0.0) (negWhole, whole) else (whole, negWhole)
                shrinkLoop(first, NextRoseTree(first) #:: NextRoseTree(second) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO Why are there no Roses, just NextRoseTrees, in this one?

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroDouble], rnd: Randomizer): (RoseTree[NonZeroDouble], List[NonZeroDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nonZeroDouble, rnd2) = rnd.nextNonZeroDouble
            (NextRoseTree(nonZeroDouble), Nil, rnd2)
        }
      }
      private val doubleCanonicals: List[NonZeroDouble] = List(1.0, -1.0, 2.0, -2.0, 3.0, -3.0).map(NonZeroDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NonZeroDouble], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[NonZeroDouble]"
    }

  /**
    * A [[Generator]] that produces Doubles, excluding zero and infinity.
    */
  implicit val nonZeroFiniteDoubleGenerator: Generator[NonZeroFiniteDouble] =
    new Generator[NonZeroFiniteDouble] {

      case class NextRoseTree(value: NonZeroFiniteDouble) extends RoseTree[NonZeroFiniteDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NonZeroFiniteDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(raw: NonZeroFiniteDouble, acc: LazyListOrStream[RoseTree[NonZeroFiniteDouble]]): LazyListOrStream[RoseTree[NonZeroFiniteDouble]] = {
            val d = raw.value
            if (d <= 1.0 && d >= -1.0) {
               if (acc.isEmpty)
                 Rose(NonZeroFiniteDouble.ensuringValid(-1.0)) #:: Rose(NonZeroFiniteDouble.ensuringValid(1.0)) #:: LazyListOrStream.empty
               else acc
            } else if (!d.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (d > 0.0) (d.floor, (-d).ceil) else (d.ceil, (-d).floor)
              shrinkLoop(NonZeroFiniteDouble.ensuringValid(nearest), NextRoseTree(NonZeroFiniteDouble.ensuringValid(nearestNeg)) #:: NextRoseTree(NonZeroFiniteDouble.ensuringValid(nearest)) #:: acc)
            }
            else {
              val sqrt: Double = math.sqrt(d.abs)
              if (sqrt < 1.0) acc
              else {
                val whole: NonZeroFiniteDouble = NonZeroFiniteDouble.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroFiniteDouble = -whole  //math.rint(-whole)
                val (first, second) = if (d > 0.0) (negWhole, whole) else (whole, negWhole)
                shrinkLoop(first, NextRoseTree(first) #:: NextRoseTree(second) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO: Confirm this is ok without any Roses, just NextRoseTrees.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroFiniteDouble], rnd: Randomizer): (RoseTree[NonZeroFiniteDouble], List[NonZeroFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nonZeroFiniteDouble, rnd2) = rnd.nextNonZeroFiniteDouble
            (NextRoseTree(nonZeroFiniteDouble), Nil, rnd2)
        }
      }
      private val doubleCanonicals: List[NonZeroFiniteDouble] = List(1.0, -1.0, 2.0, -2.0, 3.0, -3.0).map(NonZeroFiniteDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NonZeroFiniteDouble], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[NonZeroFiniteDouble]"
    }

  /**
    * A [[Generator]] that produces Floats, excluding zero but including infinity.
    */
  implicit val nonZeroFloatGenerator: Generator[NonZeroFloat] =
    new Generator[NonZeroFloat] {

      case class NextRoseTree(value: NonZeroFloat) extends RoseTree[NonZeroFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NonZeroFloat]], Randomizer) = {
          @tailrec
          def shrinkLoop(raw: NonZeroFloat, acc: LazyListOrStream[RoseTree[NonZeroFloat]]): LazyListOrStream[RoseTree[NonZeroFloat]] = {
            val d = raw.value
            if (d <= 1.0f && d >= -1.0f) {
               if (acc.isEmpty)
                 Rose(NonZeroFloat.ensuringValid(-1.0f)) #:: Rose(NonZeroFloat.ensuringValid(1.0f)) #:: LazyListOrStream.empty
               else acc
            } else if (!d.isWhole) {
              val n =
                if (d == Float.PositiveInfinity || d.isNaN)
                  Float.MaxValue
                else if (d == Float.NegativeInfinity)
                  Float.MinValue
                else d
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (n > 0.0f) (n.floor, (-n).ceil) else (n.ceil, (-n).floor)
              shrinkLoop(NonZeroFloat.ensuringValid(nearest), NextRoseTree(NonZeroFloat.ensuringValid(nearestNeg)) #:: NextRoseTree(NonZeroFloat.ensuringValid(nearest)) #:: acc)
            }
            else {
              val sqrt: Float = math.sqrt(d.abs.toDouble).toFloat
              if (sqrt < 1.0f) acc
              else {
                val whole: NonZeroFloat = NonZeroFloat.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroFloat = -whole  //math.rint(-whole)
                val (first, second) = if (d > 0.0f) (negWhole, whole) else (whole, negWhole)
                shrinkLoop(first, NextRoseTree(first) #:: NextRoseTree(second) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO confirm no roses needed

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroFloat], rnd: Randomizer): (RoseTree[NonZeroFloat], List[NonZeroFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nonZeroFloat, rnd2) = rnd.nextNonZeroFloat
            val (roseTreeOfNonZeroFloat, rnd3) = (NextRoseTree(nonZeroFloat), rnd2)
            (NextRoseTree(nonZeroFloat), Nil, rnd3)
        }
      }
      private val floatCanonicals: List[NonZeroFloat] = List(1.0f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f).map(NonZeroFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NonZeroFloat], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[NonZeroFloat]"
    }

  /**
    * A [[Generator]] that produces Floats, excluding zero and infinity.
    */
  implicit val nonZeroFiniteFloatGenerator: Generator[NonZeroFiniteFloat] =
    new Generator[NonZeroFiniteFloat] {

      case class NextRoseTree(value: NonZeroFiniteFloat) extends RoseTree[NonZeroFiniteFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NonZeroFiniteFloat]], Randomizer) = {
          @tailrec
          def shrinkLoop(raw: NonZeroFiniteFloat, acc: LazyListOrStream[RoseTree[NonZeroFiniteFloat]]): LazyListOrStream[RoseTree[NonZeroFiniteFloat]] = {
            val d = raw.value
            if (d <= 1.0f && d >= -1.0f) {
               if (acc.isEmpty)
                 Rose(NonZeroFiniteFloat.ensuringValid(-1.0f)) #:: Rose(NonZeroFiniteFloat.ensuringValid(1.0f)) #:: LazyListOrStream.empty
               else acc
            } else if (!d.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (d > 0.0f) (d.floor, (-d).ceil) else (d.ceil, (-d).floor)
              shrinkLoop(NonZeroFiniteFloat.ensuringValid(nearest), NextRoseTree(NonZeroFiniteFloat.ensuringValid(nearestNeg)) #:: NextRoseTree(NonZeroFiniteFloat.ensuringValid(nearest)) #:: acc)
            }
            else {
              val sqrt: Float = math.sqrt(d.abs.toDouble).toFloat
              if (sqrt < 1.0f) acc
              else {
                val whole: NonZeroFiniteFloat = NonZeroFiniteFloat.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroFiniteFloat = -whole  //math.rint(-whole)
                val (first, second) = if (d > 0.0f) (negWhole, whole) else (whole, negWhole)
                shrinkLoop(first, NextRoseTree(first) #:: NextRoseTree(second) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO Confirm OK without Roses.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroFiniteFloat], rnd: Randomizer): (RoseTree[NonZeroFiniteFloat], List[NonZeroFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nonZeroFiniteFloat, rnd2) = rnd.nextNonZeroFiniteFloat
            (NextRoseTree(nonZeroFiniteFloat), Nil, rnd2)
        }
      }
      private val floatCanonicals: List[NonZeroFiniteFloat] = List(1.0f, -1.0f, 2.0f, -2.0f, 3.0f, -3.0f).map(NonZeroFiniteFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NonZeroFiniteFloat], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[NonZeroFiniteFloat]"
    }

  /**
    * A [[Generator]] that produces integers, excluding zero.
    */
  implicit val nonZeroIntGenerator: Generator[NonZeroInt] =
    new Generator[NonZeroInt] {

      case class NextRoseTree(value: NonZeroInt) extends RoseTree[NonZeroInt] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NonZeroInt]], Randomizer) = {
          @tailrec
          def shrinkLoop(i: NonZeroInt, acc: LazyListOrStream[RoseTree[NonZeroInt]]): LazyListOrStream[RoseTree[NonZeroInt]] = {
            val half: Int = i / 2 // i cannot be zero, because initially it is the underlying Int value of a NonZeroInt (in types
            if (half == 0) acc    // we trust), then if half results in zero, we return acc here. I.e., we don't loop.
            else shrinkLoop(NonZeroInt.ensuringValid(half), NextRoseTree(NonZeroInt.ensuringValid(-half)) #:: NextRoseTree(NonZeroInt.ensuringValid(half)) #:: acc)
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO Confirm OK without Roses.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroInt], rnd: Randomizer): (RoseTree[NonZeroInt], List[NonZeroInt], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nonZeroInt, rnd2) = rnd.nextNonZeroInt
            (NextRoseTree(nonZeroInt), Nil, rnd2)
        }
      }
      override def toString = "Generator[NonZeroInt]"
      private val nonZeroIntCanonicals = List(NonZeroInt(1), NonZeroInt(-1), NonZeroInt(2), NonZeroInt(-2), NonZeroInt(3), NonZeroInt(-3))
      override def canonicals(rnd: Randomizer): (Iterator[NonZeroInt], Randomizer) = (nonZeroIntCanonicals.iterator, rnd)
    }

  /**
    * A [[Generator]] that produces Longs, excluding zero.
    */
  implicit val nonZeroLongGenerator: Generator[NonZeroLong] =
    new Generator[NonZeroLong] {

      case class NextRoseTree(value: NonZeroLong) extends RoseTree[NonZeroLong] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NonZeroLong]], Randomizer) = {
          @tailrec
          def shrinkLoop(i: NonZeroLong, acc: LazyListOrStream[RoseTree[NonZeroLong]]): LazyListOrStream[RoseTree[NonZeroLong]] = {
            val half: Long = i / 2 // i cannot be zero, because initially it is the underlying Int value of a NonZeroLong (in types
            if (half == 0) acc     // we trust), then if half results in zero, we return acc here. I.e., we don't loop.
            else shrinkLoop(NonZeroLong.ensuringValid(half), NextRoseTree(NonZeroLong.ensuringValid(-half)) #:: NextRoseTree(NonZeroLong.ensuringValid(half)) #:: acc)
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO Confirm OK with no Roses.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NonZeroLong], rnd: Randomizer): (RoseTree[NonZeroLong], List[NonZeroLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nonZeroLong, rnd2) = rnd.nextNonZeroLong
            (NextRoseTree(nonZeroLong), Nil, rnd2)
        }
      }
      private val nonZeroLongCanonicals = List(1, -1, 2, -2, 3, -3).map(NonZeroLong.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NonZeroLong], Randomizer) = (nonZeroLongCanonicals.iterator, rnd)
      override def toString = "Generator[NonZeroLong]"
    }

  /**
    * A [[Generator]] that produces negative Doubles, excluding zero but including infinity.
    */
  implicit val negDoubleGenerator: Generator[NegDouble] =
    new Generator[NegDouble] {

      case class NextRoseTree(value: NegDouble) extends RoseTree[NegDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegDouble, acc: LazyListOrStream[RoseTree[NegDouble]]): LazyListOrStream[RoseTree[NegDouble]] = {
            val fv = f.value
            if (fv == -1.0) acc
            else if (fv > -1.0) Rose(NegDouble(-1.0)) #:: acc
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.NegativeInfinity || fv.isNaN)
                  Double.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegDouble.ensuringValid(n.ceil)
              shrinkLoop(nearest, Rose(nearest) #:: acc)
            }
            else {
              val sqrt: Double = -(math.sqrt(fv.abs))
              val whole = NegDouble.ensuringValid(sqrt.ceil)
              shrinkLoop(whole, NextRoseTree(whole) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegDouble], rnd: Randomizer): (RoseTree[NegDouble], List[NegDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negDouble, rnd2) = rnd.nextNegDouble
            (NextRoseTree(negDouble), Nil, rnd2)
        }
      }
      private val negDoubleCanonicals: List[NegDouble] = List(-1.0, -2.0, -3.0).map(NegDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegDouble], Randomizer) = (negDoubleCanonicals.iterator, rnd)
      override def toString = "Generator[NegDouble]"
    }

  /**
    * A [[Generator]] that produces negative Doubles, excluding zero and infinity.
    */
  implicit val negFiniteDoubleGenerator: Generator[NegFiniteDouble] =
    new Generator[NegFiniteDouble] {

      case class NextRoseTree(value: NegFiniteDouble) extends RoseTree[NegFiniteDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegFiniteDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegFiniteDouble, acc: LazyListOrStream[RoseTree[NegFiniteDouble]]): LazyListOrStream[RoseTree[NegFiniteDouble]] = {
            val fv = f.value
            if (fv == -1.0) acc
            else if (fv > -1.0) Rose(NegFiniteDouble(-1.0)) #:: acc
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegFiniteDouble.ensuringValid(fv.ceil)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Double = -(math.sqrt(fv.abs))
              val whole = NegFiniteDouble.ensuringValid(sqrt.ceil)
              shrinkLoop(whole, NextRoseTree(whole) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegFiniteDouble], rnd: Randomizer): (RoseTree[NegFiniteDouble], List[NegFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negFiniteDouble, rnd2) = rnd.nextNegFiniteDouble
            (NextRoseTree(negFiniteDouble), Nil, rnd2)
        }
      }
      private val negDoubleCanonicals: List[NegFiniteDouble] = List(-1.0, -2.0, -3.0).map(NegFiniteDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegFiniteDouble], Randomizer) = (negDoubleCanonicals.iterator, rnd)
      override def toString = "Generator[NegFiniteDouble]"
    }

  /**
    * A [[Generator]] that produces negative Floats, excluding zero but including infinity.
    */
  implicit val negFloatGenerator: Generator[NegFloat] =
    new Generator[NegFloat] {

      case class NextRoseTree(value: NegFloat) extends RoseTree[NegFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegFloat]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegFloat, acc: LazyListOrStream[RoseTree[NegFloat]]): LazyListOrStream[RoseTree[NegFloat]] = {
            val fv = f.value
            if (fv == -1.0f) acc
            else if (fv > -1.0f) Rose(NegFloat(-1.0f)) #:: acc
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.NegativeInfinity || fv.isNaN)
                  Float.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegFloat.ensuringValid(n.ceil)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Float = -(math.sqrt(fv.abs.toDouble)).toFloat
              val whole = NegFloat.ensuringValid(sqrt.ceil)
              shrinkLoop(whole, NextRoseTree(whole) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegFloat], rnd: Randomizer): (RoseTree[NegFloat], List[NegFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negFloat, rnd2) = rnd.nextNegFloat
            (NextRoseTree(negFloat), Nil, rnd2)
        }
      }
      private val negFloatCanonicals: List[NegFloat] = List(-1.0f, -2.0f, -3.0f).map(NegFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegFloat], Randomizer) = (negFloatCanonicals.iterator, rnd)
      override def toString = "Generator[NegFloat]"
    }

  /**
    * A [[Generator]] that produces negative Floats, excluding zero and infinity.
    */
  implicit val negFiniteFloatGenerator: Generator[NegFiniteFloat] =
    new Generator[NegFiniteFloat] {

      case class NextRoseTree(value: NegFiniteFloat) extends RoseTree[NegFiniteFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegFiniteFloat]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegFiniteFloat, acc: LazyListOrStream[RoseTree[NegFiniteFloat]]): LazyListOrStream[RoseTree[NegFiniteFloat]] = {
            val fv = f.value
            if (fv == -1.0f) acc
            else if (fv > -1.0f) Rose(NegFiniteFloat(-1.0f)) #:: acc
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegFiniteFloat.ensuringValid(fv.ceil)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Float = -(math.sqrt(fv.abs.toDouble)).toFloat
              val whole = NegFiniteFloat.ensuringValid(sqrt.ceil)
              shrinkLoop(whole, NextRoseTree(whole) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegFiniteFloat], rnd: Randomizer): (RoseTree[NegFiniteFloat], List[NegFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negFiniteFloat, rnd2) = rnd.nextNegFiniteFloat
            (NextRoseTree(negFiniteFloat), Nil, rnd2)
        }
      }
      private val negFloatCanonicals: List[NegFiniteFloat] = List(-1.0f, -2.0f, -3.0f).map(NegFiniteFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegFiniteFloat], Randomizer) = (negFloatCanonicals.iterator, rnd)
      override def toString = "Generator[NegFiniteFloat]"
    }

  /**
    * A [[Generator]] that produces negative Ints, excluding zero.
    */
  implicit val negIntGenerator: Generator[NegInt] =
    new Generator[NegInt] {

      case class NextRoseTree(value: NegInt) extends RoseTree[NegInt] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegInt]], Randomizer) = {
          @tailrec
          def shrinkLoop(i: NegInt, acc: LazyListOrStream[RoseTree[NegInt]]): LazyListOrStream[RoseTree[NegInt]] = {
            val half: Int = i / 2
            if (half == 0) acc
            else {
              val negIntHalf = NegInt.ensuringValid(half)
              shrinkLoop(negIntHalf, NextRoseTree(negIntHalf) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO Confirm OK with no Roses.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegInt], rnd: Randomizer): (RoseTree[NegInt], List[NegInt], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negInt, rnd2) = rnd.nextNegInt
            (NextRoseTree(negInt), Nil, rnd2)
        }
      }
      private val negIntCanonicals = List(-1, -2, -3).map(NegInt.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegInt], Randomizer) = (negIntCanonicals.iterator, rnd)
      override def toString = "Generator[NegInt]"
    }

  /**
    * A [[Generator]] that produces negative Longs, excluding zero.
    */
  implicit val negLongGenerator: Generator[NegLong] =
    new Generator[NegLong] {

      case class NextRoseTree(value: NegLong) extends RoseTree[NegLong] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegLong]], Randomizer) = {
          @tailrec
          def shrinkLoop(i: NegLong, acc: LazyListOrStream[RoseTree[NegLong]]): LazyListOrStream[RoseTree[NegLong]] = {
            val half: Long = i / 2
            if (half == 0) acc
            else {
              val negLongHalf = NegLong.ensuringValid(half)
              shrinkLoop(negLongHalf, NextRoseTree(negLongHalf) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO: Confirm OK with no Roses.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegLong], rnd: Randomizer): (RoseTree[NegLong], List[NegLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negLong, rnd2) = rnd.nextNegLong
            (NextRoseTree(negLong), Nil, rnd2)
        }
      }
      private val negLongCanonicals = List(-1, -2, -3).map(NegLong.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegLong], Randomizer) = (negLongCanonicals.iterator, rnd)
      override def toString = "Generator[NegLong]"
    }

  /**
    * A [[Generator]] that produces negative Doubles, including zero and infinity.
    */
  implicit val negZDoubleGenerator: Generator[NegZDouble] =
    new Generator[NegZDouble] {

      case class NextRoseTree(value: NegZDouble) extends RoseTree[NegZDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegZDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegZDouble, acc: LazyListOrStream[RoseTree[NegZDouble]]): LazyListOrStream[RoseTree[NegZDouble]] = {
            val fv = f.value
            if (fv == 0.0) acc
            else if (fv >= -1.0) Rose(NegZDouble(0.0)) #:: acc
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.NegativeInfinity || fv.isNaN)
                  Double.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegZDouble.ensuringValid(n.ceil)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Double = -math.sqrt(fv.abs)
              if (sqrt > -1.0) Rose(NegZDouble(0.0)) #:: acc
              else {
                val whole = NegZDouble.ensuringValid(sqrt.ceil)
                shrinkLoop(whole, NextRoseTree(whole) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZDouble], rnd: Randomizer): (RoseTree[NegZDouble], List[NegZDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negZDouble, rnd2) = rnd.nextNegZDouble
            val (roseTreeOfNegZDouble, rnd3) = (NextRoseTree(negZDouble), rnd2)
            (NextRoseTree(negZDouble), Nil, rnd3)
        }
      }
      private val doubleCanonicals: List[NegZDouble] = List(0.0, -1.0, -2.0, -3.0).map(NegZDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegZDouble], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[NegZDouble]"
    }

  /**
    * A [[Generator]] that produces negative Doubles, including zero but excluding infinity.
    */
  implicit val negZFiniteDoubleGenerator: Generator[NegZFiniteDouble] =
    new Generator[NegZFiniteDouble] {

      case class NextRoseTree(value: NegZFiniteDouble) extends RoseTree[NegZFiniteDouble] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegZFiniteDouble]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegZFiniteDouble, acc: LazyListOrStream[RoseTree[NegZFiniteDouble]]): LazyListOrStream[RoseTree[NegZFiniteDouble]] = {
            val fv = f.value
            if (fv == 0.0) acc
            else if (fv >= -1.0) Rose(NegZFiniteDouble(0.0)) #:: acc
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegZFiniteDouble.ensuringValid(fv.ceil)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Double = -math.sqrt(fv.abs)
              if (sqrt > -1.0) Rose(NegZFiniteDouble(0.0)) #:: acc
              else {
                val whole = NegZFiniteDouble.ensuringValid(sqrt.ceil)
                shrinkLoop(whole, NextRoseTree(whole) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZFiniteDouble], rnd: Randomizer): (RoseTree[NegZFiniteDouble], List[NegZFiniteDouble], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negZFiniteDouble, rnd2) = rnd.nextNegZFiniteDouble
            (NextRoseTree(negZFiniteDouble), Nil, rnd2)
        }
      }
      private val doubleCanonicals: List[NegZFiniteDouble] = List(0.0, -1.0, -2.0, -3.0).map(NegZFiniteDouble.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegZFiniteDouble], Randomizer) = (doubleCanonicals.iterator, rnd)
      override def toString = "Generator[NegZFiniteDouble]"
    }

  /**
    * A [[Generator]] that produces negative Floats, including zero and infinity.
    */
  implicit val negZFloatGenerator: Generator[NegZFloat] =
    new Generator[NegZFloat] {

      case class NextRoseTree(value: NegZFloat) extends RoseTree[NegZFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegZFloat]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegZFloat, acc: LazyListOrStream[RoseTree[NegZFloat]]): LazyListOrStream[RoseTree[NegZFloat]] = {
            val fv = f.value
            if (fv == 0.0f) acc
            else if (fv >= -1.0f) Rose(NegZFloat(0.0f)) #:: acc
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.NegativeInfinity || fv.isNaN)
                  Float.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegZFloat.ensuringValid(n.ceil)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Float = -math.sqrt(fv.abs.toDouble).toFloat
              if (sqrt > -1.0f) Rose(NegZFloat(0.0f)) #:: acc
              else {
                val whole = NegZFloat.ensuringValid(sqrt.ceil)
                shrinkLoop(whole, NextRoseTree(whole) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZFloat], rnd: Randomizer): (RoseTree[NegZFloat], List[NegZFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negZFloat, rnd2) = rnd.nextNegZFloat
            (NextRoseTree(negZFloat), Nil, rnd2)
        }
      }
      private val floatCanonicals: List[NegZFloat] = List(0.0f, -1.0f, -2.0f, -3.0f).map(NegZFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegZFloat], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[NegZFloat]"
    }

  /**
    * A [[Generator]] that produces negative Floats, including zero but excluding infinity.
    */
  implicit val negZFiniteFloatGenerator: Generator[NegZFiniteFloat] =
    new Generator[NegZFiniteFloat] {

      case class NextRoseTree(value: NegZFiniteFloat) extends RoseTree[NegZFiniteFloat] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegZFiniteFloat]], Randomizer) = {
          @tailrec
          def shrinkLoop(f: NegZFiniteFloat, acc: LazyListOrStream[RoseTree[NegZFiniteFloat]]): LazyListOrStream[RoseTree[NegZFiniteFloat]] = {
            val fv = f.value
            if (fv == 0.0f) acc
            else if (fv >= -1.0f) Rose(NegZFiniteFloat(0.0f)) #:: acc
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegZFiniteFloat.ensuringValid(fv.ceil)
              shrinkLoop(nearest, NextRoseTree(nearest) #:: acc)
            }
            else {
              val sqrt: Float = -math.sqrt(fv.abs.toDouble).toFloat
              if (sqrt > -1.0f) Rose(NegZFiniteFloat(0.0f)) #:: acc
              else {
                val whole = NegZFiniteFloat.ensuringValid(sqrt.ceil)
                shrinkLoop(whole, NextRoseTree(whole) #:: acc)
              }
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZFiniteFloat], rnd: Randomizer): (RoseTree[NegZFiniteFloat], List[NegZFiniteFloat], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negZFiniteFloat, rnd2) = rnd.nextNegZFiniteFloat
            (NextRoseTree(negZFiniteFloat), Nil, rnd2)
        }
      }
      private val floatCanonicals: List[NegZFiniteFloat] = List(0.0f, -1.0f, -2.0f, -3.0f).map(NegZFiniteFloat.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegZFiniteFloat], Randomizer) = (floatCanonicals.iterator, rnd)
      override def toString = "Generator[NegZFiniteFloat]"
    }

  /**
    * A [[Generator]] that produces negative Ints, including zero.
    */
  implicit val negZIntGenerator: Generator[NegZInt] =
    new Generator[NegZInt] {

      case class NextRoseTree(value: NegZInt) extends RoseTree[NegZInt] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegZInt]], Randomizer) = {
          @tailrec
          def shrinkLoop(i: NegZInt, acc: LazyListOrStream[RoseTree[NegZInt]]): LazyListOrStream[RoseTree[NegZInt]] = {
            if (i.value == 0)
              acc
            else {
              val half: Int = i / 2
              val negIntHalf = NegZInt.ensuringValid(half)
              shrinkLoop(negIntHalf, NextRoseTree(negIntHalf) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO Confirm OK with no Rose.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZInt], rnd: Randomizer): (RoseTree[NegZInt], List[NegZInt], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negZInt, rnd2) = rnd.nextNegZInt
            (NextRoseTree(negZInt), Nil, rnd2)
        }
      }
      private val negZIntCanonicals = List(0, -1, -2, -3).map(NegZInt.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegZInt], Randomizer) = (negZIntCanonicals.iterator, rnd)
      override def toString = "Generator[NegZInt]"
    }

  /**
    * A [[Generator]] that produces negative Longs, including zero.
    */
  implicit val negZLongGenerator: Generator[NegZLong] =
    new Generator[NegZLong] {

      case class NextRoseTree(value: NegZLong) extends RoseTree[NegZLong] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NegZLong]], Randomizer) = {
          @tailrec
          def shrinkLoop(i: NegZLong, acc: LazyListOrStream[RoseTree[NegZLong]]): LazyListOrStream[RoseTree[NegZLong]] = {
            if (i.value == 0)
              acc
            else {
              val half: Long = i / 2
              val negLongHalf = NegZLong.ensuringValid(half)
              shrinkLoop(negLongHalf, NextRoseTree(negLongHalf) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      } // TODO Confirm OK no Rose.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      def next(szp: SizeParam, edges: List[NegZLong], rnd: Randomizer): (RoseTree[NegZLong], List[NegZLong], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (negZLong, rnd2) = rnd.nextNegZLong
            (NextRoseTree(negZLong), Nil, rnd2)
        }
      }
      private val negZLongCanonicals = List(0, -1, -2, -3).map(NegZLong.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NegZLong], Randomizer) = (negZLongCanonicals.iterator, rnd)
      override def toString = "Generator[NegZLong]"
    }

  /**
    * A [[Generator]] that produces Chars, but only the ones that represent digits.
    */
  implicit val numericCharGenerator: Generator[NumericChar] =
    new Generator[NumericChar] {

      case class NextRoseTree(value: NumericChar) extends RoseTree[NumericChar] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[NumericChar]], Randomizer) = {
          @tailrec
          def shrinkLoop(c: NumericChar, acc: LazyListOrStream[RoseTree[NumericChar]]): LazyListOrStream[RoseTree[NumericChar]] = {
            if (c.value == '0')
              acc
            else {
              val minusOne: Char = (c - 1).toChar // Go ahead and try all the values between i and '0'
              val numericCharMinusOne = NumericChar.ensuringValid(minusOne)
              shrinkLoop(numericCharMinusOne, NextRoseTree(numericCharMinusOne) #:: acc)
            }
          }
          (shrinkLoop(value, LazyListOrStream.empty).reverse, rndPassedToShrinks)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NumericChar], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(numericCharEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }

      def next(szp: SizeParam, edges: List[NumericChar], rnd: Randomizer): (RoseTree[NumericChar], List[NumericChar], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (posZInt, rnd2) = rnd.choosePosZInt(PosZInt.ensuringValid(0), PosZInt.ensuringValid(9))
            (NextRoseTree(NumericChar.ensuringValid((posZInt.value + 48).toChar)), Nil, rnd2)
        }
      }
      private val numericCharCanonicals = List('0', '1', '2', '3').map(NumericChar.ensuringValid(_))
      override def canonicals(rnd: Randomizer): (Iterator[NumericChar], Randomizer) = (numericCharCanonicals.iterator, rnd)
      override def toString = "Generator[NumericChar]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  /**
    * A [[Generator]] that produces arbitrary Strings.
    *
    * Note that this does not confine itself to ASCII! While failed tests will try to shrink to
    * readable ASCII, this will produce arbitrary Unicode Strings.
    */
  implicit val stringGenerator: Generator[String] =
    new Generator[String] {
      private val stringEdges = List("")

      case class NextRoseTree(value: String) extends RoseTree[String] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[String]], Randomizer) = {
          if (value.isEmpty)
            (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            val halfSize = value.length / 2
            val firstHalf = value.take(halfSize)
            val secondHalf = value.drop(halfSize)
            val tail = value.tail
            val init = value.init
            (LazyListOrStream(firstHalf, secondHalf, tail, init).distinct.filter(_ != value).map(NextRoseTree(_)), rndPassedToShrinks)
          }
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[String], Randomizer) = {
        (stringEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[String], rnd: Randomizer): (RoseTree[String], List[String], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (s, rnd2) = rnd.nextString(szp.size)
            (NextRoseTree(s), Nil, rnd2)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[String], Randomizer) = {
        val (canonicalsOfChar, rnd1) = charGenerator.canonicals(rnd)
        (Iterator("") ++ canonicalsOfChar.map(t => s"$t"), rnd1)
      }
      override def toString = "Generator[String]"
    }

  // Should throw IAE on negative size in all generators, even the ones that ignore size.
  /**
    * Given an existing `Generator[T]`, this creates a `Generator[List[T]]`.
    *
    * @param genOfT a [[Generator]] that produces values of type [[T]]
    * @tparam T the type that we are producing a List of
    * @return a List of values of type [[T]]
    */
  implicit def listGenerator[T](implicit genOfT: Generator[T]): Generator[List[T]] with HavingLength[List[T]] =
    new Generator[List[T]] with HavingLength[List[T]] { outerGenOfListOfT =>
      private val listEdges = List(Nil)

      // TODO This only uses Roses. Check that we don't need RoseTrees.
      case class NextRoseTree(value: List[T]) extends RoseTree[List[T]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[List[T]]], Randomizer) = {
          if (value.isEmpty)
            (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            val halfSize = value.length / 2
            val firstHalf = value.take(halfSize)
            val secondHalf = value.drop(halfSize)
            val tail = value.tail
            val init = value.init
            (LazyListOrStream(firstHalf, secondHalf, tail, init).distinct.filter(_ != value).map(NextRoseTree(_)), rndPassedToShrinks)
          }
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = {
        (listEdges.take(maxLength), rnd)
      }
      def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (RoseTree[List[T]], List[List[T]], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (listOfT, rnd2) = rnd.nextList[T](szp.size)
            (NextRoseTree(listOfT), Nil, rnd2)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = {
        val (canonicalsOfT, rnd1) = genOfT.canonicals(rnd)
        (canonicalsOfT.map(t => List(t)), rnd1)
      }

      override def toString = "Generator[List[T]]"
      def havingSize(size: PosZInt): Generator[List[T]] = { // TODO: add with HavingLength again
        // No edges and no shrinking. Since they said they want a list of a particular length,
        // that is what they'll get.
        // Hmm, TODO: Seems like shrinking could work by simplifying the Ts, but not reducing
        // the length of the List.
        new Generator[List[T]] {
          override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd) // TODO: filter lists's edges by valid size
          def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (RoseTree[List[T]], List[List[T]], Randomizer) =
            outerGenOfListOfT.next(SizeParam(PosZInt(0), szp.maxSize, size), edges, rnd) // TODO: SizeParam(size, size, size)?
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def toString = s"Generator[List[T] /* having length $size */]"
        }
      }

      def havingSizesBetween(from: PosZInt, to: PosZInt): Generator[List[T]] = { // TODO: add with HavingLength again
        require(from != to, Resources.fromEqualToToHavingSizesBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingSizesBetween(from, to))
        new Generator[List[T]] {
          // I don't think edges should have one list each of length from and to, because they would
          // need to have random contents, and that doesn't seem like an edge.
          override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd) // TODO: filter lists's edges by valid size
          // Specify how size is used.
          def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (RoseTree[List[T]], List[List[T]], Randomizer) = {
            val nextSize = {
              val candidate: Int = (from + (szp.size.toFloat * (to - from).toFloat / (szp.maxSize + 1).toFloat)).round
              if (candidate > to) to
              else if (candidate < from) from
              else PosZInt.ensuringValid(candidate)
            }
            // TODO: should minSize not be from from now on.
            outerGenOfListOfT.next(SizeParam(PosZInt(0), to, nextSize), edges, rnd) // This assumes from < to, and i'm not guaranteeing that yet
          }
          // If from is either 0 or 1, return the canonicals of the outer Generator.
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) =
            if (from <= 1) outerGenOfListOfT.canonicals(rnd) else (Iterator.empty, rnd)
          // TODO: Shrink can go from from up to xs length
          override def toString = s"Generator[List[T] /* having lengths between $from and $to (inclusive) */]"
        }
      }
      def havingSizesDeterminedBy(f: SizeParam => SizeParam): Generator[List[T]] = // TODO: add with HavingLength again
        new Generator[List[T]] {
          override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = (Nil, rnd)
          def next(szp: SizeParam, edges: List[List[T]], rnd: Randomizer): (RoseTree[List[T]], List[List[T]], Randomizer) =
            outerGenOfListOfT.next(f(szp), edges, rnd)
          override def canonicals(rnd: Randomizer): (Iterator[List[T]], Randomizer) = (Iterator.empty, rnd)
          override def toString = s"Generator[List[T] /* having lengths determined by a function */]"
        }
    }

  /**
    * Given a [[Generator]] that produces values of type [[T]], this returns one that produces ''functions'' that return
    * a T.
    *
    * The functions produced here are nullary -- they take no parameters, they just spew out values of type [[T]].
    *
    * @param genOfT a [[Generator]] that produces values of [[T]]
    * @tparam T the type to produce
    * @return a [[Generator]] that produces functions that return values of type [[T]]
    */
  implicit def function0Generator[T](implicit genOfT: Generator[T]): Generator[() => T] = {
    new Generator[() => T] { thisGeneratorOfFunction0 =>
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[() => T], Randomizer) = {
        val (edgesOfT, nextRnd) = genOfT.initEdges(maxLength, rnd)
        val edges = edgesOfT.map(t => PrettyFunction0(t))
        (edges, nextRnd)
      }
      def next(szp: SizeParam, edges: List[() => T], rnd: Randomizer): (RoseTree[() => T], List[() => T], Randomizer) = {
        edges match {
          case head :: tail =>
            (Rose(head), tail, rnd)
          case _ =>
            val (nextRoseTreeOfT, _, nextRnd) = genOfT.next(szp, Nil, rnd)
            (nextRoseTreeOfT.map(t => PrettyFunction0(t)), Nil, nextRnd)
        }
      }
      override def canonicals(rnd: Randomizer): (Iterator[() => T], Randomizer) = {
        val (canonicalsOfT, nextRnd) = genOfT.canonicals(rnd)
        val canonicals = canonicalsOfT.map(t => PrettyFunction0(t))
        (canonicals, nextRnd)
      }
    }
  }

  /**
    * Generate functions that take an [[Int]] and return a modified [[Int]].
    *
    * This [[Generator]] is useful for testing edge cases of some higher-order functions. Besides obvious
    * functions (returning the same Int, returning that Int plus 1), it tests overflow situations such as
    * adding [[Int.MaxValue]], negation, and other such cases.
    */
  implicit val function1IntToIntGenerator: Generator[Int => Int] = {
    object IntToIntIdentity extends (Int => Int) {
      def apply(i: Int): Int = i
      override def toString = "(i: Int) => i"
    }
    object IntToIntIncr extends (Int => Int) {
      def apply(i: Int): Int = i + 1
      override def toString = "(i: Int) => i + 1"
    }
    object IntToIntIncrBy2 extends (Int => Int) {
      def apply(i: Int): Int = i + 2
      override def toString = "(i: Int) => i + 2"
    }
    object IntToIntIncrBy3 extends (Int => Int) {
      def apply(i: Int): Int = i + 3
      override def toString = "(i: Int) => i + 3"
    }
    object IntToIntIncrByMax extends (Int => Int) {
      def apply(i: Int): Int = i + Int.MaxValue
      override def toString = "(i: Int) => i + Int.MaxValue"
    }
    object IntToIntIncrByMin extends (Int => Int) {
      def apply(i: Int): Int = i + Int.MinValue
      override def toString = "(i: Int) => i + Int.MinValue"
    }
    object IntToIntDecr extends (Int => Int) {
      def apply(i: Int): Int = i - 1
      override def toString = "(i: Int) => i - 1"
    }
    object IntToIntDecrBy2 extends (Int => Int) {
      def apply(i: Int): Int = i - 2
      override def toString = "(i: Int) => i - 2"
    }
    object IntToIntDecrBy3 extends (Int => Int) {
      def apply(i: Int): Int = i - 3
      override def toString = "(i: Int) => i - 3"
    }
    object IntToIntDecrByMax extends (Int => Int) {
      def apply(i: Int): Int = i - Int.MaxValue
      override def toString = "(i: Int) => i - Int.MaxValue"
    }
    object IntToIntDecrByMin extends (Int => Int) {
      def apply(i: Int): Int = i - Int.MinValue
      override def toString = "(i: Int) => i - Int.MinValue"
    }
    object IntToIntSquare extends (Int => Int) {
      def apply(i: Int): Int = i * i
      override def toString = "(i: Int) => i * i"
    }
    object IntToIntCube extends (Int => Int) {
      def apply(i: Int): Int = i * i * i
      override def toString = "(i: Int) => i * i * i"
    }
    object IntToIntHalf extends (Int => Int) {
      def apply(i: Int): Int = i / 2
      override def toString = "(i: Int) => i / 2"
    }
    object IntToIntThird extends (Int => Int) {
      def apply(i: Int): Int = i / 3
      override def toString = "(i: Int) => i / 3"
    }
    object IntToIntFourth extends (Int => Int) {
      def apply(i: Int): Int = i / 3
      override def toString = "(i: Int) => i / 4"
    }
    object IntToIntNegate extends (Int => Int) {
      def apply(i: Int): Int = -i
      override def toString = "(i: Int) => -i"
    }
    object IntToIntComplement extends (Int => Int) {
      def apply(i: Int): Int = ~i
      override def toString = "(i: Int) => ~i"
    }
    val funs: Vector[Int => Int] =
      Vector(
        IntToIntIdentity,
        IntToIntIncr,
        IntToIntIncrBy2,
        IntToIntIncrBy3,
        IntToIntIncrByMax,
        IntToIntIncrByMin,
        IntToIntDecr,
        IntToIntDecrBy2,
        IntToIntDecrBy3,
        IntToIntDecrByMax,
        IntToIntDecrByMin,
        IntToIntSquare,
        IntToIntCube,
        IntToIntHalf,
        IntToIntThird,
        IntToIntFourth,
        IntToIntNegate,
        IntToIntComplement
      )
    new Generator[Int => Int] {
      def next(szp: SizeParam, edges: List[Int => Int], rnd: Randomizer): (RoseTree[Int => Int], List[Int => Int], Randomizer) = {
        edges match {
          case head :: tail =>
            (Rose(head), tail, rnd)
          case _ =>
            val (nextInt, nextRnd) = rnd.nextInt
            val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
            (Rose(funs(idx)), Nil, nextRnd)
        }
      }
      override def toString = "Generator[Int => Int]"
    }
  }

  // TODO: it's a hassle to maintain 22 distinct versions of this function generator. What we *should* be doing here,
  // in theory, is abstracting over arity. Unfortunately, Scalatest can't depend on Shapeless, so that's not
  // currently practical. But once Scala 3 is real and Scalatest begins porting to it (sometime down the road),
  // we should investigate using its abstraction-over-arity machinery to reduce this boilerplate.

  /**
    * Create a [[Generator]] of functions from type [[A]] to type [[B]].
    *
    * Note that the generated functions are, necessarily, pretty random. In practice, the function you get from a
    * [[function1Generator]] (and its variations, up through [[function22Generator]]) takes the hashes of its input
    * values, combines those with a randomly-chosen number, and combines them in order to choose the generated value
    * [[B]].
    *
    * That said, each of the generated functions ''is'' deterministic: given the same input parameters and the same
    * randomly-chosen number, you will always get the same [[B]] result. And the `toString` function on the generated
    * function will show the formula you need to use in order to recreate that, which will look something like:
    *
    * {{{
    *   (a: Int, b: String, c: Float) => org.scalatest.prop.valueOf[String](a, b, c)(131)
    * }}}
    *
    * The number and type of the `a`, `b`, `c`, etc, parameters, as well as the type parameter of [[valueOf]], will
    * on the function type you are generating, but they will always follow this pattern. [[valueOf]] is the underlying
    * function that takes these parameters and the randomly-chosen number, and returns a value of the specified type.
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
  */
  implicit def function1Generator[A, B](implicit genOfB: Generator[B], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B]): Generator[A => B] = {
    new Generator[A => B] {
      def next(szp: SizeParam, edges: List[A => B], rnd: Randomizer): (RoseTree[A => B], List[A => B], Randomizer) = {

        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object AToB extends (A => B) {
          def apply(a: A): B = org.scalatest.prop.valueOf[B](a)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            s"(o: $typeOfA) => org.scalatest.prop.valueOf[$typeOfB](o)($multiplier)"
          }
        }

        (Rose(AToB), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function2Generator[A, B, C](implicit genOfC: Generator[C], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C]): Generator[(A, B) => C] = {
    new Generator[(A, B) => C] {
      def next(szp: SizeParam, edges: List[(A, B) => C], rnd: Randomizer): (RoseTree[(A, B) => C], List[(A, B) => C], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABToC extends ((A, B) => C) {
          def apply(a: A, b: B): C = org.scalatest.prop.valueOf[C](a, b)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            s"(a: $typeOfA, b: $typeOfB) => org.scalatest.prop.valueOf[$typeOfC](a, b)($multiplier)"
          }
        }

        (Rose(ABToC), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function3Generator[A, B, C, D](implicit genOfD: Generator[D], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D]): Generator[(A, B, C) => D] = {
    new Generator[(A, B, C) => D] {
      def next(szp: SizeParam, edges: List[(A, B, C) => D], rnd: Randomizer): (RoseTree[(A, B, C) => D], List[(A, B, C) => D], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCToD extends ((A, B, C) => D) {
          def apply(a: A, b: B, c: C): D = org.scalatest.prop.valueOf[D](a, b, c)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC) => org.scalatest.prop.valueOf[$typeOfD](a, b, c)($multiplier)"
          }
        }

        (Rose(ABCToD), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function4Generator[A, B, C, D, E](implicit genOfE: Generator[E], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E]): Generator[(A, B, C, D) => E] = {
    new Generator[(A, B, C, D) => E] {
      def next(szp: SizeParam, edges: List[(A, B, C, D) => E], rnd: Randomizer): (RoseTree[(A, B, C, D) => E], List[(A, B, C, D) => E], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDToE extends ((A, B, C, D) => E) {
          def apply(a: A, b: B, c: C, d: D): E = org.scalatest.prop.valueOf[E](a, b, c, d)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD) => org.scalatest.prop.valueOf[$typeOfE](a, b, c, d)($multiplier)"
          }
        }

        (Rose(ABCDToE), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function5Generator[A, B, C, D, E, F](implicit genOfF: Generator[F], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F]): Generator[(A, B, C, D, E) => F] = {
    new Generator[(A, B, C, D, E) => F] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E) => F], rnd: Randomizer): (RoseTree[(A, B, C, D, E) => F], List[(A, B, C, D, E) => F], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEToF extends ((A, B, C, D, E) => F) {
          def apply(a: A, b: B, c: C, d: D, e: E): F = org.scalatest.prop.valueOf[F](a, b, c, d, e)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE) => org.scalatest.prop.valueOf[$typeOfF](a, b, c, d, e)($multiplier)"
          }
        }

        (Rose(ABCDEToF), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function6Generator[A, B, C, D, E, F, G](implicit genOfG: Generator[G], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G]): Generator[(A, B, C, D, E, F) => G] = {
    new Generator[(A, B, C, D, E, F) => G] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F) => G], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F) => G], List[(A, B, C, D, E, F) => G], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFToG extends ((A, B, C, D, E, F) => G) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F): G = org.scalatest.prop.valueOf[G](a, b, c, d, e, f)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF) => org.scalatest.prop.valueOf[$typeOfG](a, b, c, d, e, f)($multiplier)"
          }
        }

        (Rose(ABCDEFToG), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function7Generator[A, B, C, D, E, F, G, H](implicit genOfH: Generator[H], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H]): Generator[(A, B, C, D, E, F, G) => H] = {
    new Generator[(A, B, C, D, E, F, G) => H] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G) => H], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G) => H], List[(A, B, C, D, E, F, G) => H], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGToH extends ((A, B, C, D, E, F, G) => H) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G): H = org.scalatest.prop.valueOf[H](a, b, c, d, e, f, g)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG) => org.scalatest.prop.valueOf[$typeOfH](a, b, c, d, e, f, g)($multiplier)"
          }
        }

        (Rose(ABCDEFGToH), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function8Generator[A, B, C, D, E, F, G, H, I](implicit genOfI: Generator[I], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I]): Generator[(A, B, C, D, E, F, G, H) => I] = {
    new Generator[(A, B, C, D, E, F, G, H) => I] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H) => I], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H) => I], List[(A, B, C, D, E, F, G, H) => I], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHToI extends ((A, B, C, D, E, F, G, H) => I) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H): I = org.scalatest.prop.valueOf[I](a, b, c, d, e, f, g, h)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH) => org.scalatest.prop.valueOf[$typeOfI](a, b, c, d, e, f, g, h)($multiplier)"
          }
        }

        (Rose(ABCDEFGHToI), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function9Generator[A, B, C, D, E, F, G, H, I, J](implicit genOfJ: Generator[J], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J]): Generator[(A, B, C, D, E, F, G, H, I) => J] = {
    new Generator[(A, B, C, D, E, F, G, H, I) => J] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I) => J], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I) => J], List[(A, B, C, D, E, F, G, H, I) => J], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIToJ extends ((A, B, C, D, E, F, G, H, I) => J) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I): J = org.scalatest.prop.valueOf[J](a, b, c, d, e, f, g, h, i)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI) => org.scalatest.prop.valueOf[$typeOfJ](a, b, c, d, e, f, g, h, i)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIToJ), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function10Generator[A, B, C, D, E, F, G, H, I, J, K](implicit genOfK: Generator[K], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K]): Generator[(A, B, C, D, E, F, G, H, I, J) => K] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J) => K] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J) => K], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J) => K], List[(A, B, C, D, E, F, G, H, I, J) => K], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJToK extends ((A, B, C, D, E, F, G, H, I, J) => K) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J): K = org.scalatest.prop.valueOf[K](a, b, c, d, e, f, g, h, i, j)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ) => org.scalatest.prop.valueOf[$typeOfK](a, b, c, d, e, f, g, h, i, j)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJToK), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function11Generator[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfL: Generator[L], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K) => L], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K) => L], List[(A, B, C, D, E, F, G, H, I, J, K) => L], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKToL extends ((A, B, C, D, E, F, G, H, I, J, K) => L) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K): L = org.scalatest.prop.valueOf[L](a, b, c, d, e, f, g, h, i, j, k)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK) => org.scalatest.prop.valueOf[$typeOfL](a, b, c, d, e, f, g, h, i, j, k)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKToL), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function12Generator[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfM: Generator[M], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L) => M], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L) => M], List[(A, B, C, D, E, F, G, H, I, J, K, L) => M], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLToM extends ((A, B, C, D, E, F, G, H, I, J, K, L) => M) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L): M = org.scalatest.prop.valueOf[M](a, b, c, d, e, f, g, h, i, j, k, l)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL) => org.scalatest.prop.valueOf[$typeOfM](a, b, c, d, e, f, g, h, i, j, k, l)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLToM), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfN: Generator[N], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N], List[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMToN extends ((A, B, C, D, E, F, G, H, I, J, K, L, M) => N) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M): N = org.scalatest.prop.valueOf[N](a, b, c, d, e, f, g, h, i, j, k, l, m)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM) => org.scalatest.prop.valueOf[$typeOfN](a, b, c, d, e, f, g, h, i, j, k, l, m)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMToN), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfO: Generator[O], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNToO extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N): O = org.scalatest.prop.valueOf[O](a, b, c, d, e, f, g, h, i, j, k, l, m, n)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN) => org.scalatest.prop.valueOf[$typeOfO](a, b, c, d, e, f, g, h, i, j, k, l, m, n)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNToO), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfP: Generator[P], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOToP extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O): P = org.scalatest.prop.valueOf[P](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO) => org.scalatest.prop.valueOf[$typeOfP](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOToP), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfQ: Generator[Q], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOPToQ extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P): Q = org.scalatest.prop.valueOf[Q](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP) => org.scalatest.prop.valueOf[$typeOfQ](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOPToQ), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfR: Generator[R], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOPQToR extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q): R = org.scalatest.prop.valueOf[R](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ) => org.scalatest.prop.valueOf[$typeOfR](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOPQToR), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfS: Generator[S], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOPQRToS extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R): S = org.scalatest.prop.valueOf[S](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR) => org.scalatest.prop.valueOf[$typeOfS](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOPQRToS), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfT: Generator[T], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOPQRSToT extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S): T = org.scalatest.prop.valueOf[T](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS) => org.scalatest.prop.valueOf[$typeOfT](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOPQRSToT), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfU: Generator[U], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOPQRSTToU extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T): U = org.scalatest.prop.valueOf[U](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            val typeOfU = typeInfoU.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS, t: $typeOfT) => org.scalatest.prop.valueOf[$typeOfU](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOPQRSTToU), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfV: Generator[V], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOPQRSTUToV extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U): V = org.scalatest.prop.valueOf[V](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            val typeOfU = typeInfoU.name
            val typeOfV = typeInfoV.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS, t: $typeOfT, u: $typeOfU) => org.scalatest.prop.valueOf[$typeOfV](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOPQRSTUToV), Nil, rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit genOfW: Generator[W], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V], typeInfoW: TypeInfo[W]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] {
      def next(szp: SizeParam, edges: List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W], rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W], List[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W], Randomizer) = {
        val first1000PrimesGen: Generator[Int] = first1000Primes
        val (prime, _, rnd1) = first1000PrimesGen.next(szp, Nil, rnd)
        val multiplier = if (prime.value == 2) 1 else prime.value

        object ABCDEFGHIJKLMNOPQRSTUVToW extends ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W) {
          def apply(a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V): W = org.scalatest.prop.valueOf[W](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)(multiplier)
          override def toString = {
            val typeOfA = typeInfoA.name
            val typeOfB = typeInfoB.name
            val typeOfC = typeInfoC.name
            val typeOfD = typeInfoD.name
            val typeOfE = typeInfoE.name
            val typeOfF = typeInfoF.name
            val typeOfG = typeInfoG.name
            val typeOfH = typeInfoH.name
            val typeOfI = typeInfoI.name
            val typeOfJ = typeInfoJ.name
            val typeOfK = typeInfoK.name
            val typeOfL = typeInfoL.name
            val typeOfM = typeInfoM.name
            val typeOfN = typeInfoN.name
            val typeOfO = typeInfoO.name
            val typeOfP = typeInfoP.name
            val typeOfQ = typeInfoQ.name
            val typeOfR = typeInfoR.name
            val typeOfS = typeInfoS.name
            val typeOfT = typeInfoT.name
            val typeOfU = typeInfoU.name
            val typeOfV = typeInfoV.name
            val typeOfW = typeInfoW.name
            s"(a: $typeOfA, b: $typeOfB, c: $typeOfC, d: $typeOfD, e: $typeOfE, f: $typeOfF, g: $typeOfG, h: $typeOfH, i: $typeOfI, j: $typeOfJ, k: $typeOfK, l: $typeOfL, m: $typeOfM, n: $typeOfN, o: $typeOfO, p: $typeOfP, q: $typeOfQ, r: $typeOfR, s: $typeOfS, t: $typeOfT, u: $typeOfU, v: $typeOfV) => org.scalatest.prop.valueOf[$typeOfW](a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)($multiplier)"
          }
        }

        (Rose(ABCDEFGHIJKLMNOPQRSTUVToW), Nil, rnd1)
      }
    }
  }

  /**
    * Given a [[Generator]] for type [[T]], this provides one for `Option[T]`.
    *
    * @param genOfT a [[Generator]] that produces type [[T]]
    * @tparam T the type to generate
    * @return a [[Generator]] that produces `Option[T]`
    */
  implicit def optionGenerator[T](implicit genOfT: Generator[T]): Generator[Option[T]] =
    new Generator[Option[T]] {

      // Unused currently. But this is what made me realize we may actually want a shrink
      // method on Generator that can be passed a value, so that we can shrink edges
      // inside something like Option generator (and either, or, etc.). Maybe call it
      // shrinkValue so that the name looks different.
      case class NextRoseTree(value: Option[T]) extends RoseTree[Option[T]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Option[T]]], Randomizer) = {

          value match {
            // If there is a real value, shrink that value, and return that and None.
            case Some(t) =>
              val optionOfT: Option[T] = value
              val rootRoseTree =
                new RoseTree[Option[T]] {
                  val value: Option[T] = optionOfT
                  def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Option[T]]], Randomizer) = {
                    val (topRoseTreeOfT, _, rnd2) = genOfT.next(SizeParam(1, 0, 1), List(t), rndPassedToShrinks) // topRoseTreeOfT is a RoseTree[T]
                    val (nestedRoseTrees, rnd3) = topRoseTreeOfT.shrinks(rnd2) // nestedRoseTrees: LazyListOrStream[RoseTree[T]]
                    val nestedList: LazyListOrStream[RoseTree[Option[T]]] = nestedRoseTrees.map(nrt => nrt.map(t => Some(t): Option[T])).filter(_.value != value)
                    if (nestedList.isEmpty)
                      (LazyListOrStream.empty, rnd3)
                    else
                      (nestedList, rnd3)
                  }
                }
              rootRoseTree.shrinks(rndPassedToShrinks)

            // There's no way to simplify None:
            case None => (LazyListOrStream.empty, rndPassedToShrinks)
          }
        }
      }

      // TODO: Ah, maybe edges should return List[RoseTree[Option[T]], Randomizer] instead. Then it could be shrunken.
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Option[T]], Randomizer) = {
        // Subtract one from length, and we'll wrap those in Somes. Subtract one so that None can be the first edge.
        val (edgesOfT, nextRnd) = genOfT.initEdges(if (maxLength > 0) PosZInt.ensuringValid((maxLength - 1)) else 0, rnd)
        val edges = None :: edgesOfT.map(t => Some(t))
        (edges, nextRnd)
      }

      override def canonicals(rnd: Randomizer): (Iterator[Option[T]], Randomizer) = {
        // The canonicals of Option[T] are the canonicals of T, plus None
        val (tCanonicals, nextRnd) = genOfT.canonicals(rnd)
        (Iterator(None) ++ tCanonicals.map(Some(_)), nextRnd)
      }

      def next(szp: SizeParam, edges: List[Option[T]], rnd: Randomizer): (RoseTree[Option[T]], List[Option[T]], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd) // This means I won't shrink an edge if wrapped in an Option, which is a bit odd but OK for now. UUU
          case Nil =>               // This I think can be shrunken if we add a shrinkValue method to Generator (the old shrink method).
            val (nextInt, nextRnd) = rnd.nextInt // Actually maybe not, because can't map/flatMap shrinkValue. Oh, maybe edges should
            if (nextInt % 100 == 0) // let every hundredth value or so be a None
              (NextRoseTree(None), Nil, nextRnd) // No need to shrink None.
            else {
              val (nextRoseTreeOfT, _, nextNextRnd) = genOfT.next(szp, Nil, nextRnd)
              (nextRoseTreeOfT.map(nextT => Some(nextT)), Nil, nextNextRnd)
            }  // Decided not to have None in shrinks since None is in edges. Can change that later if desired.
        }
      }
      override def toString = "Generator[Option[T]]"
    }

  /**
    * Given [[Generator]]s for two types, [[G]] and [[B]], this provides one for `G Or B`.
    *
    * @param genOfG a [[Generator]] that produces type [[G]]
    * @param genOfB a [[Generator]] that produces type [[B]]
    * @tparam G the "good" type for an [[Or]]
    * @tparam B the "bad" type for an [[Or]]
    * @return a [[Generator]] that produces `G Or B`
    */
  implicit def orGenerator[G, B](implicit genOfG: Generator[G], genOfB: Generator[B]): Generator[G Or B] =
    new Generator[G Or B] {

      case class NextRoseTree(value: G Or B) extends RoseTree[G Or B] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[G Or B]], Randomizer) = {
          value match {
            case Good(g) => {
              val (goodRt, _, nextRnd) = genOfG.next(SizeParam(1, 0, 1), List(g), rndPassedToShrinks)
              val (gShrink, nextNextRnd) = goodRt.shrinks(nextRnd)
              (gShrink.filter(_.value != value).map(rt => rt.map(Good(_) : G Or B)), nextNextRnd)
            }
            case Bad(b) => {
              val (badRt, _, nextRnd) = genOfB.next(SizeParam(1, 0, 1), List(b), rndPassedToShrinks)
              val (bShrink, nextNextRnd) = badRt.shrinks(nextRnd)
              (bShrink.filter(_.value != value).map(rt => rt.map(Bad(_) : G Or B)), nextNextRnd)
            }
          }
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[G Or B], Randomizer) = {
        val (edgesOfG, nextRnd) = genOfG.initEdges(maxLength, rnd)
        val (edgesOfB, nextNextRnd) = genOfB.initEdges(maxLength, nextRnd)
        // Fill up to maxLength, favoring Good over Bad if maxLength is odd. Else just dividing it
        // down the middle, half Good, half Bad. And filling in with the other if one side runs out.
        @tailrec
        def loop(count: Int, remainingG: List[G], remainingB: List[B], acc: List[G Or B]): List[G Or B] = {
          (count, remainingG, remainingB) match {
            case (0, _, _) => acc
            case (_, Nil, Nil) => acc
            case (c, gHead :: gTail, Nil) => loop(c - 1, gTail, Nil, Good(gHead) :: acc)
            case (c, Nil, bHead :: bTail) => loop(c - 1, Nil, bTail, Bad(bHead) :: acc)
            case (c, gHead :: gTail, _) if c % 2 == 0 => loop(c - 1, gTail, remainingB, Good(gHead) :: acc)
            case (c, _, bHead :: bTail) => loop(c - 1, remainingG, bTail, Bad(bHead) :: acc)
          }
        }
        (loop(maxLength, edgesOfG, edgesOfB, Nil), nextNextRnd)
      }

      override def canonicals(rnd: Randomizer): (Iterator[Or[G, B]], Randomizer) = {
        val (goodCanon, nextRnd) = genOfG.canonicals(rnd)
        val (badCanon, nextNextRnd) = genOfB.canonicals(nextRnd)

        (goodCanon.map(Good(_)) ++ badCanon.map(Bad(_)), nextNextRnd)
      }

      def next(szp: SizeParam, edges: List[G Or B], rnd: Randomizer): (RoseTree[G Or B], List[G Or B], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nextInt, nextRnd) = rnd.nextInt
            if (nextInt % 4 == 0) {
              val (nextRoseTreeOfB, _, nextRnd) = genOfB.next(szp, Nil, rnd)
              (nextRoseTreeOfB.map(b => Bad(b)), Nil, nextRnd)
            }
            else {
              val (nextRoseTreeOfG, _, nextRnd) = genOfG.next(szp, Nil, rnd)
              (nextRoseTreeOfG.map(g => Good(g)), Nil, nextRnd)
            }
        }
      }
    }

  // Note that this is identical to orGenerator *except* that the sides are reversed:
  // Right is "Good", and Left is "Bad".
  /**
    * Given [[Generator]]s for two types, [[L]] and [[R]], this provides one for `Either[L, R]`.
    *
    * @param genOfL a [[Generator]] that produces type [[L]]
    * @param genOfR a [[Generator]] that produces type [[R]]
    * @tparam L the "left" type for an [[Either]]
    * @tparam R the "right" type for an [[Either]]
    * @return a [[Generator]] that produces `Either[L, R]`
    */
  implicit def eitherGenerator[L, R](implicit genOfL: Generator[L], genOfR: Generator[R]): Generator[Either[L, R]] =
    new Generator[Either[L, R]] {

      case class NextRoseTree(value: Either[L, R]) extends RoseTree[Either[L, R]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Either[L, R]]], Randomizer) = {
          value match {
            case Right(r) => {
              val (rightRt, _, nextRnd) = genOfR.next(SizeParam(1, 0, 1), List(r), rndPassedToShrinks)
              val (rShrink, nextNextRnd) = rightRt.shrinks(nextRnd)
              (rShrink.map(rt => rt.map(Right(_): Either[L, R])), nextNextRnd)
            }
            case Left(l) => {
              val (leftRt, _, nextRnd) = genOfL.next(SizeParam(1, 0, 1), List(l), rndPassedToShrinks)
              val (lShrink, nextNextRnd) = leftRt.shrinks(nextRnd)
              (lShrink.map(rt => rt.map(Left(_): Either[L, R])), nextNextRnd)
            }
          }
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Either[L, R]], Randomizer) = {
        val (edgesOfL, nextRnd) = genOfL.initEdges(maxLength, rnd)
        val (edgesOfR, nextNextRnd) = genOfR.initEdges(maxLength, nextRnd)
        // Fill up to maxLength, favoring Right over Left if maxLength is odd. Else just dividing it
        // down the middle, half Right, half Left. And filling in with the other if one side runs out.
        @tailrec
        def loop(count: Int, remainingR: List[R], remainingL: List[L], acc: List[Either[L, R]]): List[Either[L, R]] = {
          (count, remainingR, remainingL) match {
            case (0, _, _) => acc
            case (_, Nil, Nil) => acc
            case (c, rHead :: rTail, Nil) => loop(c - 1, rTail, Nil, Right(rHead) :: acc)
            case (c, Nil, lHead :: lTail) => loop(c - 1, Nil, lTail, Left(lHead) :: acc)
            case (c, rHead :: rTail, _) if c % 2 == 0 => loop(c - 1, rTail, remainingL, Right(rHead) :: acc)
            case (c, _, lHead :: lTail) => loop(c - 1, remainingR, lTail, Left(lHead) :: acc)
          }
        }
        (loop(maxLength, edgesOfR, edgesOfL, Nil), nextNextRnd)
      }

      override def canonicals(rnd: Randomizer): (Iterator[Either[L, R]], Randomizer) = {
        val (rightCanon, nextRnd) = genOfR.canonicals(rnd)
        val (leftCanon, nextNextRnd) = genOfL.canonicals(nextRnd)

        (rightCanon.map(Right(_)) ++ leftCanon.map(Left(_)), nextNextRnd)
      }

      def next(szp: SizeParam, edges: List[Either[L, R]], rnd: Randomizer): (RoseTree[Either[L, R]], List[Either[L, R]], Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val (nextInt, nextRnd) = rnd.nextInt
            if (nextInt % 4 == 0) {
              // TODO: Here I was not sure if I should just map the RoseTree or takes
              // its value and wrap that in a shrink call. Might be the same thing ultimately.
              // Will check that later. Actually I'll try mapping first.
              val (nextRoseTreeOfL, _, nextRnd) = genOfL.next(szp, Nil, rnd)
              (nextRoseTreeOfL.map(l => Left(l)), Nil, nextRnd)
            }
            else {
              val (nextRoseTreeOfR, _, nextRnd) = genOfR.next(szp, Nil, rnd)
              (nextRoseTreeOfR.map(r => Right(r)), Nil, nextRnd)
            }
        }
      }
    }

  /**
    * Given [[Generator]]s for types [[A]] and [[B]], get one that produces Tuples of those types.
    *
    * [[tuple2Generator]] (and its variants, up through [[tuple22Generator]]) will create [[Generator]]s on
    * demand for essentially arbitrary Tuples, so long as you have [[Generator]]s in implicit scope for all
    * of the component types.
    *
    * @param genOfA a [[Generator]] for type [[A]]
    * @param genOfB a [[Generator]] for type [[B]]
    * @tparam A the first type in the Tuple
    * @tparam B the second type in the Tuple
    * @return a [[Generator]] that produces the desired types, Tupled together.
    */
  implicit def tuple2Generator[A, B](implicit genOfA: Generator[A], genOfB: Generator[B]): Generator[(A, B)] =
    new GeneratorFor2[A, B, (A, B)]((a: A, b: B) => (a, b), (c: (A, B)) => c)(genOfA, genOfB)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple3Generator[A, B, C](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C]): Generator[(A, B, C)] = {
    new GeneratorFor3[A, B, C, (A, B, C)]((a: A, b: B, c: C) => (a, b, c), (d: (A, B, C)) => d)(genOfA, genOfB, genOfC)
  }

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple4Generator[A, B, C, D](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D]): Generator[(A, B, C, D)] = {
    new GeneratorFor4[A, B, C, D, (A, B, C, D)]((a: A, b: B, c: C, d: D) => (a, b, c, d), (d: (A, B, C, D)) => d)(genOfA, genOfB, genOfC, genOfD)
  }

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple5Generator[A, B, C, D, E](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E]): Generator[(A, B, C, D, E)] =
    new GeneratorFor5[A, B, C, D, E, (A, B, C, D, E)]((a: A, b: B, c: C, d: D, e: E) => (a, b, c, d, e), (f: (A, B, C, D, E)) => f)(genOfA, genOfB, genOfC, genOfD, genOfE)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple6Generator[A, B, C, D, E, F](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F]): Generator[(A, B, C, D, E, F)] =
    new GeneratorFor6[A, B, C, D, E, F, (A, B, C, D, E, F)]((a: A, b: B, c: C, d: D, e: E, f: F) => (a, b, c, d, e, f), (g: (A, B, C, D, E, F)) => g)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple7Generator[A, B, C, D, E, F, G](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G]): Generator[(A, B, C, D, E, F, G)] =
    new GeneratorFor7[A, B, C, D, E, F, G, (A, B, C, D, E, F, G)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G) => (a, b, c, d, e, f, g), (h: (A, B, C, D, E, F, G)) => h)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple8Generator[A, B, C, D, E, F, G, H](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H]): Generator[(A, B, C, D, E, F, G, H)] =
    new GeneratorFor8[A, B, C, D, E, F, G, H, (A, B, C, D, E, F, G, H)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) => (a, b, c, d, e, f, g, h), (i: (A, B, C, D, E, F, G, H)) => i)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple9Generator[A, B, C, D, E, F, G, H, I](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I]): Generator[(A, B, C, D, E, F, G, H, I)] =
    new GeneratorFor9[A, B, C, D, E, F, G, H, I, (A, B, C, D, E, F, G, H, I)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) => (a, b, c, d, e, f, g, h, i), (j: (A, B, C, D, E, F, G, H, I)) => j)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple10Generator[A, B, C, D, E, F, G, H, I, J](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J]): Generator[(A, B, C, D, E, F, G, H, I, J)] =
    new GeneratorFor10[A, B, C, D, E, F, G, H, I, J, (A, B, C, D, E, F, G, H, I, J)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) => (a, b, c, d, e, f, g, h, i, j), (k: (A, B, C, D, E, F, G, H, I, J)) => k)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple11Generator[A, B, C, D, E, F, G, H, I, J, K](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K]): Generator[(A, B, C, D, E, F, G, H, I, J, K)] =
    new GeneratorFor11[A, B, C, D, E, F, G, H, I, J, K, (A, B, C, D, E, F, G, H, I, J, K)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) => (a, b, c, d, e, f, g, h, i, j, k), (l: (A, B, C, D, E, F, G, H, I, J, K)) => l)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple12Generator[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    new GeneratorFor12[A, B, C, D, E, F, G, H, I, J, K, L, (A, B, C, D, E, F, G, H, I, J, K, L)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L) => (a, b, c, d, e, f, g, h, i, j, k, l), (m: (A, B, C, D, E, F, G, H, I, J, K, L)) => m)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    new GeneratorFor13[A, B, C, D, E, F, G, H, I, J, K, L, M, (A, B, C, D, E, F, G, H, I, J, K, L, M)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M) => (a, b, c, d, e, f, g, h, i, j, k, l, m), (n: (A, B, C, D, E, F, G, H, I, J, K, L, M)) => n)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    new GeneratorFor14[A, B, C, D, E, F, G, H, I, J, K, L, M, N, (A, B, C, D, E, F, G, H, I, J, K, L, M, N)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n), (o: (A, B, C, D, E, F, G, H, I, J, K, L, M, N)) => o)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    new GeneratorFor15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), (p: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)) => p)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    new GeneratorFor16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), (q: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)) => q)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    new GeneratorFor17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q), (r: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)) => r)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    new GeneratorFor18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r), (s: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)) => s)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    new GeneratorFor19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s), (t: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)) => t)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    new GeneratorFor20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t), (u: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)) => u)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T], genOfU: Generator[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    new GeneratorFor21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u), (v: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)) => v)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU)

  /**
    * See [[tuple2Generator]].
    */
  implicit def tuple22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfA: Generator[A], genOfB: Generator[B], genOfC: Generator[C], genOfD: Generator[D], genOfE: Generator[E], genOfF: Generator[F], genOfG: Generator[G], genOfH: Generator[H], genOfI: Generator[I], genOfJ: Generator[J], genOfK: Generator[K], genOfL: Generator[L], genOfM: Generator[M], genOfN: Generator[N], genOfO: Generator[O], genOfP: Generator[P], genOfQ: Generator[Q], genOfR: Generator[R], genOfS: Generator[S], genOfT: Generator[T], genOfU: Generator[U], genOfV: Generator[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    new GeneratorFor22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)]((a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V) => (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v), (w: (A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)) => w)(genOfA, genOfB, genOfC, genOfD, genOfE, genOfF, genOfG, genOfH, genOfI, genOfJ, genOfK, genOfL, genOfM, genOfN, genOfO, genOfP, genOfQ, genOfR, genOfS, genOfT, genOfU, genOfV)

  /**
    * Given a [[Generator]] for type [[T]], this creates one for a [[Vector]] of [[T]].
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
    */
  implicit def vectorGenerator[T](implicit genOfT: Generator[T]): Generator[Vector[T]] with HavingLength[Vector[T]] =
    new Generator[Vector[T]] with HavingLength[Vector[T]] {

      case class NextRoseTree(value: Vector[T]) extends RoseTree[Vector[T]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Vector[T]]], Randomizer) = {
          if (value.isEmpty)
            (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            val halfSize = value.length / 2
            val firstHalf = value.take(halfSize)
            val secondHalf = value.drop(halfSize)
            val tail = value.tail
            val init = value.init
            (LazyListOrStream(firstHalf, secondHalf, tail, init).distinct.filter(_ != value).map(NextRoseTree(_)), rndPassedToShrinks)
          }
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[Vector[T]] =
        new Generator[Vector[T]] {

          def next(ignoredSzp: org.scalatest.prop.SizeParam, edges: List[Vector[T]], rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], List[Vector[T]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: Vector[T], rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], List[Vector[T]], org.scalatest.prop.Randomizer) =
              if (result.length == targetSize)
                (NextRoseTree(result), edges, rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfT.next(szp, List.empty, rnd)
                loop(targetSize, result :+ nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, Vector.empty, nextRnd)
          }
        }

      def next(szp: org.scalatest.prop.SizeParam, edges: List[Vector[T]],rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], List[Vector[T]], org.scalatest.prop.Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val gen = generatorWithSize(szp)
            gen.next(szp, List.empty, rnd)
        }
      }

      override def canonicals(rnd: Randomizer): (Iterator[Vector[T]], Randomizer) = {
        val (canonicalsOfT, rnd1) = genOfT.canonicals(rnd)
        (canonicalsOfT.map(t => Vector(t)), rnd1)
      }

      // Members declared in org.scalatest.prop.HavingSize
      def havingSize(len: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Vector[T]] = generatorWithSize(SizeParam(len, 0, len))
      def havingSizesBetween(from: org.scalactic.anyvals.PosZInt,to: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Vector[T]] = {
        require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
        generatorWithSize(SizeParam(from, PosZInt.ensuringValid(to - from), from))
      }
      def havingSizesDeterminedBy(f: org.scalatest.prop.SizeParam => org.scalatest.prop.SizeParam): org.scalatest.prop.Generator[Vector[T]] =
        new Generator[Vector[T]] {
          def next(szp: org.scalatest.prop.SizeParam, edges: List[Vector[T]],rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], List[Vector[T]], org.scalatest.prop.Randomizer) = {
            edges match {
              case head :: tail =>
                (NextRoseTree(head), tail, rnd)
              case _ =>
                val s = f(szp)
                val gen = generatorWithSize(s)
                gen.next(s, List.empty, rnd)
            }
          }
        }
    }

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
    */
  implicit def setGenerator[T](implicit genOfT: Generator[T]): Generator[Set[T]] with HavingSize[Set[T]] =
    new Generator[Set[T]] with HavingSize[Set[T]] {

      case class NextRoseTree(value: Set[T]) extends RoseTree[Set[T]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Set[T]]], Randomizer) = {
          if (value.isEmpty)
            (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            val halfSize = value.size / 2
            val firstHalf = value.take(halfSize)
            val secondHalf = value.drop(halfSize)
            val tail = value.tail
            val init = value.init
            (LazyListOrStream(firstHalf, secondHalf, tail, init).distinct.filter(_ != value).map(NextRoseTree(_)), rndPassedToShrinks)
          }
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[Set[T]] =
        new Generator[Set[T]] {

          def next(ignoredSzp: org.scalatest.prop.SizeParam, edges: List[Set[T]], rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], List[Set[T]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: Set[T], rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], List[Set[T]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result), edges, rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfT.next(szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, Set.empty, nextRnd)
          }
        }

      def next(szp: org.scalatest.prop.SizeParam, edges: List[Set[T]],rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], List[Set[T]], org.scalatest.prop.Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case Nil =>
            val gen = generatorWithSize(szp)
            gen.next(szp, List.empty, rnd)
        }
      }

      override def canonicals(rnd: Randomizer): (Iterator[Set[T]], Randomizer) = {
        val (canonicalsOfT, rnd1) = genOfT.canonicals(rnd)
        (canonicalsOfT.map(t => Set(t)), rnd1)
      }

      // Members declared in org.scalatest.prop.HavingSize
      def havingSize(len: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Set[T]] = generatorWithSize(SizeParam(len, 0, len))
      def havingSizesBetween(from: org.scalactic.anyvals.PosZInt,to: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Set[T]] = {
        require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
        generatorWithSize(SizeParam(from, PosZInt.ensuringValid(to - from), from))
      }
      def havingSizesDeterminedBy(f: org.scalatest.prop.SizeParam => org.scalatest.prop.SizeParam): org.scalatest.prop.Generator[Set[T]] =
        new Generator[Set[T]] {
          def next(szp: org.scalatest.prop.SizeParam, edges: List[Set[T]],rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], List[Set[T]], org.scalatest.prop.Randomizer) = {
            edges match {
              case head :: tail =>
                (NextRoseTree(head), tail, rnd)
              case _ =>
                val s = f(szp)
                val gen = generatorWithSize(s)
                gen.next(s, List.empty, rnd)
            }
          }
        }
    }

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
    */
  implicit def sortedSetGenerator[T](implicit genOfT: Generator[T], ordering: Ordering[T]): Generator[SortedSet[T]] with HavingSize[SortedSet[T]] =
    new Generator[SortedSet[T]] with HavingSize[SortedSet[T]] {

      case class NextRoseTree(value: SortedSet[T]) extends RoseTree[SortedSet[T]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[SortedSet[T]]], Randomizer) = {
          if (value.isEmpty)
            (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            val halfSize = value.size / 2
            val firstHalf = value.take(halfSize)
            val secondHalf = value.drop(halfSize)
            val tail = value.tail
            val init = value.init
            (LazyListOrStream(firstHalf, secondHalf, tail, init).distinct.filter(_ != value).map(NextRoseTree(_)), rndPassedToShrinks)
          }
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[SortedSet[T]] =
        new Generator[SortedSet[T]] {

          def next(ignoredSzp: org.scalatest.prop.SizeParam, edges: List[SortedSet[T]], rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], List[SortedSet[T]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: SortedSet[T], rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], List[SortedSet[T]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result), edges, rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfT.next(szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, SortedSet.empty, nextRnd)
          }
        }

      def next(szp: org.scalatest.prop.SizeParam, edges: List[SortedSet[T]],rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], List[SortedSet[T]], org.scalatest.prop.Randomizer) = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)
          case _ =>
            val gen = generatorWithSize(szp)
            gen.next(szp, List.empty, rnd)
        }
      }

      override def canonicals(rnd: Randomizer): (Iterator[SortedSet[T]], Randomizer) = {
        val (canonicalsOfT, rnd1) = genOfT.canonicals(rnd)
        (canonicalsOfT.map(t => SortedSet(t)), rnd1)
      }

      // Members declared in org.scalatest.prop.HavingSize
      def havingSize(len: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[SortedSet[T]] = generatorWithSize(SizeParam(len, 0, len))
      def havingSizesBetween(from: org.scalactic.anyvals.PosZInt,to: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[SortedSet[T]] = {
        require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
        generatorWithSize(SizeParam(from, PosZInt.ensuringValid(to - from), from))
      }
      def havingSizesDeterminedBy(f: org.scalatest.prop.SizeParam => org.scalatest.prop.SizeParam): org.scalatest.prop.Generator[SortedSet[T]] =
        new Generator[SortedSet[T]] {
          def next(szp: org.scalatest.prop.SizeParam, edges: List[SortedSet[T]], rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], List[SortedSet[T]], org.scalatest.prop.Randomizer) = {
            edges match {
              case head :: tail =>
                (NextRoseTree(head), tail, rnd)
              case _ =>
                val s = f(szp)
                val gen = generatorWithSize(s)
                gen.next(s, List.empty, rnd)
            }
          }
        }
    }

  /**
    * Given a [[Generator]] that produces Tuples of key/value pairs, this gives you one that produces [[Map]]s
    * with those pairs.
    *
    * If you are simply looking for random pairing of the key and value types, this is pretty easy to use:
    * if both the key and value types have [[Generator]]s, then the Tuple and Map ones will be automatically
    * and implicitly created when you need them.
    *
    * The resulting [[Generator]] also has the [[HavingSize]] trait, so you can use it to generate [[Map]]s
    * with specific sizes.
    *
    * @param genOfTuple2KV a [[Generator]] that produces Tuples of [[K]] and [[V]]
    * @tparam K the type of the keys for the [[Map]]
    * @tparam V the type of the values for the [[Map]]
    * @return a [[Generator]] of [[Map]]s from [[K]] to [[V]]
    */
  implicit def mapGenerator[K, V](implicit genOfTuple2KV: Generator[(K, V)]): Generator[Map[K, V]] with HavingSize[Map[K, V]] =
    new Generator[Map[K, V]] with HavingSize[Map[K, V]] {

      // TODO This only uses Roses. Check that we don't need RoseTrees.
      case class NextRoseTree(value: Map[K, V]) extends RoseTree[Map[K, V]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[Map[K, V]]], Randomizer) = {
          if (value.isEmpty)
            (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            val halfSize = value.size / 2
            val firstHalf = value.take(halfSize)
            val secondHalf = value.drop(halfSize)
            val tail = value.tail
            val init = value.init
            (LazyListOrStream(firstHalf, secondHalf, tail, init).distinct.filter(_ != value).map(NextRoseTree(_)), rndPassedToShrinks)
          }
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[Map[K, V]] =
        new Generator[Map[K, V]] {

          def next(ignoredSzp: org.scalatest.prop.SizeParam, edges: List[Map[K, V]], rnd: org.scalatest.prop.Randomizer): (RoseTree[Map[K, V]], List[Map[K, V]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: Map[K, V], rnd: org.scalatest.prop.Randomizer): (RoseTree[Map[K, V]], List[Map[K, V]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result), edges, rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfTuple2KV.next (szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, Map.empty, nextRnd)
          }
        }

      def next(szp: org.scalatest.prop.SizeParam, edges: List[Map[K, V]], rnd: org.scalatest.prop.Randomizer): Tuple3[RoseTree[Map[K, V]], List[Map[K, V]], org.scalatest.prop.Randomizer] = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)

          case Nil =>
            val gen = generatorWithSize(szp)
            gen.next(szp, List.empty, rnd)
        }
      }

      override def canonicals(rnd: Randomizer): (Iterator[Map[K, V]], Randomizer) = {
        val (canonicalsOfKV, rnd1) = genOfTuple2KV.canonicals(rnd)
        (canonicalsOfKV.map(t => Map(t)), rnd1)
      }

      // Members declared in org.scalatest.prop.HavingSize
      def havingSize(len: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Map[K, V]] = generatorWithSize(SizeParam(len, 0, len))
      def havingSizesBetween(from: org.scalactic.anyvals.PosZInt,to: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Map[K, V]] = {
        require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
        generatorWithSize(SizeParam(from, PosZInt.ensuringValid(to - from), from))
      }
      def havingSizesDeterminedBy(f: org.scalatest.prop.SizeParam => org.scalatest.prop.SizeParam): org.scalatest.prop.Generator[Map[K, V]] =
        new Generator[Map[K, V]] {
          def next(szp: org.scalatest.prop.SizeParam, edges: List[Map[K, V]],rnd: org.scalatest.prop.Randomizer): (RoseTree[Map[K, V]], List[Map[K, V]], org.scalatest.prop.Randomizer) = {
            edges match {
              case head :: tail =>
                (Rose(head), tail, rnd)
              case _ =>
                val s = f(szp)
                val gen = generatorWithSize(s)
                gen.next(s, List.empty, rnd)
            }
          }
        }
    }

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
    */
  implicit def sortedMapGenerator[K, V](implicit genOfTuple2KV: Generator[(K, V)], ordering: Ordering[K]): Generator[SortedMap[K, V]] with HavingSize[SortedMap[K, V]] =
    new Generator[SortedMap[K, V]] with HavingSize[SortedMap[K, V]] {

      case class NextRoseTree(value: SortedMap[K, V]) extends RoseTree[SortedMap[K, V]] {
        def shrinks(rndPassedToShrinks: Randomizer): (LazyListOrStream[RoseTree[SortedMap[K, V]]], Randomizer) = {
          if (value.isEmpty)
            (LazyListOrStream.empty, rndPassedToShrinks)
          else {
            val halfSize = value.size / 2
            val firstHalf = value.take(halfSize)
            val secondHalf = value.drop(halfSize)
            val tail = value.tail
            val init = value.init
            (LazyListOrStream(firstHalf, secondHalf, tail, init).distinct.filter(_ != value).map(NextRoseTree(_)), rndPassedToShrinks)
          }
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[SortedMap[K, V]] =
        new Generator[SortedMap[K, V]] {

          def next(ignoredSzp: org.scalatest.prop.SizeParam, edges: List[SortedMap[K, V]], rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedMap[K, V]], List[SortedMap[K, V]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: SortedMap[K, V], rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedMap[K, V]], List[SortedMap[K, V]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result), edges, rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfTuple2KV.next (szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, SortedMap.empty[K, V], nextRnd)
          }
        }

      def next(szp: org.scalatest.prop.SizeParam, edges: List[SortedMap[K, V]], rnd: org.scalatest.prop.Randomizer): Tuple3[RoseTree[SortedMap[K, V]], List[SortedMap[K, V]], org.scalatest.prop.Randomizer] = {
        edges match {
          case head :: tail =>
            (NextRoseTree(head), tail, rnd)

          case Nil =>
            val gen = generatorWithSize(szp)
            gen.next(szp, List.empty, rnd)
        }
      }

      override def canonicals(rnd: Randomizer): (Iterator[SortedMap[K, V]], Randomizer) = {
        val (canonicalsOfKV, rnd1) = genOfTuple2KV.canonicals(rnd)
        (canonicalsOfKV.map(t => SortedMap(t)), rnd1)
      }

      // Members declared in org.scalatest.prop.HavingSize
      def havingSize(len: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[SortedMap[K, V]] = generatorWithSize(SizeParam(len, 0, len))
      def havingSizesBetween(from: org.scalactic.anyvals.PosZInt,to: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[SortedMap[K, V]] = {
        require(from != to, Resources.fromEqualToToHavingLengthsBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingLengthsBetween(from, to))
        generatorWithSize(SizeParam(from, PosZInt.ensuringValid(to - from), from))
      }
      def havingSizesDeterminedBy(f: org.scalatest.prop.SizeParam => org.scalatest.prop.SizeParam): org.scalatest.prop.Generator[SortedMap[K, V]] =
        new Generator[SortedMap[K, V]] {
          def next(szp: org.scalatest.prop.SizeParam, edges: List[SortedMap[K, V]],rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedMap[K, V]], List[SortedMap[K, V]], org.scalatest.prop.Randomizer) = {
            edges match {
              case head :: tail =>
                (NextRoseTree(head), tail, rnd)
              case _ =>
                val s = f(szp)
                val gen = generatorWithSize(s)
                gen.next(s, List.empty, rnd)
            }
          }
        }
    }
}


