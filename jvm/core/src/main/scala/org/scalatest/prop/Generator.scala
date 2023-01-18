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
  * Canonicals should always be in order from "largest" to "smallest", in the shrinking sense.
  * This is ''not'' the same thing as starting with the largest number and ending with the smallest
  * numerically, though! For example, the canonicals for [[Generator.byteGenerator]] are:
  * {{{
  * private val byteCanonicals: LazyListOrStream[Byte] = LazyListOrStream(-3, 3, -2, 2, -1, 1, 0)
  * }}}
  * Zero is "smallest" -- the most-shrunk Byte, because it is the simplest for humans. Shrinking
  * should really be called simplifying.
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
  * "largest" shrunken values should be returned at the front of this LazyListOrStream, with more shrunken values later.
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

  def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer)

  private final val MaxLoopCount: Int = 100000

  def roseTreeOfEdge(edge: T, sizeParam: SizeParam, isValidFun: (T, SizeParam) => Boolean): RoseTree[T] = Rose(edge)

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
  def next(szp: SizeParam, edges: List[T], rnd: Randomizer): (RoseTree[T], List[T], Randomizer) = 
    edges.filter(e => isValid(e, szp)) match {
      case head :: tail =>
        (roseTreeOfEdge(head, szp, isValid), tail, rnd)
      case _ =>
        @tailrec
        def loop(count: Int, nextRnd: Randomizer): (RoseTree[T], Randomizer) = {
          if (count > MaxLoopCount)
            throw new IllegalStateException(s"A Generator produced by calling filter or withFilter on another Generator (possibly by using an 'if' clause in a for expression) has filtered out $MaxLoopCount objects in a row in its next method, so aborting. Please define the Generator without using filter or withFilter.")
          val (b, rnd2) = nextImpl(szp, isValid, nextRnd)
          if (isValid(b.value, szp))
            (b, rnd2)
          else
            loop(count + 1, rnd2)  
        }
        val (b, rnd2) = loop(0, rnd)
        (b, Nil, rnd2)
    }

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
      def nextImpl(szp: SizeParam, isValidFun: (U, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[U], Randomizer) = {
        val (nextRoseTreeOfT, _, nextRandomizer) = thisGeneratorOfT.next(szp, Nil, rnd)
        (nextRoseTreeOfT.map(f), nextRandomizer)
      }
      override def canonicals: LazyListOrStream[RoseTree[U]] = {
        val cansOfT = thisGeneratorOfT.canonicals
        cansOfT.map(rt => rt.map(f))
      }
    }

  // This map method can be used if the function from T to U is invertible. For example, if f
  // is a function from Int => Option[Int] that just wraps each Int in a Some, (n: Int) => (Some(n): Option[Int]),
  // the g function can be a function that unwraps it back to Int: (n: Option[Int]) => n.get. The point of this
  // method is to map the Generator while preserving an intersting shrinksForValue method. To do that we need
  // the U to T function, because shrinksToValue takes a U in the resulting Generator[U].
  def mapInvertible[U](f: T => U, g: U => T): Generator[U] = {
    new Generator[U] { thisGeneratorOfU =>
      private val underlying: Generator[U] = thisGeneratorOfT.map(f)
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[U], Randomizer) = underlying.initEdges(maxLength, rnd)
      def nextImpl(szp: SizeParam, isValidFun: (U, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[U], Randomizer) = underlying.nextImpl(szp, isValidFun, rnd)
      override def map[V](f: U => V): Generator[V] = underlying.map(f)
      override def flatMap[V](f: U => Generator[V]): Generator[V] = underlying.flatMap(f)
      override def withFilter(f: U => Boolean): Generator[U] = underlying.withFilter(f)
      override def filter(f: U => Boolean): Generator[U] = underlying.filter(f)
      override def canonicals: LazyListOrStream[RoseTree[U]] = underlying.canonicals
      override def shrinksForValue(theValue: U): Option[LazyListOrStream[RoseTree[U]]] = {
        val optRts: Option[LazyListOrStream[RoseTree[T]]] = thisGeneratorOfT.shrinksForValue(g(theValue))
        optRts.map(rts => rts.map(rt => rt.map(f)))
      }
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

      def nextImpl(szp: SizeParam, isValidFun: (U, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[U], Randomizer) = {
        val (nextRoseTreeOfT, _, nextRandomizer) = thisGeneratorOfT.next(szp, Nil, rnd)
        val genOfU: Generator[U] = f(nextRoseTreeOfT.value)
        val (u, _, nextNextRandomizer) = genOfU.next(szp, Nil, nextRandomizer)
        (u, nextNextRandomizer)
      }

      override def canonicals: LazyListOrStream[RoseTree[U]] = {
        val canonicalsOfT = thisGeneratorOfT.canonicals
        def getCanonicals(rt: RoseTree[T]): LazyListOrStream[RoseTree[U]] = {
          val genOfU: Generator[U] = f(rt.value)
          genOfU.canonicals
        }
        canonicalsOfT.flatMap(getCanonicals)
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
      override def roseTreeOfEdge(edge: T, sizeParam: SizeParam, isValidFun: (T, SizeParam) => Boolean): RoseTree[T] = thisGeneratorOfT.roseTreeOfEdge(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[T], Randomizer) = thisGeneratorOfT.nextImpl(szp, isValidFun, rnd)
      override def isValid(value: T, size: SizeParam): Boolean = f(value)
    }

  def isValid(value: T, size: SizeParam): Boolean = true

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
    * returns an empty [[LazyListOrStream]].
    *
    * This function takes a [[Randomizer]] to use as a parameter, in case canonical generation
    * for this type has a random element to it. If you use this [[Randomizer]], return the
    * ''next'' one. If you don't use it, just use the passed-in one.
    *
    * @param rnd a [[Randomizer]] to use if this function requires any random data
    * @return the canonical values for this type (if any), and the next [[Randomizer]]
    */
  def canonicals: LazyListOrStream[RoseTree[T]] = LazyListOrStream.empty

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

  // Could just use an empty LazyList to say I don't have any, but I think we should differentiate between we aren't producing
  // any from the value is already fully shrunk (like "" for String).
  def shrinksForValue(theValue: T): Option[LazyListOrStream[RoseTree[T]]] = None
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
      def nextImpl(szp: SizeParam, isValidFun: (Boolean, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Boolean], Randomizer) = {
        val (bit, nextRnd) = rnd.nextBit
        val bool = if (bit == 1) true else false
        (Rose(bool), nextRnd)
      }

      override def toString = "Generator[Boolean]"
    }  

  /**
    * A [[Generator]] that produces [[Byte]] values.
    */
  implicit val byteGenerator: Generator[Byte] = 
    new Generator[Byte] {
      case class NextRoseTree(value: Byte)(sizeParam: SizeParam, isValidFun: (Byte, SizeParam) => Boolean) extends RoseTree[Byte] {
        def shrinks: LazyListOrStream[RoseTree[Byte]] = {
          def resLazyListOrStream(theValue: Byte): LazyListOrStream[RoseTree[Byte]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Byte = (theValue / 2).toByte
              if (half == 0) { 
                if (isValidFun(0.toByte, sizeParam))
                  Rose(0.toByte) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty  
              }
              else
                LazyListOrStream((-half).toByte, half.toByte).filter(v => isValidFun(v, sizeParam))
                                                      .map(v => NextRoseTree(v)(sizeParam, isValidFun)) #::: resLazyListOrStream(half)
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Byte], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(byteEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      
      override def roseTreeOfEdge(edge: Byte, sizeParam: SizeParam, isValidFun: (Byte, SizeParam) => Boolean): RoseTree[Byte] = {
        NextRoseTree(edge)(sizeParam, isValidFun)
      }
      def nextImpl(szp: SizeParam, isValidFun: (Byte, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Byte], Randomizer) = {
        val (b, rnd2) = rnd.nextByte
        (NextRoseTree(b)(szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[Byte]] = {
        case class CanonicalRoseTree(value: Byte) extends RoseTree[Byte] {
          def shrinks: LazyListOrStream[RoseTree[Byte]] = {
            def resLazyListOrStream(theValue: Byte): LazyListOrStream[RoseTree[Byte]] = {
              if (theValue == 0) LazyListOrStream.empty
              else {
                val minusOne: Byte = (theValue - 1).toByte
                if (minusOne == 0) Rose(0.toByte) #:: LazyListOrStream.empty
                else CanonicalRoseTree((-minusOne).toByte) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4).shrinks
      }
      override def toString = "Generator[Byte]"

      // For now I will not take a Randomizer. I'm hoping we can just get rid of it in shrinks. Shrinks can just
      // be based on the values being shrunk.
      override def shrinksForValue(valueToShrink: Byte): Option[LazyListOrStream[RoseTree[Byte]]] = Some(NextRoseTree(valueToShrink)(SizeParam(1, 0, 1), this.isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces [[Short]] values.
    */
  implicit val shortGenerator: Generator[Short] =
    new Generator[Short] {

      case class NextRoseTree(value: Short)(sizeParam: SizeParam, isValidFun: (Short, SizeParam) => Boolean) extends RoseTree[Short] {
        def shrinks: LazyListOrStream[RoseTree[Short]] = {
          def resLazyListOrStream(theValue: Short): LazyListOrStream[RoseTree[Short]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Short = (theValue / 2).toShort
              if (half == 0) {
                if (isValidFun(0.toShort, sizeParam))
                  Rose(0.toShort) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty
              }
              else 
                LazyListOrStream((-half).toShort, half.toShort).filter(v => isValidFun(v, sizeParam))
                                                      .map(v => NextRoseTree(v)(sizeParam, isValidFun)) #::: resLazyListOrStream(half)
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Short], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(shortEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: Short, sizeParam: SizeParam, isValidFun: (Short, SizeParam) => Boolean): RoseTree[Short] = NextRoseTree(edge)(sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (Short, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Short], Randomizer) = {
        val (s, rnd2) = rnd.nextShort
        (NextRoseTree(s)(szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[Short]] = {
        case class CanonicalRoseTree(value: Short) extends RoseTree[Short] {
          def shrinks: LazyListOrStream[RoseTree[Short]] = {
            def resLazyListOrStream(theValue: Short): LazyListOrStream[RoseTree[Short]] = {
              if (theValue == 0) LazyListOrStream.empty
              else {
                val minusOne: Short = (theValue - 1).toShort
                if (minusOne == 0) Rose(0.toShort) #:: LazyListOrStream.empty
                else CanonicalRoseTree((-minusOne).toShort) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4).shrinks
      }
      override def toString = "Generator[Short]"
      override def shrinksForValue(valueToShrink: Short): Option[LazyListOrStream[RoseTree[Short]]] = Some(NextRoseTree(valueToShrink)(SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces [[Char]] values.
    */
  implicit val charGenerator: Generator[Char] =
    new Generator[Char] {

      case class NextRoseTree(value: Char)(sizeParam: SizeParam, isValidFun: (Char, SizeParam) => Boolean) extends RoseTree[Char] {
        def shrinks: LazyListOrStream[RoseTree[Char]] = {
          val userFriendlyChars = "9876543210ZYXWVUTSRQPONMLKJIHGFEDCBAzyxwvutsrqponmljkihgfedcba"
          // In this one we accept any of these characters. Else we try them in the above order.
          val valueIdx = userFriendlyChars.indexOf(value)
          if (valueIdx < 0) LazyListOrStream.empty
          else {
            def resLazyListOrStream(theIndex: Int): LazyListOrStream[RoseTree[Char]] = {
              if (theIndex == userFriendlyChars.length) LazyListOrStream.empty
              else {
                val s = userFriendlyChars(theIndex)
                if (isValidFun(s, sizeParam))
                  NextRoseTree(s)(sizeParam, isValidFun) #:: resLazyListOrStream(theIndex + 1)
                else
                  resLazyListOrStream(theIndex + 1)  
              }
            }
            resLazyListOrStream(valueIdx + 1)
          }
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Char], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(charEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: Char, sizeParam: SizeParam, isValidFun: (Char, SizeParam) => Boolean): RoseTree[Char] = NextRoseTree(edge)(sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (Char, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Char], Randomizer) = {
        val (c, rnd2) = rnd.nextChar
        (NextRoseTree(c)(szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[Char]] = {
        val lowerAlphaChars = "zyxwvutsrqponmljkihgfedcba"
        val theLength = lowerAlphaChars.length
        case class CanonicalRoseTree(valueIndex: Int) extends RoseTree[Char] {
          val value = lowerAlphaChars(valueIndex)
          def shrinks: LazyListOrStream[RoseTree[Char]] = {
            def resLazyListOrStream(nxtIndex: Int): LazyListOrStream[RoseTree[Char]] = {
              if (nxtIndex >= theLength) LazyListOrStream.empty // Return no shrinks if already at a
              else CanonicalRoseTree(nxtIndex) #:: resLazyListOrStream(nxtIndex + 1)
            }
            resLazyListOrStream(valueIndex + 1)
          }
       }

        def canonicalsResLazyListOrStream(theIndex: Int): LazyListOrStream[RoseTree[Char]] = {
          if (theIndex >= theLength) LazyListOrStream.empty // Return no shrinks if already at a
          else CanonicalRoseTree(theIndex) #:: canonicalsResLazyListOrStream(theIndex + 1)
        }
        canonicalsResLazyListOrStream(0)
      }

      override def toString = "Generator[Char]"
      override def shrinksForValue(valueToShrink: Char): Option[LazyListOrStream[RoseTree[Char]]] = Some(NextRoseTree(valueToShrink)(SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces [[Int]] values.
    */
  implicit val intGenerator: Generator[Int] =
    new Generator[Int] {

      case class NextRoseTree(value: Int, sizeParam: SizeParam, isValidFun: (Int, SizeParam) => Boolean) extends RoseTree[Int] {
        def shrinks: LazyListOrStream[RoseTree[Int]] = {
          def resLazyListOrStream(theValue: Int): LazyListOrStream[RoseTree[Int]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Int = theValue / 2
              if (half == 0) { 
                if (isValidFun(0, sizeParam))
                  Rose(0) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty
              }
              else 
                LazyListOrStream(-half, half).filter(v => isValidFun(v, sizeParam))
                                                      .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(half)
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Int], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(intEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: Int, sizeParam: SizeParam, isValidFun: (Int, SizeParam) => Boolean): RoseTree[Int] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (Int, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Int], Randomizer) = {
        val (i, rnd2) = rnd.nextInt
        (NextRoseTree(i, szp, isValidFun), rnd2)
      }
      override def toString = "Generator[Int]"
      override def canonicals: LazyListOrStream[RoseTree[Int]] = {
        case class CanonicalRoseTree(value: Int) extends RoseTree[Int] {
          def shrinks: LazyListOrStream[RoseTree[Int]] = {
            def resLazyListOrStream(theValue: Int): LazyListOrStream[RoseTree[Int]] = {
              if (theValue == 0) LazyListOrStream.empty
              else {
                val minusOne: Int = (theValue - 1).toInt
                if (minusOne == 0) Rose(0.toInt) #:: LazyListOrStream.empty
                else CanonicalRoseTree((-minusOne).toInt) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4).shrinks
      }
      override def shrinksForValue(valueToShrink: Int): Option[LazyListOrStream[RoseTree[Int]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces [[Long]] values.
    */
  implicit val longGenerator: Generator[Long] =
    new Generator[Long] {

      case class NextRoseTree(value: Long, sizeParam: SizeParam, isValidFun: (Long, SizeParam) => Boolean) extends RoseTree[Long] {
        def shrinks: LazyListOrStream[RoseTree[Long]] = {
          def resLazyListOrStream(theValue: Long): LazyListOrStream[RoseTree[Long]] = {
            if (theValue == 0) LazyListOrStream.empty
            else {
              val half: Long = (theValue / 2)
              if (half == 0) { 
                if (isValidFun(0, sizeParam))
                  Rose(0L) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty
              }
              else 
                LazyListOrStream(-half, half).filter(v => isValidFun(v, sizeParam))
                                                      .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(half)
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Long], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(longEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: Long, sizeParam: SizeParam, isValidFun: (Long, SizeParam) => Boolean): RoseTree[Long] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (Long, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Long], Randomizer) = {
        val (n, rnd2) = rnd.nextLong
        (NextRoseTree(n, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[Long]] = {
        case class CanonicalRoseTree(value: Long) extends RoseTree[Long] {
          def shrinks: LazyListOrStream[RoseTree[Long]] = {
            def resLazyListOrStream(theValue: Long): LazyListOrStream[RoseTree[Long]] = {
              if (theValue == 0) LazyListOrStream.empty
              else {
                val minusOne: Long = (theValue - 1).toLong
                if (minusOne == 0) Rose(0.toLong) #:: LazyListOrStream.empty
                else CanonicalRoseTree((-minusOne).toLong) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4).shrinks
      }
      override def toString = "Generator[Long]"
      override def shrinksForValue(valueToShrink: Long): Option[LazyListOrStream[RoseTree[Long]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces [[Float]] values.
    */
  implicit val floatGenerator: Generator[Float] =
    new Generator[Float] {

      case class NextRoseTree(value: Float, sizeParam: SizeParam, isValidFun: (Float, SizeParam) => Boolean) extends RoseTree[Float] {
        def shrinks: LazyListOrStream[RoseTree[Float]] = {
          def resLazyListOrStream(theValue: Float): LazyListOrStream[RoseTree[Float]] = {
            if (theValue == 0.0f)
              LazyListOrStream.empty
            else if (theValue <= 1.0f && theValue >= -1.0f)
              if (isValidFun(0.0f, sizeParam)) Rose(0.0f) #:: LazyListOrStream.empty else LazyListOrStream.empty
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
              LazyListOrStream(nearestNeg, nearest).filter(v => isValidFun(v, sizeParam))
                                                      .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(nearest)
            }  
            else {
              val sqrt: Float = math.sqrt(theValue.abs.toDouble).toFloat
              if (sqrt < 1.0f && sqrt >= -1.0) 
                if (isValidFun(0.0f, sizeParam)) Rose(0.0f) #:: LazyListOrStream.empty else LazyListOrStream.empty
              else {
                // Try both the negative and postive, negative first because positive is simpler for humans,
                // so more "shrunk."
                val whole: Float = sqrt.floor
                val negWhole: Float = -whole // math.rint((-whole).toDouble).toFloat
                LazyListOrStream(negWhole, whole).filter(v => isValidFun(v, sizeParam))
                                                 .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(whole)
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Float], Randomizer) = {
        (floatEdges.take(maxLength), rnd)
      }
      override def roseTreeOfEdge(edge: Float, sizeParam: SizeParam, isValidFun: (Float, SizeParam) => Boolean): RoseTree[Float] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (Float, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Float], Randomizer) = {
        val (f, rnd2) = rnd.nextFloat
        (NextRoseTree(f, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[Float]] = {
        case class CanonicalRoseTree(value: Float) extends RoseTree[Float] {
          def shrinks: LazyListOrStream[RoseTree[Float]] = {
            def resLazyListOrStream(theValue: Float): LazyListOrStream[RoseTree[Float]] = {
              if (theValue == 0) LazyListOrStream.empty
              else {
                val minusOne: Float = (theValue - 1.0f).toFloat
                if (minusOne == 0) Rose(0.toFloat) #:: LazyListOrStream.empty
                else CanonicalRoseTree((-minusOne).toFloat) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[Float]"
      override def shrinksForValue(valueToShrink: Float): Option[LazyListOrStream[RoseTree[Float]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces [[Double]] values.
    */
  implicit val doubleGenerator: Generator[Double] =
    new Generator[Double] {

      case class NextRoseTree(value: Double, sizeParam: SizeParam, isValidFun: (Double, SizeParam) => Boolean) extends RoseTree[Double] {
        def shrinks: LazyListOrStream[RoseTree[Double]] = {
          def resLazyListOrStream(theValue: Double): LazyListOrStream[RoseTree[Double]] = {

            if (theValue == 0.0)
              LazyListOrStream.empty
            else if (theValue <= 1.0 && theValue >= -1.0)
              if (isValidFun(0.0, sizeParam)) Rose(0.0) #:: LazyListOrStream.empty else LazyListOrStream.empty
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
              LazyListOrStream(nearestNeg, nearest).filter(v => isValidFun(v, sizeParam))
                                                      .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(nearest)
            }  
            else {
              val sqrt: Double = math.sqrt(theValue.abs)
              if (sqrt < 1.0 && sqrt >= -1.0) 
                if (isValidFun(0.0, sizeParam)) Rose(0.0) #:: LazyListOrStream.empty else LazyListOrStream.empty
              else {
                // Try both the negative and postive, negative first because positive is simpler for humans,
                // so more "shrunk."
                val whole: Double = sqrt.floor
                val negWhole: Double = -whole // math.rint((-whole).toDouble).toDouble
                LazyListOrStream(negWhole, whole).filter(v => isValidFun(v, sizeParam))
                                                 .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(whole)
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Double], Randomizer) = {
        (doubleEdges.take(maxLength), rnd)
      }
      override def roseTreeOfEdge(edge: Double, sizeParam: SizeParam, isValidFun: (Double, SizeParam) => Boolean): RoseTree[Double] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (Double, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Double], Randomizer) = {
        val (d, rnd2) = rnd.nextDouble
        (NextRoseTree(d, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[Double]] = {
        case class CanonicalRoseTree(value: Double) extends RoseTree[Double] {
          def shrinks: LazyListOrStream[RoseTree[Double]] = {
            def resLazyListOrStream(theValue: Double): LazyListOrStream[RoseTree[Double]] = {
              if (theValue == 0) LazyListOrStream.empty
              else {
                val minusOne: Double = (theValue - 1.0).toDouble
                if (minusOne == 0) Rose(0.toDouble) #:: LazyListOrStream.empty
                else CanonicalRoseTree((-minusOne).toDouble) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[Double]"
      override def shrinksForValue(valueToShrink: Double): Option[LazyListOrStream[RoseTree[Double]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive integers, excluding zero.
    */
  implicit val posIntGenerator: Generator[PosInt] =
    new Generator[PosInt] {

      case class NextRoseTree(value: PosInt, sizeParam: SizeParam, isValidFun: (PosInt, SizeParam) => Boolean) extends RoseTree[PosInt] {
        def shrinks: LazyListOrStream[RoseTree[PosInt]] = {
          def resLazyListOrStream(theValue: PosInt): LazyListOrStream[RoseTree[PosInt]] = {
            val half = theValue / 2
            if (half == 0) LazyListOrStream.empty
            else {
              val posIntHalf = PosInt.ensuringValid(half)
              if (isValidFun(posIntHalf, sizeParam))
                NextRoseTree(posIntHalf, sizeParam, isValidFun) #:: resLazyListOrStream(posIntHalf)
              else
                resLazyListOrStream(posIntHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosInt, sizeParam: SizeParam, isValidFun: (PosInt, SizeParam) => Boolean): RoseTree[PosInt] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosInt, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosInt], Randomizer) = {
        val (posInt, rnd2) = rnd.nextPosInt
        (NextRoseTree(posInt, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosInt]] = {
        case class CanonicalRoseTree(value: PosInt) extends RoseTree[PosInt] {
          def shrinks: LazyListOrStream[RoseTree[PosInt]] = {
            def resLazyListOrStream(theValue: PosInt): LazyListOrStream[RoseTree[PosInt]] = {
              if (theValue.value == 1) LazyListOrStream.empty
              else {
                val minusOne: PosInt = PosInt.ensuringValid(theValue.value - 1)
                if (minusOne.value == 1) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4).shrinks
      }
      override def toString = "Generator[PosInt]"
      override def shrinksForValue(valueToShrink: PosInt): Option[LazyListOrStream[RoseTree[PosInt]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive integers, including zero.
    */
  implicit val posZIntGenerator: Generator[PosZInt] =
    new Generator[PosZInt] {

      case class NextRoseTree(value: PosZInt, sizeParam: SizeParam, isValidFun: (PosZInt, SizeParam) => Boolean) extends RoseTree[PosZInt] {
        def shrinks: LazyListOrStream[RoseTree[PosZInt]] = {
          def resLazyListOrStream(theValue: PosZInt): LazyListOrStream[RoseTree[PosZInt]] = {
            if (theValue.value == 0) LazyListOrStream.empty
            else {
              val half: Int = theValue / 2
              val posZIntHalf = PosZInt.ensuringValid(half)
              if (isValidFun(posZIntHalf, sizeParam))
                NextRoseTree(posZIntHalf, sizeParam, isValidFun) #:: resLazyListOrStream(posZIntHalf)
              else
                resLazyListOrStream(posZIntHalf)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosZInt, sizeParam: SizeParam, isValidFun: (PosZInt, SizeParam) => Boolean): RoseTree[PosZInt] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosZInt, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosZInt], Randomizer) = {
        val (posZInt, rnd2) = rnd.nextPosZInt
        (NextRoseTree(posZInt, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosZInt]] = {
        case class CanonicalRoseTree(value: PosZInt) extends RoseTree[PosZInt] {
          def shrinks: LazyListOrStream[RoseTree[PosZInt]] = {
            def resLazyListOrStream(theValue: PosZInt): LazyListOrStream[RoseTree[PosZInt]] = {
              if (theValue.value == 0) LazyListOrStream.empty
              else {
                val minusOne: PosZInt = PosZInt.ensuringValid(theValue.value - 1)
                if (minusOne.value == 0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4).shrinks
      }
      override def toString = "Generator[PosZInt]"
      override def shrinksForValue(valueToShrink: PosZInt): Option[LazyListOrStream[RoseTree[PosZInt]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Longs, excluding zero.
    */
  implicit val posLongGenerator: Generator[PosLong] =
    new Generator[PosLong] {

      case class NextRoseTree(value: PosLong, sizeParam: SizeParam, isValidFun: (PosLong, SizeParam) => Boolean) extends RoseTree[PosLong] {
        def shrinks: LazyListOrStream[RoseTree[PosLong]] = {

          def resLazyListOrStream(theValue: PosLong): LazyListOrStream[RoseTree[PosLong]] = {
            val half = theValue / 2
            if (half == 0) LazyListOrStream.empty
            else {
              val posLongHalf = PosLong.ensuringValid(half)
              if (isValidFun(posLongHalf, sizeParam))
                NextRoseTree(posLongHalf, sizeParam, isValidFun) #:: resLazyListOrStream(posLongHalf)
              else
                resLazyListOrStream(posLongHalf)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosLong, sizeParam: SizeParam, isValidFun: (PosLong, SizeParam) => Boolean): RoseTree[PosLong] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosLong, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosLong], Randomizer) = {
        val (posLong, rnd2) = rnd.nextPosLong
        (NextRoseTree(posLong, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosLong]] = {
        case class CanonicalRoseTree(value: PosLong) extends RoseTree[PosLong] {
          def shrinks: LazyListOrStream[RoseTree[PosLong]] = {
            def resLazyListOrStream(theValue: PosLong): LazyListOrStream[RoseTree[PosLong]] = {
              if (theValue.value == 1) LazyListOrStream.empty
              else {
                val minusOne: PosLong = PosLong.ensuringValid(theValue.value - 1)
                if (minusOne.value == 1) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4L).shrinks
      }
      override def toString = "Generator[PosLong]"
      override def shrinksForValue(valueToShrink: PosLong): Option[LazyListOrStream[RoseTree[PosLong]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Longs, including zero.
    */
  implicit val posZLongGenerator: Generator[PosZLong] =
    new Generator[PosZLong] {
      
      case class NextRoseTree(value: PosZLong, sizeParam: SizeParam, isValidFun: (PosZLong, SizeParam) => Boolean) extends RoseTree[PosZLong] {
        def shrinks: LazyListOrStream[RoseTree[PosZLong]] = {
          def resLazyListOrStream(theValue: PosZLong): LazyListOrStream[RoseTree[PosZLong]] = {
            if (theValue.value == 0L) LazyListOrStream.empty
            else {
              val half: Long = theValue / 2
              val posZLongHalf = PosZLong.ensuringValid(half)
              if (isValidFun(posZLongHalf, sizeParam))
                NextRoseTree(posZLongHalf, sizeParam, isValidFun) #:: resLazyListOrStream(posZLongHalf)
              else
                resLazyListOrStream(posZLongHalf)  
            }
          }
          resLazyListOrStream(value)
        }
      }
      
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosZLong, sizeParam: SizeParam, isValidFun: (PosZLong, SizeParam) => Boolean): RoseTree[PosZLong] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosZLong, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosZLong], Randomizer) = {
        val (posZLong, rnd2) = rnd.nextPosZLong
        (NextRoseTree(posZLong, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosZLong]] = {
        case class CanonicalRoseTree(value: PosZLong) extends RoseTree[PosZLong] {
          def shrinks: LazyListOrStream[RoseTree[PosZLong]] = {
            def resLazyListOrStream(theValue: PosZLong): LazyListOrStream[RoseTree[PosZLong]] = {
              if (theValue.value == 0) LazyListOrStream.empty
              else {
                val minusOne: PosZLong = PosZLong.ensuringValid(theValue.value - 1)
                if (minusOne.value == 0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4L).shrinks
      }
      override def toString = "Generator[PosZLong]"
      override def shrinksForValue(valueToShrink: PosZLong): Option[LazyListOrStream[RoseTree[PosZLong]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Floats, excluding zero.
    */
  implicit val posFloatGenerator: Generator[PosFloat] =
    new Generator[PosFloat] {

      case class NextRoseTree(value: PosFloat, sizeParam: SizeParam, isValidFun: (PosFloat, SizeParam) => Boolean) extends RoseTree[PosFloat] {
        def shrinks: LazyListOrStream[RoseTree[PosFloat]] = {
          def resLazyListOrStream(theValue: PosFloat): LazyListOrStream[RoseTree[PosFloat]] = {
            val fv = theValue.value
            if (fv == 1.0f)
              LazyListOrStream.empty
            else if (fv < 1.0f) {
              if (isValidFun(PosFloat(1.0f), sizeParam))
                Rose(PosFloat(1.0f)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.PositiveInfinity || fv.isNaN)
                  Float.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosFloat.ensuringValid(n.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              val whole = PosFloat.ensuringValid(sqrt.floor)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosFloat, sizeParam: SizeParam, isValidFun: (PosFloat, SizeParam) => Boolean): RoseTree[PosFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosFloat], Randomizer) = {
        val (posFloat, rnd2) = rnd.nextPosFloat
        (NextRoseTree(posFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosFloat]] = {
        case class CanonicalRoseTree(value: PosFloat) extends RoseTree[PosFloat] {
          def shrinks: LazyListOrStream[RoseTree[PosFloat]] = {
            def resLazyListOrStream(theValue: PosFloat): LazyListOrStream[RoseTree[PosFloat]] = {
              if (theValue.value == 1.0f) LazyListOrStream.empty
              else {
                val minusOne: PosFloat = PosFloat.ensuringValid(theValue.value - 1.0f)
                if (minusOne.value == 1.0f) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[PosFloat]"
      override def shrinksForValue(valueToShrink: PosFloat): Option[LazyListOrStream[RoseTree[PosFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Floats, excluding zero and infinity.
    */
  implicit val posFiniteFloatGenerator: Generator[PosFiniteFloat] =
    new Generator[PosFiniteFloat] {

      case class NextRoseTree(value: PosFiniteFloat, sizeParam: SizeParam, isValidFun: (PosFiniteFloat, SizeParam) => Boolean) extends RoseTree[PosFiniteFloat] {
        def shrinks: LazyListOrStream[RoseTree[PosFiniteFloat]] = {
          def resLazyListOrStream(theValue: PosFiniteFloat): LazyListOrStream[RoseTree[PosFiniteFloat]] = {
            val fv = theValue.value
            if (fv == 1.0f) LazyListOrStream.empty
            else if (fv < 1.0f) { 
              if (isValidFun(PosFiniteFloat(1.0f), sizeParam))
                Rose(PosFiniteFloat(1.0f)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosFiniteFloat.ensuringValid(fv.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              val whole = PosFiniteFloat.ensuringValid(sqrt.floor)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosFiniteFloat, sizeParam: SizeParam, isValidFun: (PosFiniteFloat, SizeParam) => Boolean): RoseTree[PosFiniteFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosFiniteFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosFiniteFloat], Randomizer) = {
        val (posFiniteFloat, rnd2) = rnd.nextPosFiniteFloat
        (NextRoseTree(posFiniteFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosFiniteFloat]] = {
        case class CanonicalRoseTree(value: PosFiniteFloat) extends RoseTree[PosFiniteFloat] {
          def shrinks: LazyListOrStream[RoseTree[PosFiniteFloat]] = {
            def resLazyListOrStream(theValue: PosFiniteFloat): LazyListOrStream[RoseTree[PosFiniteFloat]] = {
              if (theValue.value == 1.0f) LazyListOrStream.empty
              else {
                val minusOne: PosFiniteFloat = PosFiniteFloat.ensuringValid(theValue.value - 1.0f)
                if (minusOne.value == 1.0f) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[PosFiniteFloat]"
      override def shrinksForValue(valueToShrink: PosFiniteFloat): Option[LazyListOrStream[RoseTree[PosFiniteFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Floats, excluding infinity.
    */
  implicit val finiteFloatGenerator: Generator[FiniteFloat] =
    new Generator[FiniteFloat] {

      case class NextRoseTree(value: FiniteFloat, sizeParam: SizeParam, isValidFun: (FiniteFloat, SizeParam) => Boolean) extends RoseTree[FiniteFloat] {
        def shrinks: LazyListOrStream[RoseTree[FiniteFloat]] = {
          def resLazyListOrStream(theValue: FiniteFloat): LazyListOrStream[RoseTree[FiniteFloat]] = {
            val fv = theValue.value
            if (fv == 0.0f) LazyListOrStream.empty
            else if (fv <= 1.0f && fv >= -1.0f) {
              if (isValidFun(FiniteFloat(0.0f), sizeParam))
                Rose(FiniteFloat(0.0f)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (fv > 0.0f) (fv.floor, (-fv).ceil) else (fv.ceil, (-fv).floor)
              LazyListOrStream(FiniteFloat.ensuringValid(nearestNeg), FiniteFloat.ensuringValid(nearest))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(FiniteFloat.ensuringValid(nearest))
            }
            else {
              val sqrt: Float = math.sqrt(fv.abs.toDouble).toFloat
              if (sqrt < 1.0f) {
                if (isValidFun(FiniteFloat(0.0f), sizeParam))
                  Rose(FiniteFloat(0.0f)) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty  
              }
              else {
                val whole: Float = sqrt.floor
                val negWhole: Float = math.rint((-whole).toDouble).toFloat
                val (first, second) = if (fv > 0.0f) (negWhole, whole) else (whole, negWhole)
                LazyListOrStream(FiniteFloat.ensuringValid(first), FiniteFloat.ensuringValid(second))
                  .filter(isValidFun(_, sizeParam))
                  .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(FiniteFloat.ensuringValid(first))
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[FiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(finiteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: FiniteFloat, sizeParam: SizeParam, isValidFun: (FiniteFloat, SizeParam) => Boolean): RoseTree[FiniteFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (FiniteFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[FiniteFloat], Randomizer) = {
        val (finiteFloat, rnd2) = rnd.nextFiniteFloat
        (NextRoseTree(finiteFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[FiniteFloat]] = {
        case class CanonicalRoseTree(value: FiniteFloat) extends RoseTree[FiniteFloat] {
          def shrinks: LazyListOrStream[RoseTree[FiniteFloat]] = {
            def resLazyListOrStream(theValue: FiniteFloat): LazyListOrStream[RoseTree[FiniteFloat]] = {
              if (theValue.value == 0.0f) LazyListOrStream.empty
              else {
                val minusOne: FiniteFloat = FiniteFloat.ensuringValid(theValue.value - 1.0f)
                if (minusOne.value == 0.0f) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(-minusOne) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[FiniteFloat]"
      override def shrinksForValue(valueToShrink: FiniteFloat): Option[LazyListOrStream[RoseTree[FiniteFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Doubles, excluding infinity.
    */
  implicit val finiteDoubleGenerator: Generator[FiniteDouble] =
    new Generator[FiniteDouble] {

      case class NextRoseTree(value: FiniteDouble, sizeParam: SizeParam, isValidFun: (FiniteDouble, SizeParam) => Boolean) extends RoseTree[FiniteDouble] {
        def shrinks: LazyListOrStream[RoseTree[FiniteDouble]] = {
          def resLazyListOrStream(theValue: FiniteDouble): LazyListOrStream[RoseTree[FiniteDouble]] = {
            val dv: Double = theValue.value
            if (dv == 0.0) LazyListOrStream.empty
            else if (dv <= 1.0 && dv >= -1.0) {
              if (isValidFun(FiniteDouble(0.0), sizeParam))
                Rose(FiniteDouble(0.0)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!dv.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (dv > 0.0) (dv.floor, (-dv).ceil) else (dv.ceil, (-dv).floor)
              LazyListOrStream(FiniteDouble.ensuringValid(nearestNeg), FiniteDouble.ensuringValid(nearest))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(FiniteDouble.ensuringValid(nearest))
            }
            else {
              val sqrt: Double = math.sqrt(dv.abs)
              if (sqrt < 1.0) {
                if (isValidFun(FiniteDouble(0.0), sizeParam))
                  Rose(FiniteDouble(0.0)) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty  
              }
              else {
                val whole: Double = sqrt.floor
                val negWhole: Double = math.rint((-whole).toDouble)
                val (first, second) = if (dv > 0.0) (negWhole, whole) else (whole, negWhole)
                LazyListOrStream(FiniteDouble.ensuringValid(first), FiniteDouble.ensuringValid(second))
                  .filter(isValidFun(_, sizeParam))
                  .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(FiniteDouble.ensuringValid(first))
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[FiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(finiteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: FiniteDouble, sizeParam: SizeParam, isValidFun: (FiniteDouble, SizeParam) => Boolean): RoseTree[FiniteDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (FiniteDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[FiniteDouble], Randomizer) = {
        val (finiteDouble, rnd2) = rnd.nextFiniteDouble
        (NextRoseTree(finiteDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[FiniteDouble]] = {
        case class CanonicalRoseTree(value: FiniteDouble) extends RoseTree[FiniteDouble] {
          def shrinks: LazyListOrStream[RoseTree[FiniteDouble]] = {
            def resLazyListOrStream(theValue: FiniteDouble): LazyListOrStream[RoseTree[FiniteDouble]] = {
              if (theValue.value == 0.0) LazyListOrStream.empty
              else {
                val minusOne: FiniteDouble = FiniteDouble.ensuringValid(theValue.value - 1.0)
                if (minusOne.value == 0.0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(-minusOne) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[FiniteDouble]"
      override def shrinksForValue(valueToShrink: FiniteDouble): Option[LazyListOrStream[RoseTree[FiniteDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Floats, including zero and infinity.
    */
  implicit val posZFloatGenerator: Generator[PosZFloat] =
    new Generator[PosZFloat] {

      case class NextRoseTree(value: PosZFloat, sizeParam: SizeParam, isValidFun: (PosZFloat, SizeParam) => Boolean) extends RoseTree[PosZFloat] {
        def shrinks: LazyListOrStream[RoseTree[PosZFloat]] = {
          def resLazyListOrStream(theValue: PosZFloat): LazyListOrStream[RoseTree[PosZFloat]] = {
            val fv: Float = theValue.value
            if (fv == 0.0f) LazyListOrStream.empty
            else if (fv <= 1.0f) { 
              if (isValidFun(PosZFloat(0.0f), sizeParam))
                Rose(PosZFloat(0.0f)) #:: LazyListOrStream.empty
              else 
                LazyListOrStream.empty
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.PositiveInfinity || fv.isNaN)
                  Float.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosZFloat.ensuringValid(n.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              if (sqrt < 1.0f) {
                if (isValidFun(PosZFloat(0.0f), sizeParam))
                  Rose(PosZFloat(0.0f)) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty  
              }
              else {
                val whole = PosZFloat.ensuringValid(sqrt.floor)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)  
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosZFloat, sizeParam: SizeParam, isValidFun: (PosZFloat, SizeParam) => Boolean): RoseTree[PosZFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosZFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosZFloat], Randomizer) = {
        val (posZFloat, rnd2) = rnd.nextPosZFloat
        (NextRoseTree(posZFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosZFloat]] = {
        case class CanonicalRoseTree(value: PosZFloat) extends RoseTree[PosZFloat] {
          def shrinks: LazyListOrStream[RoseTree[PosZFloat]] = {
            def resLazyListOrStream(theValue: PosZFloat): LazyListOrStream[RoseTree[PosZFloat]] = {
              if (theValue.value == 0.0f) LazyListOrStream.empty
              else {
                val minusOne: PosZFloat = PosZFloat.ensuringValid(theValue.value - 1.0f)
                if (minusOne.value == 0.0f) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[PosZFloat]"
      override def shrinksForValue(valueToShrink: PosZFloat): Option[LazyListOrStream[RoseTree[PosZFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Floats, including zero but excluding infinity.
    */
  implicit val posZFiniteFloatGenerator: Generator[PosZFiniteFloat] =
    new Generator[PosZFiniteFloat] {

      case class NextRoseTree(value: PosZFiniteFloat, sizeParam: SizeParam, isValidFun: (PosZFiniteFloat, SizeParam) => Boolean) extends RoseTree[PosZFiniteFloat] {
        def shrinks: LazyListOrStream[RoseTree[PosZFiniteFloat]] = {
          def resLazyListOrStream(theValue: PosZFiniteFloat): LazyListOrStream[RoseTree[PosZFiniteFloat]] = {
            val fv = theValue.value
            if (fv == 0.0f) LazyListOrStream.empty
            else if (fv <= 1.0f) {
              if (isValidFun(PosZFiniteFloat(0.0f), sizeParam))
                Rose(PosZFiniteFloat(0.0f)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosZFiniteFloat.ensuringValid(fv.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = math.sqrt(fv.toDouble).toFloat
              if (sqrt < 1.0f) Rose(PosZFiniteFloat(0.0f)) #:: LazyListOrStream.empty
              else {
                val whole = PosZFiniteFloat.ensuringValid(sqrt.floor)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)  
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosZFiniteFloat, sizeParam: SizeParam, isValidFun: (PosZFiniteFloat, SizeParam) => Boolean): RoseTree[PosZFiniteFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosZFiniteFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosZFiniteFloat], Randomizer) = {
        val (posZFiniteFloat, rnd2) = rnd.nextPosZFiniteFloat
        (NextRoseTree(posZFiniteFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosZFiniteFloat]] = {
        case class CanonicalRoseTree(value: PosZFiniteFloat) extends RoseTree[PosZFiniteFloat] {
          def shrinks: LazyListOrStream[RoseTree[PosZFiniteFloat]] = {
            def resLazyListOrStream(theValue: PosZFiniteFloat): LazyListOrStream[RoseTree[PosZFiniteFloat]] = {
              if (theValue.value == 0.0f) LazyListOrStream.empty
              else {
                val minusOne: PosZFiniteFloat = PosZFiniteFloat.ensuringValid(theValue.value - 1.0f)
                if (minusOne.value == 0.0f) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[PosZFiniteFloat]"
      override def shrinksForValue(valueToShrink: PosZFiniteFloat): Option[LazyListOrStream[RoseTree[PosZFiniteFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Doubles, excluding zero but including infinity.
    */
  implicit val posDoubleGenerator: Generator[PosDouble] =
    new Generator[PosDouble] {

      case class NextRoseTree(value: PosDouble, sizeParam: SizeParam, isValidFun: (PosDouble, SizeParam) => Boolean) extends RoseTree[PosDouble] {
        def shrinks: LazyListOrStream[RoseTree[PosDouble]] = {
          def resLazyListOrStream(theValue: PosDouble): LazyListOrStream[RoseTree[PosDouble]] = {
            val fv = theValue.value
            if (fv == 1.0) LazyListOrStream.empty
            else if (fv < 1.0) { 
              if (isValidFun(PosDouble(1.0), sizeParam))
                Rose(PosDouble(1.0)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.PositiveInfinity || fv.isNaN)
                  Double.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosDouble.ensuringValid(n.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              val whole = PosDouble.ensuringValid(sqrt.floor)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosDouble, sizeParam: SizeParam, isValidFun: (PosDouble, SizeParam) => Boolean): RoseTree[PosDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosDouble], Randomizer) = {
        val (posDouble, rnd2) = rnd.nextPosDouble
        (NextRoseTree(posDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosDouble]] = {
        case class CanonicalRoseTree(value: PosDouble) extends RoseTree[PosDouble] {
          def shrinks: LazyListOrStream[RoseTree[PosDouble]] = {
            def resLazyListOrStream(theValue: PosDouble): LazyListOrStream[RoseTree[PosDouble]] = {
              if (theValue.value == 1.0) LazyListOrStream.empty
              else {
                val minusOne: PosDouble = PosDouble.ensuringValid(theValue.value - 1.0)
                if (minusOne.value == 1.0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[PosDouble]"
      override def shrinksForValue(valueToShrink: PosDouble): Option[LazyListOrStream[RoseTree[PosDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Doubles, excluding zero and infinity.
    */
  implicit val posFiniteDoubleGenerator: Generator[PosFiniteDouble] =
    new Generator[PosFiniteDouble] {

      case class NextRoseTree(value: PosFiniteDouble, sizeParam: SizeParam, isValidFun: (PosFiniteDouble, SizeParam) => Boolean) extends RoseTree[PosFiniteDouble] {
        def shrinks: LazyListOrStream[RoseTree[PosFiniteDouble]] = {
          def resLazyListOrStream(theValue: PosFiniteDouble): LazyListOrStream[RoseTree[PosFiniteDouble]] = {
            val fv = theValue.value
            if (fv == 1.0) LazyListOrStream.empty
            else if (fv < 1.0) { 
              if (isValidFun(PosFiniteDouble(1.0), sizeParam))
                Rose(PosFiniteDouble(1.0)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosFiniteDouble.ensuringValid(fv.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              val whole = PosFiniteDouble.ensuringValid(sqrt.floor)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosFiniteDouble, sizeParam: SizeParam, isValidFun: (PosFiniteDouble, SizeParam) => Boolean): RoseTree[PosFiniteDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosFiniteDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosFiniteDouble], Randomizer) = {
        val (posFiniteDouble, rnd2) = rnd.nextPosFiniteDouble
        (NextRoseTree(posFiniteDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosFiniteDouble]] = {
        case class CanonicalRoseTree(value: PosFiniteDouble) extends RoseTree[PosFiniteDouble] {
          def shrinks: LazyListOrStream[RoseTree[PosFiniteDouble]] = {
            def resLazyListOrStream(theValue: PosFiniteDouble): LazyListOrStream[RoseTree[PosFiniteDouble]] = {
              if (theValue.value == 1.0) LazyListOrStream.empty
              else {
                val minusOne: PosFiniteDouble = PosFiniteDouble.ensuringValid(theValue.value - 1.0)
                if (minusOne.value == 1.0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[PosFiniteDouble]"
      override def shrinksForValue(valueToShrink: PosFiniteDouble): Option[LazyListOrStream[RoseTree[PosFiniteDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Doubles, including zero and infinity.
    */
  implicit val posZDoubleGenerator: Generator[PosZDouble] =
    new Generator[PosZDouble] {

      case class NextRoseTree(value: PosZDouble, sizeParam: SizeParam, isValidFun: (PosZDouble, SizeParam) => Boolean) extends RoseTree[PosZDouble] {
        def shrinks: LazyListOrStream[RoseTree[PosZDouble]] = {
          def resLazyListOrStream(theValue: PosZDouble): LazyListOrStream[RoseTree[PosZDouble]] = {
            val fv = theValue.value
            if (fv == 0.0) LazyListOrStream.empty
            else if (fv <= 1.0) {
              if (isValidFun(PosZDouble(0.0), sizeParam))
                Rose(PosZDouble(0.0)) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.PositiveInfinity || fv.isNaN)
                  Double.MaxValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = PosZDouble.ensuringValid(n.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              if (sqrt < 1.0) { 
                if (isValidFun(PosZDouble(0.0), sizeParam))
                  Rose(PosZDouble(0.0)) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty  
              }
              else {
                val whole = PosZDouble.ensuringValid(sqrt.floor)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)  
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosZDouble, sizeParam: SizeParam, isValidFun: (PosZDouble, SizeParam) => Boolean): RoseTree[PosZDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosZDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosZDouble], Randomizer) = {
        val (posZDouble, rnd2) = rnd.nextPosZDouble
        (NextRoseTree(posZDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosZDouble]] = {
        case class CanonicalRoseTree(value: PosZDouble) extends RoseTree[PosZDouble] {
          def shrinks: LazyListOrStream[RoseTree[PosZDouble]] = {
            def resLazyListOrStream(theValue: PosZDouble): LazyListOrStream[RoseTree[PosZDouble]] = {
              if (theValue.value == 0.0) LazyListOrStream.empty
              else {
                val minusOne: PosZDouble = PosZDouble.ensuringValid(theValue.value - 1.0)
                if (minusOne.value == 0.0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[PosZDouble]"
      override def shrinksForValue(valueToShrink: PosZDouble): Option[LazyListOrStream[RoseTree[PosZDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces positive Doubles, including zero but excluding infinity.
    */
  implicit val posZFiniteDoubleGenerator: Generator[PosZFiniteDouble] =
    new Generator[PosZFiniteDouble] {

      case class NextRoseTree(value: PosZFiniteDouble, sizeParam: SizeParam, isValidFun: (PosZFiniteDouble, SizeParam) => Boolean) extends RoseTree[PosZFiniteDouble] {
        def shrinks: LazyListOrStream[RoseTree[PosZFiniteDouble]] = {
          def resLazyListOrStream(theValue: PosZFiniteDouble): LazyListOrStream[RoseTree[PosZFiniteDouble]] = {
            val fv = theValue.value
            if (fv == 0.0) LazyListOrStream.empty
            else if (fv <= 1.0) {
              if (isValidFun(PosZFiniteDouble(0.0), sizeParam))
                Rose(PosZFiniteDouble(0.0)) #:: LazyListOrStream.empty
              else 
                LazyListOrStream.empty
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = PosZFiniteDouble.ensuringValid(fv.floor)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = math.sqrt(fv)
              if (sqrt < 1.0) { 
                if (isValidFun(PosZFiniteDouble(0.0), sizeParam))
                  Rose(PosZFiniteDouble(0.0)) #:: LazyListOrStream.empty
                else
                  LazyListOrStream.empty  
              }
              else {
                val whole = PosZFiniteDouble.ensuringValid(sqrt.floor)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[PosZFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(posZFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: PosZFiniteDouble, sizeParam: SizeParam, isValidFun: (PosZFiniteDouble, SizeParam) => Boolean): RoseTree[PosZFiniteDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (PosZFiniteDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[PosZFiniteDouble], Randomizer) = {
        val (posZFiniteDouble, rnd2) = rnd.nextPosZFiniteDouble
        (NextRoseTree(posZFiniteDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[PosZFiniteDouble]] = {
        case class CanonicalRoseTree(value: PosZFiniteDouble) extends RoseTree[PosZFiniteDouble] {
          def shrinks: LazyListOrStream[RoseTree[PosZFiniteDouble]] = {
            def resLazyListOrStream(theValue: PosZFiniteDouble): LazyListOrStream[RoseTree[PosZFiniteDouble]] = {
              if (theValue.value == 0.0) LazyListOrStream.empty
              else {
                val minusOne: PosZFiniteDouble = PosZFiniteDouble.ensuringValid(theValue.value - 1.0)
                if (minusOne.value == 0.0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[PosZFiniteDouble]"
      override def shrinksForValue(valueToShrink: PosZFiniteDouble): Option[LazyListOrStream[RoseTree[PosZFiniteDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Doubles, excluding zero but including infinity.
    */
  implicit val nonZeroDoubleGenerator: Generator[NonZeroDouble] =
    new Generator[NonZeroDouble] {

      case class NextRoseTree(value: NonZeroDouble, sizeParam: SizeParam, isValidFun: (NonZeroDouble, SizeParam) => Boolean) extends RoseTree[NonZeroDouble] {
        def shrinks: LazyListOrStream[RoseTree[NonZeroDouble]] = {
          def resLazyListOrStream(theValue: NonZeroDouble): LazyListOrStream[RoseTree[NonZeroDouble]] = {
            val d = theValue.value
            if (d <= 1.0 && d >= -1.0)
              LazyListOrStream.empty
            else if (!d.isWhole) {
              val n =
                if (d == Double.PositiveInfinity || d.isNaN)
                  Double.MaxValue
                else if (d == Double.NegativeInfinity)
                  Double.MinValue
                else d
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (n > 0.0) (n.floor, (-n).ceil) else (n.ceil, (-n).floor)
              LazyListOrStream(NonZeroDouble.ensuringValid(nearestNeg), NonZeroDouble.ensuringValid(nearest))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(NonZeroDouble.ensuringValid(nearest))
            }
            else {
              val sqrt: Double = math.sqrt(d.abs)
              if (sqrt < 1.0) LazyListOrStream.empty
              else {
                val whole: NonZeroDouble = NonZeroDouble.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroDouble = -whole  //math.rint(-whole)
                val (first, second) = if (d > 0.0) (negWhole, whole) else (whole, negWhole)
                LazyListOrStream(first, second)
                  .filter(isValidFun(_, sizeParam))
                  .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(first)
              }
            }
          }

          val d: Double = value.value
          if (d <= 1.0 && d >= -1.0) {
            // For now, if a non-zero floating point value is between -1.0 and 1.0 exclusive, just try -1.0 and 1.0.
            // Our attitude is that whole numbers are simpler, so more shrunken, than non-whole numbers. Since the failing value
            // non-zero, there isn't any number smaller that's whole, so for now we'll hop up to -1.0 and 1.0.
            LazyListOrStream(NonZeroDouble.ensuringValid(-1.0), NonZeroDouble.ensuringValid(1.0))
              .filter(isValidFun(_, sizeParam))
              .map(Rose(_))  #::: LazyListOrStream.empty
          }
          else
            resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NonZeroDouble, sizeParam: SizeParam, isValidFun: (NonZeroDouble, SizeParam) => Boolean): RoseTree[NonZeroDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NonZeroDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NonZeroDouble], Randomizer) = {
        val (nonZeroDouble, rnd2) = rnd.nextNonZeroDouble
        (NextRoseTree(nonZeroDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NonZeroDouble]] = {
        case class CanonicalRoseTree(value: NonZeroDouble) extends RoseTree[NonZeroDouble] {
          def shrinks: LazyListOrStream[RoseTree[NonZeroDouble]] = {
            def resLazyListOrStream(theValue: NonZeroDouble): LazyListOrStream[RoseTree[NonZeroDouble]] = {
              if (theValue.value == 1.0) LazyListOrStream.empty
              else {
                val minusOne: NonZeroDouble = NonZeroDouble.ensuringValid(theValue.value - 1.0)
                if (minusOne.value == 1.0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[NonZeroDouble]"
      override def shrinksForValue(valueToShrink: NonZeroDouble): Option[LazyListOrStream[RoseTree[NonZeroDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Doubles, excluding zero and infinity.
    */
  implicit val nonZeroFiniteDoubleGenerator: Generator[NonZeroFiniteDouble] =
    new Generator[NonZeroFiniteDouble] {

      case class NextRoseTree(value: NonZeroFiniteDouble, sizeParam: SizeParam, isValidFun: (NonZeroFiniteDouble, SizeParam) => Boolean) extends RoseTree[NonZeroFiniteDouble] {
        def shrinks: LazyListOrStream[RoseTree[NonZeroFiniteDouble]] = {
          def resLazyListOrStream(theValue: NonZeroFiniteDouble): LazyListOrStream[RoseTree[NonZeroFiniteDouble]] = {
            val d = theValue.value
            if (d <= 1.0 && d >= -1.0)
              LazyListOrStream.empty[RoseTree[NonZeroFiniteDouble]]
            else if (!d.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (d > 0.0) (d.floor, (-d).ceil) else (d.ceil, (-d).floor)
              LazyListOrStream(NonZeroFiniteDouble.ensuringValid(nearestNeg), NonZeroFiniteDouble.ensuringValid(nearest))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(NonZeroFiniteDouble.ensuringValid(nearest))
            }
            else {
              val sqrt: Double = math.sqrt(d.abs)
              if (sqrt < 1.0) LazyListOrStream.empty[RoseTree[NonZeroFiniteDouble]]
              else {
                val whole: NonZeroFiniteDouble = NonZeroFiniteDouble.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroFiniteDouble = -whole  //math.rint(-whole)
                val (first, second) = if (d > 0.0) (negWhole, whole) else (whole, negWhole)
                LazyListOrStream(first, second)
                  .filter(isValidFun(_, sizeParam))
                  .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(first)
              }
            }
          }
          val d: Double = value.value
          if (d <= 1.0 && d >= -1.0) {
            // For now, if a non-zero floating point value is between -1.0 and 1.0 exclusive, just try -1.0 and 1.0.
            // Our attitude is that whole numbers are simpler, so more shrunken, than non-whole numbers. Since the failing value
            // non-zero, there isn't any number smaller that's whole, so for now we'll hop up to -1.0 and 1.0. 
            LazyListOrStream(NonZeroFiniteDouble.ensuringValid(-1.0), NonZeroFiniteDouble.ensuringValid(1.0))
              .filter(isValidFun(_, sizeParam))
              .map(NextRoseTree(_, sizeParam, isValidFun)) #::: LazyListOrStream.empty
          }
          else
            resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NonZeroFiniteDouble, sizeParam: SizeParam, isValidFun: (NonZeroFiniteDouble, SizeParam) => Boolean): RoseTree[NonZeroFiniteDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NonZeroFiniteDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NonZeroFiniteDouble], Randomizer) = {
        val (nonZeroFiniteDouble, rnd2) = rnd.nextNonZeroFiniteDouble
        (NextRoseTree(nonZeroFiniteDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NonZeroFiniteDouble]] = {
        case class CanonicalRoseTree(value: NonZeroFiniteDouble) extends RoseTree[NonZeroFiniteDouble] {
          def shrinks: LazyListOrStream[RoseTree[NonZeroFiniteDouble]] = {
            def resLazyListOrStream(theValue: NonZeroFiniteDouble): LazyListOrStream[RoseTree[NonZeroFiniteDouble]] = {
              if (theValue.value == 1.0) LazyListOrStream.empty
              else {
                val minusOne: NonZeroFiniteDouble = NonZeroFiniteDouble.ensuringValid(theValue.value - 1.0)
                if (minusOne.value == 1.0) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(-minusOne) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0).shrinks
      }
      override def toString = "Generator[NonZeroFiniteDouble]"
      override def shrinksForValue(valueToShrink: NonZeroFiniteDouble): Option[LazyListOrStream[RoseTree[NonZeroFiniteDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Floats, excluding zero but including infinity.
    */
  implicit val nonZeroFloatGenerator: Generator[NonZeroFloat] =
    new Generator[NonZeroFloat] {

      case class NextRoseTree(value: NonZeroFloat, sizeParam: SizeParam, isValidFun: (NonZeroFloat, SizeParam) => Boolean) extends RoseTree[NonZeroFloat] {
        def shrinks: LazyListOrStream[RoseTree[NonZeroFloat]] = {
          def resLazyListOrStream(theValue: NonZeroFloat): LazyListOrStream[RoseTree[NonZeroFloat]] = {
            val d = theValue.value
            if (d <= 1.0f && d >= -1.0f)
              LazyListOrStream.empty[RoseTree[NonZeroFloat]]
            else if (!d.isWhole) {
              val n =
                if (d == Float.PositiveInfinity || d.isNaN)
                  Float.MaxValue
                else if (d == Float.NegativeInfinity)
                  Float.MinValue
                else d
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (n > 0.0f) (n.floor, (-n).ceil) else (n.ceil, (-n).floor)
              LazyListOrStream(NonZeroFloat.ensuringValid(nearestNeg), NonZeroFloat.ensuringValid(nearest))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(NonZeroFloat.ensuringValid(nearest))
            }
            else {
              val sqrt: Float = math.sqrt(d.abs.toDouble).toFloat
              if (sqrt < 1.0f) LazyListOrStream.empty[RoseTree[NonZeroFloat]]
              else {
                val whole: NonZeroFloat = NonZeroFloat.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroFloat = -whole  //math.rint(-whole)
                LazyListOrStream(negWhole, whole)
                  .filter(isValidFun(_, sizeParam))
                  .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(whole)
              }
            }
          }
          val d: Float = value.value
          if (d <= 1.0 && d >= -1.0) {
            // For now, if a non-zero floating point value is between -1.0 and 1.0 exclusive, just try -1.0 and 1.0.
            // Our attitude is that whole numbers are simpler, so more shrunken, than non-whole numbers. Since the failing value
            // non-zero, there isn't any number smaller that's whole, so for now we'll hop up to -1.0 and 1.0. 
            LazyListOrStream(NonZeroFloat.ensuringValid(-1.0f), NonZeroFloat.ensuringValid(1.0f))
              .filter(isValidFun(_, sizeParam))
              .map(Rose(_)) #::: LazyListOrStream.empty
          }
          else
            resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NonZeroFloat, sizeParam: SizeParam, isValidFun: (NonZeroFloat, SizeParam) => Boolean): RoseTree[NonZeroFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NonZeroFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NonZeroFloat], Randomizer) = {
        val (nonZeroFloat, rnd2) = rnd.nextNonZeroFloat
        (NextRoseTree(nonZeroFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NonZeroFloat]] = {
        case class CanonicalRoseTree(value: NonZeroFloat) extends RoseTree[NonZeroFloat] {
          def shrinks: LazyListOrStream[RoseTree[NonZeroFloat]] = {
            def resLazyListOrStream(theValue: NonZeroFloat): LazyListOrStream[RoseTree[NonZeroFloat]] = {
              if (theValue.value == 1.0f) LazyListOrStream.empty
              else {
                val minusOne: NonZeroFloat = NonZeroFloat.ensuringValid(theValue - 1.0f)
                if (minusOne.value == 1.0f || minusOne.value == -1.0f) Rose(-minusOne) #:: Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(-minusOne) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[NonZeroFloat]"
      override def shrinksForValue(valueToShrink: NonZeroFloat): Option[LazyListOrStream[RoseTree[NonZeroFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Floats, excluding zero and infinity.
    */
  implicit val nonZeroFiniteFloatGenerator: Generator[NonZeroFiniteFloat] =
    new Generator[NonZeroFiniteFloat] {

      case class NextRoseTree(value: NonZeroFiniteFloat, sizeParam: SizeParam, isValidFun: (NonZeroFiniteFloat, SizeParam) => Boolean) extends RoseTree[NonZeroFiniteFloat] {
        def shrinks: LazyListOrStream[RoseTree[NonZeroFiniteFloat]] = {
          def resLazyListOrStream(theValue: NonZeroFiniteFloat): LazyListOrStream[RoseTree[NonZeroFiniteFloat]] = {
            val d = theValue.value
            if (d <= 1.0f && d >= -1.0f)
              LazyListOrStream.empty[RoseTree[NonZeroFiniteFloat]]
            else if (!d.isWhole) {
              // Nearest whole numbers closer to zero
              val (nearest, nearestNeg) = if (d > 0.0f) (d.floor, (-d).ceil) else (d.ceil, (-d).floor)
              LazyListOrStream(NonZeroFiniteFloat.ensuringValid(nearestNeg), NonZeroFiniteFloat.ensuringValid(nearest))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(NonZeroFiniteFloat.ensuringValid(nearest))
            }
            else {
              val sqrt: Float = math.sqrt(d.abs.toDouble).toFloat
              if (sqrt < 1.0f) LazyListOrStream.empty[RoseTree[NonZeroFiniteFloat]]
              else {
                val whole: NonZeroFiniteFloat = NonZeroFiniteFloat.ensuringValid(sqrt.floor)
                // Bill: math.rint behave similarly on js, is it ok we just do -whole instead?  Seems to pass our tests.
                val negWhole: NonZeroFiniteFloat = -whole  //math.rint(-whole)
                LazyListOrStream(negWhole, whole)
                  .filter(isValidFun(_, sizeParam))
                  .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(whole)
              }
            }
          }
          val d: Float = value.value
          if (d <= 1.0 && d >= -1.0) {
            // For now, if a non-zero floating point value is between -1.0 and 1.0 exclusive, just try -1.0 and 1.0.
            // Our attitude is that whole numbers are simpler, so more shrunken, than non-whole numbers. Since the failing value
            // non-zero, there isn't any number smaller that's whole, so for now we'll hop up to -1.0 and 1.0.
            LazyListOrStream(NonZeroFiniteFloat.ensuringValid(-1.0f), NonZeroFiniteFloat.ensuringValid(1.0f))
              .filter(isValidFun(_, sizeParam))
              .map(Rose(_)) #::: LazyListOrStream.empty
          }
          else
            resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NonZeroFiniteFloat, sizeParam: SizeParam, isValidFun: (NonZeroFiniteFloat, SizeParam) => Boolean): RoseTree[NonZeroFiniteFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NonZeroFiniteFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NonZeroFiniteFloat], Randomizer) = {
        val (nonZeroFiniteFloat, rnd2) = rnd.nextNonZeroFiniteFloat
        (NextRoseTree(nonZeroFiniteFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NonZeroFiniteFloat]] = {
        case class CanonicalRoseTree(value: NonZeroFiniteFloat) extends RoseTree[NonZeroFiniteFloat] {
          def shrinks: LazyListOrStream[RoseTree[NonZeroFiniteFloat]] = {
            def resLazyListOrStream(theValue: NonZeroFiniteFloat): LazyListOrStream[RoseTree[NonZeroFiniteFloat]] = {
              if (theValue.value == 1.0f) LazyListOrStream.empty
              else {
                val minusOne: NonZeroFiniteFloat = NonZeroFiniteFloat.ensuringValid(theValue.value - 1.0f)
                if (minusOne.value == 1.0f) Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(-minusOne) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4.0f).shrinks
      }
      override def toString = "Generator[NonZeroFiniteFloat]"
      override def shrinksForValue(valueToShrink: NonZeroFiniteFloat): Option[LazyListOrStream[RoseTree[NonZeroFiniteFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces integers, excluding zero.
    */
  implicit val nonZeroIntGenerator: Generator[NonZeroInt] =
    new Generator[NonZeroInt] {

      case class NextRoseTree(value: NonZeroInt, sizeParam: SizeParam, isValidFun: (NonZeroInt, SizeParam) => Boolean) extends RoseTree[NonZeroInt] {
        def shrinks: LazyListOrStream[RoseTree[NonZeroInt]] = {
          def resLazyListOrStream(theValue: NonZeroInt): LazyListOrStream[RoseTree[NonZeroInt]] = {
            val i = theValue.value
            val half: Int = i / 2 // i cannot be zero, because initially it is the underlying Int value of a NonZeroInt (in types
            if (half == 0) LazyListOrStream.empty[RoseTree[NonZeroInt]]    // we trust), then if half results in zero, we return empty list. I.e., no more shrinks available.
            else {
              LazyListOrStream(NonZeroInt.ensuringValid(-half), NonZeroInt.ensuringValid(half))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(NonZeroInt.ensuringValid(half))
            }
          }
          resLazyListOrStream(value)
        }
      } // TODO Confirm OK without Roses. I.e., will the last one have an empty shrinks method?

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NonZeroInt, sizeParam: SizeParam, isValidFun: (NonZeroInt, SizeParam) => Boolean): RoseTree[NonZeroInt] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NonZeroInt, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NonZeroInt], Randomizer) = {
        val (nonZeroInt, rnd2) = rnd.nextNonZeroInt
        (NextRoseTree(nonZeroInt, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NonZeroInt]] = {
        case class CanonicalRoseTree(value: NonZeroInt) extends RoseTree[NonZeroInt] {
          def shrinks: LazyListOrStream[RoseTree[NonZeroInt]] = {
            def resLazyListOrStream(theValue: NonZeroInt): LazyListOrStream[RoseTree[NonZeroInt]] = {
              if (theValue.value == 1) LazyListOrStream.empty
              else {
                val minusOne = NonZeroInt.ensuringValid(theValue.value - 1)
                if (minusOne.value == 1) Rose(-minusOne) #:: Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(-minusOne) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4).shrinks
      }
      override def toString = "Generator[NonZeroInt]"
      override def shrinksForValue(valueToShrink: NonZeroInt): Option[LazyListOrStream[RoseTree[NonZeroInt]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Longs, excluding zero.
    */
  implicit val nonZeroLongGenerator: Generator[NonZeroLong] =
    new Generator[NonZeroLong] {

      case class NextRoseTree(value: NonZeroLong, sizeParam: SizeParam, isValidFun: (NonZeroLong, SizeParam) => Boolean) extends RoseTree[NonZeroLong] {
        def shrinks: LazyListOrStream[RoseTree[NonZeroLong]] = {
          def resLazyListOrStream(theValue: NonZeroLong): LazyListOrStream[RoseTree[NonZeroLong]] = {
            val i = theValue.value
            val half: Long = i / 2 // i cannot be zero, because initially it is the underlying Int value of a NonZeroLong (in types
            if (half == 0) LazyListOrStream.empty[RoseTree[NonZeroLong]]     // we trust), then if half results in zero, we return acc here. I.e., we don't loop.
            else {
              LazyListOrStream(NonZeroLong.ensuringValid(-half), NonZeroLong.ensuringValid(half))
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_, sizeParam, isValidFun)) #::: resLazyListOrStream(NonZeroLong.ensuringValid(half))
            }
          }
          resLazyListOrStream(value)
        }
      } // TODO Confirm OK without Roses. I.e., will the last one have an empty shrinks method?

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NonZeroLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(nonZeroLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NonZeroLong, sizeParam: SizeParam, isValidFun: (NonZeroLong, SizeParam) => Boolean): RoseTree[NonZeroLong] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NonZeroLong, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NonZeroLong], Randomizer) = {
        val (nonZeroLong, rnd2) = rnd.nextNonZeroLong
        (NextRoseTree(nonZeroLong, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NonZeroLong]] = {
        case class CanonicalRoseTree(value: NonZeroLong) extends RoseTree[NonZeroLong] {
          def shrinks: LazyListOrStream[RoseTree[NonZeroLong]] = {
            def resLazyListOrStream(theValue: NonZeroLong): LazyListOrStream[RoseTree[NonZeroLong]] = {
              if (theValue.value == 1L) LazyListOrStream.empty
              else {
                val minusOne = NonZeroLong.ensuringValid(theValue.value - 1L)
                if (minusOne.value == 1) Rose(-minusOne) #:: Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(-minusOne) #:: CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(4L).shrinks
      }
      override def toString = "Generator[NonZeroLong]"
      override def shrinksForValue(valueToShrink: NonZeroLong): Option[LazyListOrStream[RoseTree[NonZeroLong]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Doubles, excluding zero but including infinity.
    */
  implicit val negDoubleGenerator: Generator[NegDouble] =
    new Generator[NegDouble] {

      case class NextRoseTree(value: NegDouble, sizeParam: SizeParam, isValidFun: (NegDouble, SizeParam) => Boolean) extends RoseTree[NegDouble] {
        def shrinks: LazyListOrStream[RoseTree[NegDouble]] = {
          def resLazyListOrStream(theValue: NegDouble): LazyListOrStream[RoseTree[NegDouble]] = {
            val fv = theValue.value
            if (fv == -1.0) LazyListOrStream.empty[RoseTree[NegDouble]]
            else if (fv > -1.0) {
              if (isValidFun(NegDouble(-1.0), sizeParam))
                Rose(NegDouble(-1.0)) #:: LazyListOrStream.empty[RoseTree[NegDouble]]
              else
                LazyListOrStream.empty[RoseTree[NegDouble]]  
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.NegativeInfinity || fv.isNaN)
                  Double.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegDouble.ensuringValid(n.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = -(math.sqrt(fv.abs))
              val whole = NegDouble.ensuringValid(sqrt.ceil)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegDouble, sizeParam: SizeParam, isValidFun: (NegDouble, SizeParam) => Boolean): RoseTree[NegDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegDouble], Randomizer) = {
        val (negDouble, rnd2) = rnd.nextNegDouble
        (NextRoseTree(negDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegDouble]] = {
        case class CanonicalRoseTree(value: NegDouble) extends RoseTree[NegDouble] {
          def shrinks: LazyListOrStream[RoseTree[NegDouble]] = {
            def resLazyListOrStream(theValue: NegDouble): LazyListOrStream[RoseTree[NegDouble]] = {
              if (theValue.value == -1.0) LazyListOrStream.empty
              else {
                val plusOne: NegDouble = NegDouble.ensuringValid(theValue.value + 1.0)
                if (plusOne.value == -1.0) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0).shrinks
      }
      override def toString = "Generator[NegDouble]"
      override def shrinksForValue(valueToShrink: NegDouble): Option[LazyListOrStream[RoseTree[NegDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Doubles, excluding zero and infinity.
    */
  implicit val negFiniteDoubleGenerator: Generator[NegFiniteDouble] =
    new Generator[NegFiniteDouble] {

      case class NextRoseTree(value: NegFiniteDouble, sizeParam: SizeParam, isValidFun: (NegFiniteDouble, SizeParam) => Boolean) extends RoseTree[NegFiniteDouble] {
        def shrinks: LazyListOrStream[RoseTree[NegFiniteDouble]] = {
          def resLazyListOrStream(theValue: NegFiniteDouble): LazyListOrStream[RoseTree[NegFiniteDouble]] = {
            val fv = theValue.value
            if (fv == -1.0) LazyListOrStream.empty[RoseTree[NegFiniteDouble]]
            else if (fv > -1.0) {
              if (isValidFun(NegFiniteDouble(-1.0), sizeParam))
                Rose(NegFiniteDouble(-1.0)) #:: LazyListOrStream.empty[RoseTree[NegFiniteDouble]]
              else
                LazyListOrStream.empty[RoseTree[NegFiniteDouble]]  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegFiniteDouble.ensuringValid(fv.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = -(math.sqrt(fv.abs))
              val whole = NegFiniteDouble.ensuringValid(sqrt.ceil)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegFiniteDouble, sizeParam: SizeParam, isValidFun: (NegFiniteDouble, SizeParam) => Boolean): RoseTree[NegFiniteDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegFiniteDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegFiniteDouble], Randomizer) = {
        val (negFiniteDouble, rnd2) = rnd.nextNegFiniteDouble
        (NextRoseTree(negFiniteDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegFiniteDouble]] = {
        case class CanonicalRoseTree(value: NegFiniteDouble) extends RoseTree[NegFiniteDouble] {
          def shrinks: LazyListOrStream[RoseTree[NegFiniteDouble]] = {
            def resLazyListOrStream(theValue: NegFiniteDouble): LazyListOrStream[RoseTree[NegFiniteDouble]] = {
              if (theValue.value == -1.0) LazyListOrStream.empty
              else {
                val plusOne: NegFiniteDouble = NegFiniteDouble.ensuringValid(theValue.value + 1.0)
                if (plusOne.value == -1.0) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0).shrinks
      }
      override def toString = "Generator[NegFiniteDouble]"
      override def shrinksForValue(valueToShrink: NegFiniteDouble): Option[LazyListOrStream[RoseTree[NegFiniteDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Floats, excluding zero but including infinity.
    */
  implicit val negFloatGenerator: Generator[NegFloat] =
    new Generator[NegFloat] {

      case class NextRoseTree(value: NegFloat, sizeParam: SizeParam, isValidFun: (NegFloat, SizeParam) => Boolean) extends RoseTree[NegFloat] {
        def shrinks: LazyListOrStream[RoseTree[NegFloat]] = {
          def resLazyListOrStream(theValue: NegFloat): LazyListOrStream[RoseTree[NegFloat]] = {
            val fv = theValue.value
            if (fv == -1.0f) LazyListOrStream.empty[RoseTree[NegFloat]]
            else if (fv > -1.0f) {
              if (isValidFun(NegFloat(-1.0f), sizeParam))
                Rose(NegFloat(-1.0f)) #:: LazyListOrStream.empty[RoseTree[NegFloat]]
              else
                LazyListOrStream.empty[RoseTree[NegFloat]]  
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.NegativeInfinity || fv.isNaN)
                  Float.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegFloat.ensuringValid(n.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = -(math.sqrt(fv.abs.toDouble)).toFloat
              val whole = NegFloat.ensuringValid(sqrt.ceil)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegFloat, sizeParam: SizeParam, isValidFun: (NegFloat, SizeParam) => Boolean): RoseTree[NegFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegFloat], Randomizer) = {
        val (negFloat, rnd2) = rnd.nextNegFloat
        (NextRoseTree(negFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegFloat]] = {
        case class CanonicalRoseTree(value: NegFloat) extends RoseTree[NegFloat] {
          def shrinks: LazyListOrStream[RoseTree[NegFloat]] = {
            def resLazyListOrStream(theValue: NegFloat): LazyListOrStream[RoseTree[NegFloat]] = {
              if (theValue.value == -1.0f) LazyListOrStream.empty
              else {
                val plusOne: NegFloat = NegFloat.ensuringValid(theValue.value + 1.0f)
                if (plusOne.value == -1.0f) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0f).shrinks
      }
      override def toString = "Generator[NegFloat]"
      override def shrinksForValue(valueToShrink: NegFloat): Option[LazyListOrStream[RoseTree[NegFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Floats, excluding zero and infinity.
    */
  implicit val negFiniteFloatGenerator: Generator[NegFiniteFloat] =
    new Generator[NegFiniteFloat] {

      case class NextRoseTree(value: NegFiniteFloat, sizeParam: SizeParam, isValidFun: (NegFiniteFloat, SizeParam) => Boolean) extends RoseTree[NegFiniteFloat] {
        def shrinks: LazyListOrStream[RoseTree[NegFiniteFloat]] = {
          def resLazyListOrStream(theValue: NegFiniteFloat): LazyListOrStream[RoseTree[NegFiniteFloat]] = {
            val fv = theValue.value
            if (fv == -1.0f) LazyListOrStream.empty[RoseTree[NegFiniteFloat]]
            else if (fv > -1.0f) {
              if (isValidFun(NegFiniteFloat(-1.0f), sizeParam))
                Rose(NegFiniteFloat(-1.0f)) #:: LazyListOrStream.empty[RoseTree[NegFiniteFloat]]
              else
                LazyListOrStream.empty[RoseTree[NegFiniteFloat]]  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegFiniteFloat.ensuringValid(fv.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = -(math.sqrt(fv.abs.toDouble)).toFloat
              val whole = NegFiniteFloat.ensuringValid(sqrt.ceil)
              if (isValidFun(whole, sizeParam))
                NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
              else
                resLazyListOrStream(whole)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegFiniteFloat, sizeParam: SizeParam, isValidFun: (NegFiniteFloat, SizeParam) => Boolean): RoseTree[NegFiniteFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegFiniteFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegFiniteFloat], Randomizer) = {
        val (negFiniteFloat, rnd2) = rnd.nextNegFiniteFloat
        (NextRoseTree(negFiniteFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegFiniteFloat]] = {
        case class CanonicalRoseTree(value: NegFiniteFloat) extends RoseTree[NegFiniteFloat] {
          def shrinks: LazyListOrStream[RoseTree[NegFiniteFloat]] = {
            def resLazyListOrStream(theValue: NegFiniteFloat): LazyListOrStream[RoseTree[NegFiniteFloat]] = {
              if (theValue.value == -1.0f) LazyListOrStream.empty
              else {
                val plusOne: NegFiniteFloat = NegFiniteFloat.ensuringValid(theValue.value + 1.0f)
                if (plusOne.value == -1.0f) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0f).shrinks
      }
      override def toString = "Generator[NegFiniteFloat]"
      override def shrinksForValue(valueToShrink: NegFiniteFloat): Option[LazyListOrStream[RoseTree[NegFiniteFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Ints, excluding zero.
    */
  implicit val negIntGenerator: Generator[NegInt] =
    new Generator[NegInt] {

      case class NextRoseTree(value: NegInt, sizeParam: SizeParam, isValidFun: (NegInt, SizeParam) => Boolean) extends RoseTree[NegInt] {
        def shrinks: LazyListOrStream[RoseTree[NegInt]] = {
          def resLazyListOrStream(theValue: NegInt): LazyListOrStream[RoseTree[NegInt]] = {
            val i = theValue.value
            val half: Int = i / 2
            if (half == 0) LazyListOrStream.empty[RoseTree[NegInt]]
            else {
              val negIntHalf = NegInt.ensuringValid(half)
              if (isValidFun(negIntHalf, sizeParam))
                NextRoseTree(negIntHalf, sizeParam, isValidFun) #:: resLazyListOrStream(negIntHalf)
              else
                resLazyListOrStream(negIntHalf)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegInt, sizeParam: SizeParam, isValidFun: (NegInt, SizeParam) => Boolean): RoseTree[NegInt] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegInt, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegInt], Randomizer) = {
        val (negInt, rnd2) = rnd.nextNegInt
        (NextRoseTree(negInt, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegInt]] = {
        case class CanonicalRoseTree(value: NegInt) extends RoseTree[NegInt] {
          def shrinks: LazyListOrStream[RoseTree[NegInt]] = {
            def resLazyListOrStream(theValue: NegInt): LazyListOrStream[RoseTree[NegInt]] = {
              if (theValue.value == -1) LazyListOrStream.empty
              else {
                val plusOne: NegInt = NegInt.ensuringValid(theValue.value + 1)
                if (plusOne.value == -1) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4).shrinks
      }
      override def toString = "Generator[NegInt]"
      override def shrinksForValue(valueToShrink: NegInt): Option[LazyListOrStream[RoseTree[NegInt]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Longs, excluding zero.
    */
  implicit val negLongGenerator: Generator[NegLong] =
    new Generator[NegLong] {

      case class NextRoseTree(value: NegLong, sizeParam: SizeParam, isValidFun: (NegLong, SizeParam) => Boolean) extends RoseTree[NegLong] {
        def shrinks: LazyListOrStream[RoseTree[NegLong]] = {
          def resLazyListOrStream(theValue: NegLong): LazyListOrStream[RoseTree[NegLong]] = {
            val i = theValue.value
            val half: Long = i / 2
            if (half == 0) LazyListOrStream.empty[RoseTree[NegLong]]
            else {
              val negLongHalf = NegLong.ensuringValid(half)
              if (isValidFun(negLongHalf, sizeParam))
                NextRoseTree(negLongHalf, sizeParam, isValidFun) #:: resLazyListOrStream(negLongHalf)
              else
                resLazyListOrStream(negLongHalf)
            }
          }
          resLazyListOrStream(value)
        }
      } // TODO: Confirm OK with no Roses.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegLong, sizeParam: SizeParam, isValidFun: (NegLong, SizeParam) => Boolean): RoseTree[NegLong] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegLong, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegLong], Randomizer) = {
        val (negLong, rnd2) = rnd.nextNegLong
        (NextRoseTree(negLong, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegLong]] = {
        case class CanonicalRoseTree(value: NegLong) extends RoseTree[NegLong] {
          def shrinks: LazyListOrStream[RoseTree[NegLong]] = {
            def resLazyListOrStream(theValue: NegLong): LazyListOrStream[RoseTree[NegLong]] = {
              if (theValue.value == -1) LazyListOrStream.empty
              else {
                val plusOne: NegLong = NegLong.ensuringValid(theValue.value + 1)
                if (plusOne.value == -1) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4L).shrinks
      }
      override def toString = "Generator[NegLong]"
      override def shrinksForValue(valueToShrink: NegLong): Option[LazyListOrStream[RoseTree[NegLong]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Doubles, including zero and infinity.
    */
  implicit val negZDoubleGenerator: Generator[NegZDouble] =
    new Generator[NegZDouble] {

      case class NextRoseTree(value: NegZDouble, sizeParam: SizeParam, isValidFun: (NegZDouble, SizeParam) => Boolean) extends RoseTree[NegZDouble] {
        def shrinks: LazyListOrStream[RoseTree[NegZDouble]] = {
          def resLazyListOrStream(theValue: NegZDouble): LazyListOrStream[RoseTree[NegZDouble]] = {
            val fv = theValue.value
            if (fv == 0.0) LazyListOrStream.empty[RoseTree[NegZDouble]]
            else if (fv >= -1.0) { 
              if (isValidFun(NegZDouble(0.0), sizeParam))
                Rose(NegZDouble(0.0)) #:: LazyListOrStream.empty[RoseTree[NegZDouble]]
              else
                LazyListOrStream.empty[RoseTree[NegZDouble]]  
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Double.NegativeInfinity || fv.isNaN)
                  Double.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegZDouble.ensuringValid(n.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = -math.sqrt(fv.abs)
              if (sqrt > -1.0)  {
                if (isValidFun(NegZDouble(0.0), sizeParam))
                  Rose(NegZDouble(0.0)) #:: LazyListOrStream.empty[RoseTree[NegZDouble]]
                else
                  LazyListOrStream.empty[RoseTree[NegZDouble]]  
              }
              else {
                val whole = NegZDouble.ensuringValid(sqrt.ceil)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)  
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegZDouble, sizeParam: SizeParam, isValidFun: (NegZDouble, SizeParam) => Boolean): RoseTree[NegZDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegZDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegZDouble], Randomizer) = {
        val (negZDouble, rnd2) = rnd.nextNegZDouble
        (NextRoseTree(negZDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegZDouble]] = {
        case class CanonicalRoseTree(value: NegZDouble) extends RoseTree[NegZDouble] {
          def shrinks: LazyListOrStream[RoseTree[NegZDouble]] = {
            def resLazyListOrStream(theValue: NegZDouble): LazyListOrStream[RoseTree[NegZDouble]] = {
              if (theValue.value == 0.0) LazyListOrStream.empty
              else {
                val plusOne: NegZDouble = NegZDouble.ensuringValid(theValue.value + 1.0)
                if (plusOne.value == 0.0) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0).shrinks
      }
      override def toString = "Generator[NegZDouble]"
      override def shrinksForValue(valueToShrink: NegZDouble): Option[LazyListOrStream[RoseTree[NegZDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Doubles, including zero but excluding infinity.
    */
  implicit val negZFiniteDoubleGenerator: Generator[NegZFiniteDouble] =
    new Generator[NegZFiniteDouble] {

      case class NextRoseTree(value: NegZFiniteDouble, sizeParam: SizeParam, isValidFun: (NegZFiniteDouble, SizeParam) => Boolean) extends RoseTree[NegZFiniteDouble] {
        def shrinks: LazyListOrStream[RoseTree[NegZFiniteDouble]] = {
          def resLazyListOrStream(theValue: NegZFiniteDouble): LazyListOrStream[RoseTree[NegZFiniteDouble]] = {
            val fv = theValue.value
            if (fv == 0.0) LazyListOrStream.empty[RoseTree[NegZFiniteDouble]]
            else if (fv >= -1.0) { 
              if (isValidFun(NegZFiniteDouble(0.0), sizeParam))
                Rose(NegZFiniteDouble(0.0)) #:: LazyListOrStream.empty[RoseTree[NegZFiniteDouble]]
              else
                LazyListOrStream.empty[RoseTree[NegZFiniteDouble]]  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegZFiniteDouble.ensuringValid(fv.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Double = -math.sqrt(fv.abs)
              if (sqrt > -1.0) {
                if (isValidFun(NegZFiniteDouble(0.0), sizeParam))
                  Rose(NegZFiniteDouble(0.0)) #:: LazyListOrStream.empty[RoseTree[NegZFiniteDouble]]
                else
                  LazyListOrStream.empty[RoseTree[NegZFiniteDouble]]  
              }
              else {
                val whole = NegZFiniteDouble.ensuringValid(sqrt.ceil)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)  
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFiniteDouble], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFiniteDoubleEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegZFiniteDouble, sizeParam: SizeParam, isValidFun: (NegZFiniteDouble, SizeParam) => Boolean): RoseTree[NegZFiniteDouble] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegZFiniteDouble, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegZFiniteDouble], Randomizer) = {
        val (negZFiniteDouble, rnd2) = rnd.nextNegZFiniteDouble
        (NextRoseTree(negZFiniteDouble, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegZFiniteDouble]] = {
        case class CanonicalRoseTree(value: NegZFiniteDouble) extends RoseTree[NegZFiniteDouble] {
          def shrinks: LazyListOrStream[RoseTree[NegZFiniteDouble]] = {
            def resLazyListOrStream(theValue: NegZFiniteDouble): LazyListOrStream[RoseTree[NegZFiniteDouble]] = {
              if (theValue.value == 0.0) LazyListOrStream.empty
              else {
                val plusOne: NegZFiniteDouble = NegZFiniteDouble.ensuringValid(theValue.value + 1.0)
                if (plusOne.value == 0.0) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0).shrinks
      }
      override def toString = "Generator[NegZFiniteDouble]"
      override def shrinksForValue(valueToShrink: NegZFiniteDouble): Option[LazyListOrStream[RoseTree[NegZFiniteDouble]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Floats, including zero and infinity.
    */
  implicit val negZFloatGenerator: Generator[NegZFloat] =
    new Generator[NegZFloat] {

      case class NextRoseTree(value: NegZFloat, sizeParam: SizeParam, isValidFun: (NegZFloat, SizeParam) => Boolean) extends RoseTree[NegZFloat] {
        def shrinks: LazyListOrStream[RoseTree[NegZFloat]] = {
          def resLazyListOrStream(theValue: NegZFloat): LazyListOrStream[RoseTree[NegZFloat]] = {
            val fv = theValue.value
            if (fv == 0.0f) LazyListOrStream.empty[RoseTree[NegZFloat]]
            else if (fv >= -1.0f) {
              if (isValidFun(NegZFloat(0.0f), sizeParam))
                Rose(NegZFloat(0.0f)) #:: LazyListOrStream.empty[RoseTree[NegZFloat]]
              else
                LazyListOrStream.empty[RoseTree[NegZFloat]]  
            }
            else if (!fv.isWhole) {
              val n =
                if (fv == Float.NegativeInfinity || fv.isNaN)
                  Float.MinValue
                else fv
              // Nearest whole numbers closer to zero
              val nearest = NegZFloat.ensuringValid(n.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = -math.sqrt(fv.abs.toDouble).toFloat
              if (sqrt > -1.0f) {
                if (isValidFun(NegZFloat(0.0f), sizeParam))
                  Rose(NegZFloat(0.0f)) #:: LazyListOrStream.empty[RoseTree[NegZFloat]]
                else
                  LazyListOrStream.empty[RoseTree[NegZFloat]]  
              }
              else {
                val whole = NegZFloat.ensuringValid(sqrt.ceil)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)  
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegZFloat, sizeParam: SizeParam, isValidFun: (NegZFloat, SizeParam) => Boolean): RoseTree[NegZFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegZFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegZFloat], Randomizer) = {
        val (negZFloat, rnd2) = rnd.nextNegZFloat
        (NextRoseTree(negZFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegZFloat]] = {
        case class CanonicalRoseTree(value: NegZFloat) extends RoseTree[NegZFloat] {
          def shrinks: LazyListOrStream[RoseTree[NegZFloat]] = {
            def resLazyListOrStream(theValue: NegZFloat): LazyListOrStream[RoseTree[NegZFloat]] = {
              if (theValue.value == 0.0f) LazyListOrStream.empty
              else {
                val plusOne: NegZFloat = NegZFloat.ensuringValid(theValue.value + 1.0f)
                if (plusOne.value == 0.0f) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0f).shrinks
      }
      override def toString = "Generator[NegZFloat]"
      override def shrinksForValue(valueToShrink: NegZFloat): Option[LazyListOrStream[RoseTree[NegZFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Floats, including zero but excluding infinity.
    */
  implicit val negZFiniteFloatGenerator: Generator[NegZFiniteFloat] =
    new Generator[NegZFiniteFloat] {

      case class NextRoseTree(value: NegZFiniteFloat, sizeParam: SizeParam, isValidFun: (NegZFiniteFloat, SizeParam) => Boolean) extends RoseTree[NegZFiniteFloat] {
        def shrinks: LazyListOrStream[RoseTree[NegZFiniteFloat]] = {
          def resLazyListOrStream(theValue: NegZFiniteFloat): LazyListOrStream[RoseTree[NegZFiniteFloat]] = {
            val fv = theValue.value
            if (fv == 0.0f) LazyListOrStream.empty[RoseTree[NegZFiniteFloat]]
            else if (fv >= -1.0f) {
              if (isValidFun(NegZFiniteFloat(0.0f), sizeParam))
                Rose(NegZFiniteFloat(0.0f)) #:: LazyListOrStream.empty[RoseTree[NegZFiniteFloat]]
              else
                LazyListOrStream.empty[RoseTree[NegZFiniteFloat]]  
            }
            else if (!fv.isWhole) {
              // Nearest whole numbers closer to zero
              val nearest = NegZFiniteFloat.ensuringValid(fv.ceil)
              if (isValidFun(nearest, sizeParam))
                NextRoseTree(nearest, sizeParam, isValidFun) #:: resLazyListOrStream(nearest)
              else
                resLazyListOrStream(nearest)  
            }
            else {
              val sqrt: Float = -math.sqrt(fv.abs.toDouble).toFloat
              if (sqrt > -1.0f) {
                if (isValidFun(NegZFiniteFloat(0.0f), sizeParam))
                  Rose(NegZFiniteFloat(0.0f)) #:: LazyListOrStream.empty[RoseTree[NegZFiniteFloat]]
                else
                  LazyListOrStream.empty[RoseTree[NegZFiniteFloat]]  
              }
              else {
                val whole = NegZFiniteFloat.ensuringValid(sqrt.ceil)
                if (isValidFun(whole, sizeParam))
                  NextRoseTree(whole, sizeParam, isValidFun) #:: resLazyListOrStream(whole)
                else
                  resLazyListOrStream(whole)  
              }
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZFiniteFloat], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZFiniteFloatEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegZFiniteFloat, sizeParam: SizeParam, isValidFun: (NegZFiniteFloat, SizeParam) => Boolean): RoseTree[NegZFiniteFloat] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegZFiniteFloat, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegZFiniteFloat], Randomizer) = {
        val (negZFiniteFloat, rnd2) = rnd.nextNegZFiniteFloat
        (NextRoseTree(negZFiniteFloat, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegZFiniteFloat]] = {
        case class CanonicalRoseTree(value: NegZFiniteFloat) extends RoseTree[NegZFiniteFloat] {
          def shrinks: LazyListOrStream[RoseTree[NegZFiniteFloat]] = {
            def resLazyListOrStream(theValue: NegZFiniteFloat): LazyListOrStream[RoseTree[NegZFiniteFloat]] = {
              if (theValue.value == 0.0f) LazyListOrStream.empty
              else {
                val plusOne: NegZFiniteFloat = NegZFiniteFloat.ensuringValid(theValue.value + 1.0f)
                if (plusOne.value == 0.0f) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4.0f).shrinks
      }
      override def toString = "Generator[NegZFiniteFloat]"
      override def shrinksForValue(valueToShrink: NegZFiniteFloat): Option[LazyListOrStream[RoseTree[NegZFiniteFloat]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Ints, including zero.
    */
  implicit val negZIntGenerator: Generator[NegZInt] =
    new Generator[NegZInt] {

      case class NextRoseTree(value: NegZInt, sizeParam: SizeParam, isValidFun: (NegZInt, SizeParam) => Boolean) extends RoseTree[NegZInt] {
        def shrinks: LazyListOrStream[RoseTree[NegZInt]] = {
          def resLazyListOrStream(theValue: NegZInt): LazyListOrStream[RoseTree[NegZInt]] = {
            if (theValue.value == 0)
              LazyListOrStream.empty[RoseTree[NegZInt]]
            else {
              val half: Int = theValue.value / 2
              val negZIntHalf = NegZInt.ensuringValid(half)
              if (isValidFun(negZIntHalf, sizeParam))
                NextRoseTree(negZIntHalf, sizeParam, isValidFun) #:: resLazyListOrStream(negZIntHalf)
              else
                resLazyListOrStream(negZIntHalf)  
            }
          }
          resLazyListOrStream(value)
        }
      } // TODO Confirm OK with no Rose.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZInt], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZIntEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegZInt, sizeParam: SizeParam, isValidFun: (NegZInt, SizeParam) => Boolean): RoseTree[NegZInt] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegZInt, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegZInt], Randomizer) = {
        val (negZInt, rnd2) = rnd.nextNegZInt
        (NextRoseTree(negZInt, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegZInt]] = {
        case class CanonicalRoseTree(value: NegZInt) extends RoseTree[NegZInt] {
          def shrinks: LazyListOrStream[RoseTree[NegZInt]] = {
            def resLazyListOrStream(theValue: NegZInt): LazyListOrStream[RoseTree[NegZInt]] = {
              if (theValue.value == 0) LazyListOrStream.empty
              else {
                val plusOne: NegZInt = NegZInt.ensuringValid(theValue.value + 1)
                if (plusOne.value == 0) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4).shrinks
      }
      override def toString = "Generator[NegZInt]"
      override def shrinksForValue(valueToShrink: NegZInt): Option[LazyListOrStream[RoseTree[NegZInt]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces negative Longs, including zero.
    */
  implicit val negZLongGenerator: Generator[NegZLong] =
    new Generator[NegZLong] {

      case class NextRoseTree(value: NegZLong, sizeParam: SizeParam, isValidFun: (NegZLong, SizeParam) => Boolean) extends RoseTree[NegZLong] {
        def shrinks: LazyListOrStream[RoseTree[NegZLong]] = {
          def resLazyListOrStream(theValue: NegZLong): LazyListOrStream[RoseTree[NegZLong]] = {
            if (theValue.value == 0)
              LazyListOrStream.empty[RoseTree[NegZLong]]
            else {
              val half: Long = theValue.value / 2
              val negLongHalf = NegZLong.ensuringValid(half)
              if (isValidFun(negLongHalf, sizeParam))
                NextRoseTree(negLongHalf, sizeParam, isValidFun) #:: resLazyListOrStream(negLongHalf)
              else
                resLazyListOrStream(negLongHalf)  
            }
          }
          resLazyListOrStream(value)
        }
      } // TODO Confirm OK no Rose.

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NegZLong], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(negZLongEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NegZLong, sizeParam: SizeParam, isValidFun: (NegZLong, SizeParam) => Boolean): RoseTree[NegZLong] = NextRoseTree(edge, sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NegZLong, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NegZLong], Randomizer) = {
        val (negZLong, rnd2) = rnd.nextNegZLong
        (NextRoseTree(negZLong, szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NegZLong]] = {
        case class CanonicalRoseTree(value: NegZLong) extends RoseTree[NegZLong] {
          def shrinks: LazyListOrStream[RoseTree[NegZLong]] = {
            def resLazyListOrStream(theValue: NegZLong): LazyListOrStream[RoseTree[NegZLong]] = {
              if (theValue.value == 0L) LazyListOrStream.empty
              else {
                val plusOne: NegZLong = NegZLong.ensuringValid(theValue.value + 1)
                if (plusOne.value == 0L) Rose(plusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(plusOne) #:: resLazyListOrStream(plusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree(-4L).shrinks
      }
      override def toString = "Generator[NegZLong]"
      override def shrinksForValue(valueToShrink: NegZLong): Option[LazyListOrStream[RoseTree[NegZLong]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }

  /**
    * A [[Generator]] that produces Chars, but only the ones that represent digits.
    */
  implicit val numericCharGenerator: Generator[NumericChar] =
    new Generator[NumericChar] {

      case class NextRoseTree(value: NumericChar)(sizeParam: SizeParam, isValidFun: (NumericChar, SizeParam) => Boolean) extends RoseTree[NumericChar] {
        def shrinks: LazyListOrStream[RoseTree[NumericChar]] = {
          def resLazyListOrStream(theValue: NumericChar): LazyListOrStream[RoseTree[NumericChar]] = {
            if (theValue.value == '0')
              LazyListOrStream.empty
            else {
              val minusOne: Char = (theValue.value - 1).toChar // Go ahead and try all the values between i and '0'
              val numericCharMinusOne = NumericChar.ensuringValid(minusOne)
              if (isValidFun(numericCharMinusOne, sizeParam))
                NextRoseTree(numericCharMinusOne)(sizeParam, isValidFun) #:: resLazyListOrStream(numericCharMinusOne)
              else
                resLazyListOrStream(numericCharMinusOne)  
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[NumericChar], Randomizer) = {
        val (allEdges, nextRnd) = Randomizer.shuffle(numericCharEdges, rnd)
        (allEdges.take(maxLength), nextRnd)
      }
      override def roseTreeOfEdge(edge: NumericChar, sizeParam: SizeParam, isValidFun: (NumericChar, SizeParam) => Boolean): RoseTree[NumericChar] = NextRoseTree(edge)(sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (NumericChar, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[NumericChar], Randomizer) = {
        val (posZInt, rnd2) = rnd.choosePosZInt(PosZInt.ensuringValid(0), PosZInt.ensuringValid(9))
        (NextRoseTree(NumericChar.ensuringValid((posZInt.value + 48).toChar))(szp, isValidFun), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[NumericChar]] = {
        case class CanonicalRoseTree(value: NumericChar) extends RoseTree[NumericChar] {
          def shrinks: LazyListOrStream[RoseTree[NumericChar]] = {
            def resLazyListOrStream(theValue: NumericChar): LazyListOrStream[RoseTree[NumericChar]] = {
              if (theValue.value == '0') LazyListOrStream.empty
              else {
                val minusOne: NumericChar = NumericChar.ensuringValid((theValue.value - 1).toChar)
                if (minusOne.value == '0') Rose(minusOne) #:: LazyListOrStream.empty
                else CanonicalRoseTree(minusOne) #:: resLazyListOrStream(minusOne)
              }
            }
            resLazyListOrStream(value)
          }
        }
        CanonicalRoseTree('4').shrinks
      }
      override def toString = "Generator[NumericChar]"
      override def shrinksForValue(valueToShrink: NumericChar): Option[LazyListOrStream[RoseTree[NumericChar]]] = Some(NextRoseTree(valueToShrink)(SizeParam(1, 0, 1), isValid).shrinks)
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

      // For strings, we won't shrink the characters.  We could, but the trees could get really big. Just cut the length of
      // the list in half and try both halves each round, using the same characters.
      // TODO: Write a test for this shrinks implementation.
      case class NextRoseTree(value: String)(sizeParam: SizeParam, isValidFun: (String, SizeParam) => Boolean) extends RoseTree[String] {
        def shrinks: LazyListOrStream[RoseTree[String]] = {
          def resLazyListOrStream(theValue: String): LazyListOrStream[RoseTree[String]] = {
            if (theValue.isEmpty)
              LazyListOrStream.empty
            else if (theValue.length == 1) {
              if (isValidFun("", sizeParam))
                Rose("") #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else {
              val halfSize = theValue.length / 2
              val firstHalf = theValue.take(halfSize)
              val secondHalf = theValue.drop(halfSize)
              // If value has an odd number of chars, the second half will be one character longer than the first half.
              LazyListOrStream(secondHalf, firstHalf)
                .filter(isValidFun(_, sizeParam))
                .map(NextRoseTree(_)(sizeParam, isValidFun)) #::: resLazyListOrStream(firstHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[String], Randomizer) = {
        (stringEdges.take(maxLength), rnd)
      }
      override def roseTreeOfEdge(edge: String, sizeParam: SizeParam, isValidFun: (String, SizeParam) => Boolean): RoseTree[String] = NextRoseTree(edge)(sizeParam, isValidFun)
      def nextImpl(szp: SizeParam, isValidFun: (String, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[String], Randomizer) = {
        val (s, rnd2) = rnd.nextString(szp.size)
        (NextRoseTree(s)(szp, isValid), rnd2)
      }
      override def canonicals: LazyListOrStream[RoseTree[String]] = {
        val canonicalsOfChar = charGenerator.canonicals
        canonicalsOfChar.map(t => Rose(s"${ t.value }")) #::: LazyListOrStream(Rose(""))
      }
      override def toString = "Generator[String]"
      override def shrinksForValue(valueToShrink: String): Option[LazyListOrStream[RoseTree[String]]] = Some(NextRoseTree(valueToShrink)(SizeParam(1, 0, 1), isValid).shrinks)
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

      // For lists, we won't bother shrinking the elements. We could, but the trees could get very big.
      // So we will just cut the length of the list in half and try both
      // halves each round, using the same elements.
      // TODO: Write a test for this shrinks implementation.
      case class NextRoseTree(value: List[T], sizeParam: SizeParam, isValidFun: (List[T], SizeParam) => Boolean) extends RoseTree[List[T]] {
        def shrinks: LazyListOrStream[RoseTree[List[T]]] = {
          def resLazyListOrStream(theValue: List[T]): LazyListOrStream[RoseTree[List[T]]] = {
            if (theValue.isEmpty) 
              LazyListOrStream.empty
            else if (theValue.length == 1) {
              if (isValidFun(List.empty, sizeParam))
                Rose(List.empty) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty
            }
            else {
              val halfSize = theValue.length / 2 // Linear time
              val firstHalf = theValue.take(halfSize)
              val secondHalf = theValue.drop(halfSize)
              // If value has an odd number of elements, the second half will be one character longer than the first half.
              LazyListOrStream(secondHalf, firstHalf).filter(v => isValidFun(v, sizeParam))
                                                     .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(firstHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }

      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[List[T]], Randomizer) = {
        (listEdges.take(maxLength), rnd)
      }

      override def roseTreeOfEdge(edge: List[T], sizeParam: SizeParam, isValidFun: (List[T], SizeParam) => Boolean): RoseTree[List[T]] = NextRoseTree(edge, sizeParam, isValid)

      def generatorWithSize(szp: SizeParam): Generator[List[T]] =
        new Generator[List[T]] {
          override def roseTreeOfEdge(edge: List[T], sizeParam: SizeParam, isValidFun: (List[T], SizeParam) => Boolean): RoseTree[List[T]] = NextRoseTree(edge, szp, isValid)
          def nextImpl(nextSzp: org.scalatest.prop.SizeParam, isValidFun: (List[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[List[T]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: List[T], rnd: org.scalatest.prop.Randomizer): (RoseTree[List[T]], org.scalatest.prop.Randomizer) =
              if (result.length == targetSize)
                (NextRoseTree(result, szp, isValid), rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfT.next(szp, List.empty, rnd)
                loop(targetSize, result :+ nextRoseTreeOfT.value, nextRnd)
              }

            val nextSize = {
              val candidate: Int = (szp.minSize + (nextSzp.size.toFloat * (szp.maxSize - szp.minSize).toFloat / (nextSzp.maxSize + 1).toFloat)).round
              if (candidate > szp.maxSize) szp.maxSize
              else if (candidate < szp.minSize) szp.minSize
              else PosZInt.ensuringValid(candidate)
            }

            loop(nextSize.value, List.empty, rnd)
          }

          // If from is either 0 or 1, return the canonicals of the outer Generator.
          override def canonicals: LazyListOrStream[RoseTree[List[T]]] =
            if (szp.minSize <= 1) outerGenOfListOfT.canonicals else LazyListOrStream.empty

          override def isValid(value: List[T], size: SizeParam): Boolean = value.length >= szp.minSize.value && value.size <= (szp.minSize.value + szp.sizeRange.value)
        }

      def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (List[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[List[T]], org.scalatest.prop.Randomizer) = {
        val gen = generatorWithSize(szp)
        gen.nextImpl(szp, isValidFun, rnd)
      }

      override def canonicals: LazyListOrStream[RoseTree[List[T]]] = {
        val canonicalsOfT = genOfT.canonicals
        canonicalsOfT.map(rt => rt.map(t => List(t)))
      }

      override def toString = "Generator[List[T]]"
      // Members declared in org.scalatest.prop.HavingSize
      def havingSize(len: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[List[T]] = generatorWithSize(SizeParam(len, 0, len))
      def havingSizesBetween(from: org.scalactic.anyvals.PosZInt,to: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[List[T]] = {
        require(from != to, Resources.fromEqualToToHavingSizesBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingSizesBetween(from, to))
        generatorWithSize(SizeParam(from, PosZInt.ensuringValid(to - from), from))
      }
      def havingSizesDeterminedBy(f: org.scalatest.prop.SizeParam => org.scalatest.prop.SizeParam): org.scalatest.prop.Generator[List[T]] =
        new Generator[List[T]] {
          def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (List[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[List[T]], org.scalatest.prop.Randomizer) = {
            val s = f(szp)
            val gen = generatorWithSize(s)
            gen.nextImpl(s, isValidFun, rnd)
          }
          override def isValid(value: List[T], sizeParam: SizeParam): Boolean = {
            val fSizeParam = f(sizeParam)
            value.length >= fSizeParam.minSize.value && value.length <= (fSizeParam.minSize.value + fSizeParam.sizeRange.value)
          }
        }
      override def shrinksForValue(valueToShrink: List[T]): Option[LazyListOrStream[RoseTree[List[T]]]] = Some(NextRoseTree(valueToShrink, SizeParam(0, 0, 0), isValid).shrinks)
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
      def nextImpl(szp: SizeParam, isValidFun: (() => T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[() => T], Randomizer) = {
        val (nextRoseTreeOfT, _, nextRnd) = genOfT.next(szp, Nil, rnd)
        (nextRoseTreeOfT.map(t => PrettyFunction0(t)), nextRnd)
      }
      override def canonicals: LazyListOrStream[RoseTree[() => T]] = {
        val canonicalsOfT = genOfT.canonicals
        canonicalsOfT.map(rt => rt.map(t => PrettyFunction0(t): Function0[T]))
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
      def nextImpl(szp: SizeParam, isValidFun: (Int => Int, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Int => Int], Randomizer) = {
        val (nextInt, nextRnd) = rnd.nextInt
        val idx = (if (nextInt == Int.MinValue) Int.MaxValue else nextInt.abs) % funs.length
        (Rose(funs(idx)), nextRnd)
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
      def nextImpl(szp: SizeParam, isValidFun: (A => B, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[A => B], Randomizer) = {

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

        (Rose(AToB), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function2Generator[A, B, C](implicit genOfC: Generator[C], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C]): Generator[(A, B) => C] = {
    new Generator[(A, B) => C] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B) => C, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B) => C], Randomizer) = {
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

        (Rose(ABToC), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function3Generator[A, B, C, D](implicit genOfD: Generator[D], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D]): Generator[(A, B, C) => D] = {
    new Generator[(A, B, C) => D] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C) => D, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C) => D], Randomizer) = {
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

        (Rose(ABCToD), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function4Generator[A, B, C, D, E](implicit genOfE: Generator[E], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E]): Generator[(A, B, C, D) => E] = {
    new Generator[(A, B, C, D) => E] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D) => E, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D) => E], Randomizer) = {
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

        (Rose(ABCDToE), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function5Generator[A, B, C, D, E, F](implicit genOfF: Generator[F], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F]): Generator[(A, B, C, D, E) => F] = {
    new Generator[(A, B, C, D, E) => F] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E) => F, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E) => F], Randomizer) = {
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

        (Rose(ABCDEToF), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function6Generator[A, B, C, D, E, F, G](implicit genOfG: Generator[G], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G]): Generator[(A, B, C, D, E, F) => G] = {
    new Generator[(A, B, C, D, E, F) => G] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F) => G, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F) => G], Randomizer) = {
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

        (Rose(ABCDEFToG), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function7Generator[A, B, C, D, E, F, G, H](implicit genOfH: Generator[H], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H]): Generator[(A, B, C, D, E, F, G) => H] = {
    new Generator[(A, B, C, D, E, F, G) => H] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G) => H, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G) => H], Randomizer) = {
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

        (Rose(ABCDEFGToH), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function8Generator[A, B, C, D, E, F, G, H, I](implicit genOfI: Generator[I], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I]): Generator[(A, B, C, D, E, F, G, H) => I] = {
    new Generator[(A, B, C, D, E, F, G, H) => I] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H) => I, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H) => I], Randomizer) = {
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

        (Rose(ABCDEFGHToI), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function9Generator[A, B, C, D, E, F, G, H, I, J](implicit genOfJ: Generator[J], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J]): Generator[(A, B, C, D, E, F, G, H, I) => J] = {
    new Generator[(A, B, C, D, E, F, G, H, I) => J] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I) => J, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I) => J], Randomizer) = {
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

        (Rose(ABCDEFGHIToJ), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function10Generator[A, B, C, D, E, F, G, H, I, J, K](implicit genOfK: Generator[K], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K]): Generator[(A, B, C, D, E, F, G, H, I, J) => K] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J) => K] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J) => K, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J) => K], Randomizer) = {
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

        (Rose(ABCDEFGHIJToK), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function11Generator[A, B, C, D, E, F, G, H, I, J, K, L](implicit genOfL: Generator[L], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L]): Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K) => L] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K) => L, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K) => L], Randomizer) = {
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

        (Rose(ABCDEFGHIJKToL), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function12Generator[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit genOfM: Generator[M], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L) => M] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L) => M, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L) => M], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLToM), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function13Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit genOfN: Generator[N], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M) => N, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M) => N], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMToN), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function14Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit genOfO: Generator[O], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N) => O], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNToO), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function15Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit genOfP: Generator[P], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O) => P], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOToP), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function16Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit genOfQ: Generator[Q], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P) => Q], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOPToQ), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function17Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit genOfR: Generator[R], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q) => R], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOPQToR), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function18Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit genOfS: Generator[S], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R) => S], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOPQRToS), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function19Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit genOfT: Generator[T], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S) => T], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOPQRSToT), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function20Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit genOfU: Generator[U], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T) => U], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOPQRSTToU), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function21Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit genOfV: Generator[V], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U) => V], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOPQRSTUToV), rnd1)
      }
    }
  }

  /**
    * See [[function1Generator]].
    */
  implicit def function22Generator[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W](implicit genOfW: Generator[W], typeInfoA: TypeInfo[A], typeInfoB: TypeInfo[B], typeInfoC: TypeInfo[C], typeInfoD: TypeInfo[D], typeInfoE: TypeInfo[E], typeInfoF: TypeInfo[F], typeInfoG: TypeInfo[G], typeInfoH: TypeInfo[H], typeInfoI: TypeInfo[I], typeInfoJ: TypeInfo[J], typeInfoK: TypeInfo[K], typeInfoL: TypeInfo[L], typeInfoM: TypeInfo[M], typeInfoN: TypeInfo[N], typeInfoO: TypeInfo[O], typeInfoP: TypeInfo[P], typeInfoQ: TypeInfo[Q], typeInfoR: TypeInfo[R], typeInfoS: TypeInfo[S], typeInfoT: TypeInfo[T], typeInfoU: TypeInfo[U], typeInfoV: TypeInfo[V], typeInfoW: TypeInfo[W]): Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] = {
    new Generator[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W] {
      def nextImpl(szp: SizeParam, isValidFun: ((A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V) => W], Randomizer) = {
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

        (Rose(ABCDEFGHIJKLMNOPQRSTUVToW), rnd1)
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
  implicit def optionGenerator[T](implicit genOfT: Generator[T]): Generator[Option[T]] = {
    case class NextRoseTree(value: Option[T], sizeParam: SizeParam, isValidFun: (Option[T], SizeParam) => Boolean) extends RoseTree[Option[T]] { thisRoseTreeOfOptionOfT =>
        def shrinks: LazyListOrStream[RoseTree[Option[T]]] = {

          value match {
            // If there is a real value, t, shrink that value, and return that and None.
            case Some(t) =>
              val nestedRoseTreesOpt: Option[LazyListOrStream[RoseTree[T]]] = genOfT.shrinksForValue(t)
              nestedRoseTreesOpt match {
                case Some(nestedRoseTrees) =>
                  val nestedList: LazyListOrStream[RoseTree[Option[T]]] =
                    nestedRoseTrees.map(nrt => nrt.map(t => Some(t))) 
                      .filter(rt => isValidFun(rt.value, sizeParam))
                      .map(rt => NextRoseTree(rt.value, sizeParam, isValidFun)) 
                  nestedList #::: (if (isValidFun(None, sizeParam)) LazyListOrStream[RoseTree[Option[T]]](Rose(None)) else LazyListOrStream.empty[RoseTree[Option[T]]])
                case None =>
                  // If the shrinksForValue lazy list is empty, degrade to canonicals.
                  val canonicalTs = genOfT.canonicals
                  canonicalTs.map(rt => rt.map(t => Some(t)))
                    .filter(rt => isValidFun(rt.value, sizeParam))
                    .map(rt => NextRoseTree(rt.value, sizeParam, isValidFun)) #:::
                  (if (isValidFun(None, sizeParam)) LazyListOrStream[RoseTree[Option[T]]](Rose(None)) else LazyListOrStream.empty[RoseTree[Option[T]]])
              }

            // There's no way to simplify None:
            case None =>
              LazyListOrStream.empty
          }
        }
      }

    new Generator[Option[T]] {

      // TODO: Ah, maybe edges should return List[RoseTree[Option[T]], Randomizer] instead. Then it could be shrunken.
      override def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Option[T]], Randomizer) = {
        // Subtract one from length, and we'll wrap those in Somes. Subtract one so that None can be the first edge.
        val (edgesOfT, nextRnd) = genOfT.initEdges(if (maxLength > 0) PosZInt.ensuringValid((maxLength - 1)) else 0, rnd)
        val edges = None :: edgesOfT.map(t => Some(t))
        (edges, nextRnd)
      }

      override def canonicals: LazyListOrStream[RoseTree[Option[T]]] = {
        // The canonicals of Option[T] are the canonicals of T, plus None
        val tCanonicals = genOfT.canonicals
        LazyListOrStream(Rose(None: Option[T])) #::: tCanonicals.map(rt => rt.map(t => Some(t): Option[T]))
      }

      override def roseTreeOfEdge(edge: Option[T], sizeParam: SizeParam, isValidFun: (Option[T], SizeParam) => Boolean): RoseTree[Option[T]] = NextRoseTree(edge, sizeParam, isValidFun)

      def nextImpl(szp: SizeParam, isValidFun: (Option[T], SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Option[T]], Randomizer) = {
        val (nextInt, nextRnd) = rnd.nextInt
        if (nextInt % 100 == 0) // let every hundredth value or so be a None
          (Rose(None), nextRnd) // No need to shrink None.
        else {
          val (nextRoseTreeOfT, _, nextNextRnd) = genOfT.next(szp, Nil, nextRnd)
          val nextT = nextRoseTreeOfT.value
          (NextRoseTree(Some(nextT), szp, isValid), nextNextRnd)
        }
      }
      override def toString = "Generator[Option[T]]"
      override def shrinksForValue(valueToShrink: Option[T]): Option[LazyListOrStream[RoseTree[Option[T]]]] = Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
    }
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

      case class NextRoseTree(value: G Or B, sizeParam: SizeParam, isValidFun: (G Or B, SizeParam) => Boolean) extends RoseTree[G Or B] {
        def shrinks: LazyListOrStream[RoseTree[G Or B]] = {
          value match {
            case Good(g) =>

              val nestedRoseTreesOpt: Option[LazyListOrStream[RoseTree[G]]] = genOfG.shrinksForValue(g)
              nestedRoseTreesOpt match {
                case Some(nestedRoseTrees) =>
                  val nestedList: LazyListOrStream[RoseTree[G Or B]] =
                    nestedRoseTrees.map(nrt => nrt.map(t => Good(t)))
                      .filter(rt => isValidFun(rt.value, sizeParam)) 
                  nestedList #::: LazyListOrStream.empty[RoseTree[G Or B]]
                case None =>
                  // If the shrinksForValue lazy list is empty, degrade to canonicals.
                  val canonicalGs = genOfG.canonicals
                  canonicalGs.map(rt => rt.map(t => Good(t))) 
                    .filter(rt => isValidFun(rt.value, sizeParam)) #::: LazyListOrStream.empty[RoseTree[G Or B]]
              }

            case Bad(b) =>

              val nestedRoseTreesOpt: Option[LazyListOrStream[RoseTree[B]]] = genOfB.shrinksForValue(b)
              nestedRoseTreesOpt match {
                case Some(nestedRoseTrees) =>
                  val nestedList: LazyListOrStream[RoseTree[G Or B]] =
                    nestedRoseTrees.map(nrt => nrt.map(t => Bad(t)))
                      .filter(rt => isValidFun(rt.value, sizeParam))
                  nestedList #::: LazyListOrStream.empty[RoseTree[G Or B]]
                case None =>
                  // If the shrinksForValue lazy list is empty, degrade to canonicals.
                  val canonicalBs = genOfB.canonicals
                  canonicalBs.map(rt => rt.map(t => Bad(t)))
                    .filter(rt => isValidFun(rt.value, sizeParam)) #::: LazyListOrStream.empty[RoseTree[G Or B]]
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

      override def canonicals: LazyListOrStream[RoseTree[G Or B]] = {
        val goodCanon = genOfG.canonicals
        val badCanon = genOfB.canonicals
        goodCanon.map(rt => rt.map(t => Good(t): G Or B)) #::: badCanon.map(rt => rt.map(t => Bad(t): G Or B))
      }

      override def roseTreeOfEdge(edge: G Or B, sizeParam: SizeParam, isValidFun: (G Or B, SizeParam) => Boolean): RoseTree[G Or B] = NextRoseTree(edge, sizeParam, isValidFun)

      def nextImpl(szp: SizeParam, isValidFun: (G Or B, SizeParam) => Boolean, rnd: Randomizer): (RoseTree[G Or B], Randomizer) = {
        val (nextInt, nextRnd) = rnd.nextInt
        if (nextInt % 4 == 0) {
          val (nextRoseTreeOfB, _, nextRnd) = genOfB.next(szp, Nil, rnd)
          (nextRoseTreeOfB.map(b => Bad(b)), nextRnd)
        }
        else {
          val (nextRoseTreeOfG, _, nextRnd) = genOfG.next(szp, Nil, rnd)
          (nextRoseTreeOfG.map(g => Good(g)), nextRnd)
        }
      }

      override def shrinksForValue(valueToShrink: G Or B): Option[LazyListOrStream[RoseTree[G Or B]]] =
        Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
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

      case class NextRoseTree(value: Either[L, R], sizeParam: SizeParam, isValidFun: (Either[L, R], SizeParam) => Boolean) extends RoseTree[Either[L, R]] {
        def shrinks: LazyListOrStream[RoseTree[Either[L, R]]] = {
          value match {
            case Left(l) =>
              val nestedRoseTreesOpt: Option[LazyListOrStream[RoseTree[L]]] = genOfL.shrinksForValue(l)
              nestedRoseTreesOpt match {
                case Some(nestedRoseTrees) =>
                  val nestedList: LazyListOrStream[RoseTree[Either[L, R]]] =
                    nestedRoseTrees.map(nrt => nrt.map(t => Left(t)))
                      .filter(rt => isValidFun(rt.value, sizeParam))
                  nestedList #::: LazyListOrStream.empty[RoseTree[Either[L, R]]]
                case None =>
                  // If the shrinksForValue lazy list is empty, degrade to canonicals.
                  val canonicalGs = genOfL.canonicals
                  canonicalGs.map(rt => rt.map(t => Left(t)))
                    .filter(rt => isValidFun(rt.value, sizeParam)) #::: LazyListOrStream.empty[RoseTree[Either[L, R]]]
              }

            case Right(r) =>
              val nestedRoseTreesOpt: Option[LazyListOrStream[RoseTree[R]]] = genOfR.shrinksForValue(r)
              nestedRoseTreesOpt match {
                case Some(nestedRoseTrees) =>
                  val nestedList: LazyListOrStream[RoseTree[Either[L, R]]] =
                    nestedRoseTrees.map(nrt => nrt.map(t => Right(t)))
                      .filter(rt => isValidFun(rt.value, sizeParam))
                  nestedList #::: LazyListOrStream.empty[RoseTree[Either[L, R]]]
                case None =>
                  // If the shrinksForValue lazy list is empty, degrade to canonicals.
                  val canonicalBs = genOfR.canonicals
                  canonicalBs.map(rt => rt.map(t => Right(t))) 
                    .filter(rt => isValidFun(rt.value, sizeParam)) #::: LazyListOrStream.empty[RoseTree[Either[L, R]]]
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

      override def canonicals: LazyListOrStream[RoseTree[Either[L, R]]] = {
        val rightCanon = genOfR.canonicals
        val leftCanon = genOfL.canonicals
        rightCanon.map(rt => rt.map(t => Right(t))) #::: leftCanon.map(rt => rt.map(t => Left(t))) #::: LazyListOrStream.empty[RoseTree[Either[L, R]]]
      }

      override def roseTreeOfEdge(edge: Either[L, R], sizeParam: SizeParam, isValidFun: (Either[L, R], SizeParam) => Boolean): RoseTree[Either[L, R]] = NextRoseTree(edge, sizeParam, isValidFun)

      def nextImpl(szp: SizeParam, isValidFun: (Either[L, R], SizeParam) => Boolean, rnd: Randomizer): (RoseTree[Either[L, R]], Randomizer) = {
        val (nextInt, nextRnd) = rnd.nextInt
        if (nextInt % 4 == 0) {
          // TODO: Here I was not sure if I should just map the RoseTree or takes
          // its value and wrap that in a shrink call. Might be the same thing ultimately.
          // Will check that later. Actually I'll try mapping first.
          val (nextRoseTreeOfL, _, nextRnd) = genOfL.next(szp, Nil, rnd)
          (nextRoseTreeOfL.map(l => Left(l)), nextRnd)
        }
        else {
          val (nextRoseTreeOfR, _, nextRnd) = genOfR.next(szp, Nil, rnd)
          (nextRoseTreeOfR.map(r => Right(r)), nextRnd)
        }
      }

      override def shrinksForValue(valueToShrink: Either[L, R]): Option[LazyListOrStream[RoseTree[Either[L, R]]]] =
        Some(NextRoseTree(valueToShrink, SizeParam(1, 0, 1), isValid).shrinks)
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
    new Generator[Vector[T]] with HavingLength[Vector[T]] { thisGen =>

      case class NextRoseTree(value: Vector[T], sizeParam: SizeParam, isValidFun: (Vector[T], SizeParam) => Boolean) extends RoseTree[Vector[T]] {
        def shrinks: LazyListOrStream[RoseTree[Vector[T]]] = {
          def resLazyListOrStream(theValue: Vector[T]): LazyListOrStream[RoseTree[Vector[T]]] = {
            if (theValue.isEmpty)
              LazyListOrStream.empty
            else if (theValue.length == 1) {
              if (isValidFun(Vector.empty, sizeParam))
                Rose(Vector.empty) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty  
            }
            else {
              val halfSize = theValue.length / 2
              val firstHalf = theValue.take(halfSize)
              val secondHalf = theValue.drop(halfSize)
              // If value has an odd number of elements, the second half will be one character longer than the first half.
              LazyListOrStream(secondHalf, firstHalf).filter(v => isValidFun(v, sizeParam))
                                                     .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(firstHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[Vector[T]] =
        new Generator[Vector[T]] {

          def nextImpl(ignoredSzp: org.scalatest.prop.SizeParam, isValidFun: (Vector[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: Vector[T], rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], org.scalatest.prop.Randomizer) =
              if (result.length == targetSize)
                (NextRoseTree(result, ignoredSzp, isValid), rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfT.next(szp, List.empty, rnd)
                loop(targetSize, result :+ nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, Vector.empty, nextRnd)
          }

          override def isValid(value: Vector[T], size: SizeParam): Boolean = value.length >= szp.minSize.value && value.size <= (szp.minSize.value + szp.sizeRange.value)
        }

      def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (Vector[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], org.scalatest.prop.Randomizer) = {
        val gen = generatorWithSize(szp)
        gen.nextImpl(szp, isValidFun, rnd)
      }

      override def canonicals: LazyListOrStream[RoseTree[Vector[T]]] = {
        val canonicalsOfT = genOfT.canonicals
        canonicalsOfT.map(rt => rt.map(t => Vector(t)))
      }

      // Members declared in org.scalatest.prop.HavingSize
      def havingSize(len: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Vector[T]] = generatorWithSize(SizeParam(len, 0, len))
      def havingSizesBetween(from: org.scalactic.anyvals.PosZInt,to: org.scalactic.anyvals.PosZInt): org.scalatest.prop.Generator[Vector[T]] = {
        require(from != to, Resources.fromEqualToToHavingSizesBetween(from))
        require(from < to, Resources.fromGreaterThanToHavingSizesBetween(from, to))
        generatorWithSize(SizeParam(from, PosZInt.ensuringValid(to - from), from))
      }
      def havingSizesDeterminedBy(f: org.scalatest.prop.SizeParam => org.scalatest.prop.SizeParam): org.scalatest.prop.Generator[Vector[T]] =
        new Generator[Vector[T]] {
          def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (Vector[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Vector[T]], org.scalatest.prop.Randomizer) = {
            val s = f(szp)
            val gen = generatorWithSize(s)
            gen.nextImpl(s, isValid, rnd)
          }
          override def isValid(value: Vector[T], sizeParam: SizeParam): Boolean = {
            val fSizeParam = f(sizeParam)
            value.length >= fSizeParam.minSize.value && value.length <= (fSizeParam.minSize.value + fSizeParam.sizeRange.value)
          }
        }

      override def shrinksForValue(valueToShrink: Vector[T]): Option[LazyListOrStream[RoseTree[Vector[T]]]] = Some(NextRoseTree(valueToShrink, SizeParam(0, 0, 0), isValid).shrinks)
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

      case class NextRoseTree(value: Set[T], sizeParam: SizeParam, isValidFun: (Set[T], SizeParam) => Boolean) extends RoseTree[Set[T]] {
        def shrinks: LazyListOrStream[RoseTree[Set[T]]] = {
          def resLazyListOrStream(theValue: Set[T]): LazyListOrStream[RoseTree[Set[T]]] = {
            if (theValue.isEmpty)
              LazyListOrStream.empty
            else if (theValue.size == 1) {
              if (isValidFun(Set.empty, sizeParam))
                Rose(Set.empty[T]) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty
            }
            else {
              val halfSize = theValue.size / 2
              val firstHalf = theValue.take(halfSize)
              val secondHalf = theValue.drop(halfSize)
              // If value has an odd number of elements, the second half will be one character longer than the first half.
              LazyListOrStream(secondHalf, firstHalf).filter(v => isValidFun(v, sizeParam))
                                                     .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(firstHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[Set[T]] =
        new Generator[Set[T]] {

          def nextImpl(ignoredSzp: org.scalatest.prop.SizeParam, isValidFun: (Set[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: Set[T], rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result, ignoredSzp, isValid), rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfT.next(szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, Set.empty, nextRnd)
          }

          override def isValid(value: Set[T], size: SizeParam): Boolean = value.size >= szp.minSize.value && value.size <= (szp.minSize.value + szp.sizeRange.value)
        }

      def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (Set[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], org.scalatest.prop.Randomizer) = {
        val gen = generatorWithSize(szp)
        gen.nextImpl(szp, isValidFun, rnd)
      }

      override def canonicals: LazyListOrStream[RoseTree[Set[T]]] = {
        val canonicalsOfT = genOfT.canonicals
        canonicalsOfT.map(rt => rt.map(t => Set(t)))
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
          def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (Set[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Set[T]], org.scalatest.prop.Randomizer) = {
            val s = f(szp)
            val gen = generatorWithSize(s)
            gen.nextImpl(s, isValidFun, rnd)
          }
          override def isValid(value: Set[T], sizeParam: SizeParam): Boolean = {
            val fSizeParam = f(sizeParam)
            value.size >= fSizeParam.minSize.value && value.size <= (fSizeParam.minSize.value + fSizeParam.sizeRange.value)
          }
        }

      override def shrinksForValue(valueToShrink: Set[T]): Option[LazyListOrStream[RoseTree[Set[T]]]] = Some(NextRoseTree(valueToShrink, SizeParam(0, 0, 0), isValid).shrinks)
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

      case class NextRoseTree(value: SortedSet[T], sizeParam: SizeParam, isValidFun: (SortedSet[T], SizeParam) => Boolean) extends RoseTree[SortedSet[T]] {
        def shrinks: LazyListOrStream[RoseTree[SortedSet[T]]] = {
          def resLazyListOrStream(theValue: SortedSet[T]): LazyListOrStream[RoseTree[SortedSet[T]]] = {
            if (theValue.isEmpty)
              LazyListOrStream.empty
            else if (theValue.size == 1) {
              if (isValidFun(SortedSet.empty, sizeParam))
                Rose(SortedSet.empty[T]) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty
            }
            else {
              val halfSize = theValue.size / 2
              val firstHalf = theValue.take(halfSize)
              val secondHalf = theValue.drop(halfSize)
              // If value has an odd number of elements, the second half will be one character longer than the first half.
              LazyListOrStream(secondHalf, firstHalf).filter(v => isValidFun(v, sizeParam))
                                                     .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(firstHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[SortedSet[T]] =
        new Generator[SortedSet[T]] {

          def nextImpl(ignoredSzp: org.scalatest.prop.SizeParam, isValidFun: (SortedSet[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: SortedSet[T], rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result, ignoredSzp, isValid), rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfT.next(szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, SortedSet.empty, nextRnd)
          }

          override def isValid(value: SortedSet[T], size: SizeParam): Boolean = value.size >= szp.minSize.value && value.size <= (szp.minSize.value + szp.sizeRange.value)
        }

      def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (SortedSet[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], org.scalatest.prop.Randomizer) = {
        val gen = generatorWithSize(szp)
        gen.nextImpl(szp, isValidFun, rnd)
      }

      override def canonicals: LazyListOrStream[RoseTree[SortedSet[T]]] = {
        val canonicalsOfT = genOfT.canonicals
        canonicalsOfT.map(rt => rt.map(t => SortedSet(t)))
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
          def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (SortedSet[T], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedSet[T]], org.scalatest.prop.Randomizer) = {
            val s = f(szp)
            val gen = generatorWithSize(s)
            gen.nextImpl(s, isValidFun, rnd)
          }
          override def isValid(value: SortedSet[T], sizeParam: SizeParam): Boolean = {
            val fSizeParam = f(sizeParam)
            value.size >= fSizeParam.minSize.value && value.size <= (fSizeParam.minSize.value + fSizeParam.sizeRange.value)
          }
        }

      override def shrinksForValue(valueToShrink: SortedSet[T]): Option[LazyListOrStream[RoseTree[SortedSet[T]]]] = Some(NextRoseTree(valueToShrink, SizeParam(0, 0, 0), isValid).shrinks)
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

      case class NextRoseTree(value: Map[K, V], sizeParam: SizeParam, isValidFun: (Map[K, V], SizeParam) => Boolean) extends RoseTree[Map[K, V]] {
        def shrinks: LazyListOrStream[RoseTree[Map[K, V]]] = {
          def resLazyListOrStream(theValue: Map[K, V]): LazyListOrStream[RoseTree[Map[K, V]]] = {
            if (theValue.isEmpty)
              LazyListOrStream.empty
            else if (theValue.size == 1) {
              if (isValidFun(Map.empty, sizeParam))
                Rose(Map.empty[K, V]) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty
            }
            else {
              val halfSize = theValue.size / 2
              val firstHalf = theValue.take(halfSize)
              val secondHalf = theValue.drop(halfSize)
              // If value has an odd number of elements, the second half will be one character longer than the first half.
              LazyListOrStream(secondHalf, firstHalf).filter(v => isValidFun(v, sizeParam))
                                                     .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(firstHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }


      def generatorWithSize(szp: SizeParam): Generator[Map[K, V]] =
        new Generator[Map[K, V]] {

          def nextImpl(ignoredSzp: org.scalatest.prop.SizeParam, isValidFun: (Map[K, V], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Map[K, V]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: Map[K, V], rnd: org.scalatest.prop.Randomizer): (RoseTree[Map[K, V]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result, ignoredSzp, isValid), rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfTuple2KV.next (szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, Map.empty, nextRnd)
          }

          override def isValid(value: Map[K, V], size: SizeParam): Boolean = value.size >= szp.minSize.value && value.size <= (szp.minSize.value + szp.sizeRange.value)
        }

      def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (Map[K, V], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): Tuple2[RoseTree[Map[K, V]], org.scalatest.prop.Randomizer] = {
        val gen = generatorWithSize(szp)
        gen.nextImpl(szp, isValidFun, rnd)
      }

      override def canonicals: LazyListOrStream[RoseTree[Map[K, V]]] = {
        val canonicalsOfKV = genOfTuple2KV.canonicals
        canonicalsOfKV.map(rt => rt.map(t => Map(t)))
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
          def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (Map[K, V], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[Map[K, V]], org.scalatest.prop.Randomizer) = {
            val s = f(szp)
            val gen = generatorWithSize(s)
            gen.nextImpl(s, isValidFun, rnd)
          }
          override def isValid(value: Map[K, V], sizeParam: SizeParam): Boolean = {
            val fSizeParam = f(sizeParam)
            value.size >= fSizeParam.minSize.value && value.size <= (fSizeParam.minSize.value + fSizeParam.sizeRange.value)
          }
        }

      override def shrinksForValue(valueToShrink: Map[K, V]): Option[LazyListOrStream[RoseTree[Map[K, V]]]] = Some(NextRoseTree(valueToShrink, SizeParam(0, 0, 0), isValid).shrinks)
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

      case class NextRoseTree(value: SortedMap[K, V], sizeParam: SizeParam, isValidFun: (SortedMap[K, V], SizeParam) => Boolean) extends RoseTree[SortedMap[K, V]] {
        def shrinks: LazyListOrStream[RoseTree[SortedMap[K, V]]] = {
          def resLazyListOrStream(theValue: SortedMap[K, V]): LazyListOrStream[RoseTree[SortedMap[K, V]]] = {
            if (theValue.isEmpty)
              LazyListOrStream.empty
            else if (theValue.size == 1) {
              if (isValidFun(SortedMap.empty, sizeParam))
                Rose(SortedMap.empty[K, V]) #:: LazyListOrStream.empty
              else
                LazyListOrStream.empty
            }
            else {
              val halfSize = theValue.size / 2
              val firstHalf = theValue.take(halfSize)
              val secondHalf = theValue.drop(halfSize)
              // If value has an odd number of elements, the second half will be one character longer than the first half.
              LazyListOrStream(secondHalf, firstHalf).filter(v => isValidFun(v, sizeParam))
                                                     .map(v => NextRoseTree(v, sizeParam, isValidFun)) #::: resLazyListOrStream(firstHalf)
            }
          }
          resLazyListOrStream(value)
        }
      }

      def generatorWithSize(szp: SizeParam): Generator[SortedMap[K, V]] =
        new Generator[SortedMap[K, V]] {

          def nextImpl(ignoredSzp: org.scalatest.prop.SizeParam, isValidFun: (SortedMap[K, V], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedMap[K, V]], org.scalatest.prop.Randomizer) = {
            @scala.annotation.tailrec
            def loop(targetSize: Int, result: SortedMap[K, V], rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedMap[K, V]], org.scalatest.prop.Randomizer) =
              if (result.size == targetSize)
                (NextRoseTree(result, ignoredSzp, isValid), rnd)
              else {
                val (nextRoseTreeOfT, nextEdges, nextRnd) = genOfTuple2KV.next (szp, List.empty, rnd)
                loop(targetSize, result + nextRoseTreeOfT.value, nextRnd)
              }

            val (size, nextRnd) = rnd.choosePosZInt(szp.minSize, szp.maxSize)
            loop(size.value, SortedMap.empty[K, V], nextRnd)
          }

          override def isValid(value: SortedMap[K, V], size: SizeParam): Boolean = value.size >= szp.minSize.value && value.size <= (szp.minSize.value + szp.sizeRange.value)
        }

      def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (SortedMap[K, V], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): Tuple2[RoseTree[SortedMap[K, V]], org.scalatest.prop.Randomizer] = {
        val gen = generatorWithSize(szp)
        gen.nextImpl(szp, isValidFun, rnd)
      }

      override def canonicals: LazyListOrStream[RoseTree[SortedMap[K, V]]] = {
        val canonicalsOfKV = genOfTuple2KV.canonicals
        canonicalsOfKV.map(rt => rt.map(t => SortedMap(t)))
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
          def nextImpl(szp: org.scalatest.prop.SizeParam, isValidFun: (SortedMap[K, V], SizeParam) => Boolean, rnd: org.scalatest.prop.Randomizer): (RoseTree[SortedMap[K, V]], org.scalatest.prop.Randomizer) = {
            val s = f(szp)
            val gen = generatorWithSize(s)
            gen.nextImpl(s, isValidFun, rnd)
          }
          override def isValid(value: SortedMap[K, V], sizeParam: SizeParam): Boolean = {
            val fSizeParam = f(sizeParam)
            value.size >= fSizeParam.minSize.value && value.size <= (fSizeParam.minSize.value + fSizeParam.sizeRange.value)
          }
        }

      override def shrinksForValue(valueToShrink: SortedMap[K, V]): Option[LazyListOrStream[RoseTree[SortedMap[K, V]]]] = Some(NextRoseTree(valueToShrink, SizeParam(0, 0, 0), isValid).shrinks)
    }
}


