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
package org.scalatest

import org.scalactic.anyvals._

import prop._

/**
  * Scalatest support for Property-based testing.
  *
  * ==Introduction to Property-based Testing==
  *
  * In traditional unit testing, you write tests that describe precisely what the test will do:
  * create these objects, wire them together, call these functions, assert on the results, and
  * so on. It is clear and deterministic, but also limited, because it only covers the
  * exact situations you think to test. In most cases, it is not feasible to test all of the possible
  * combinations of data that might arise in real-world use.
  *
  * Property-based testing works the other way around. You describe ''properties'' -- rules that
  * you expect your classes to live by -- and describe how to test those properties. The test
  * system then generates relatively large amounts of synthetic data (with an emphasis on edge
  * cases that tend to make things break), so that you can see if the properties hold true in
  * these situations.
  *
  * As a result, property-based testing is scientific in the purest sense: you are stating a
  * hypothesis about how things should work (the property), and the system is trying to falsify
  * that hypothesis. If the tests pass, that doesn't ''prove'' the property holds, but it at least
  * gives you some confidence that you are probably correct.
  *
  * Property-based testing is deliberately a bit random: while the edge cases get tried upfront,
  * the system also usually generates a number of random values to try out. This makes things a
  * bit non-deterministic -- each run will be tried with somewhat different data. To make it
  * easier to debug, and to build regression tests, the system provides tools to re-run a failed
  * test with precisely the same data.
  *
  *
  * ==Background==
  *
  * '''TODO: Bill should insert a brief section on QuickCheck, ScalaCheck, etc, and how this
  * system is similar and different.'''
  *
  *
  * ==Using Property Checks==
  *
  * In order to use the tools described here, you should import this package:
  * {{{
  *   import org.scalatest._
  *   import org.scalatest.prop._
  * }}}
  *
  * This library is designed to work well with the types defined in Scalactic, and some functions take
  * types such as [[PosZInt]] as parameters. So it can also be helpful to import those with:
  * {{{
  *   import org.scalactic.anyvals._
  * }}}
  *
  * In order to call `forAll`, the function that actually
  * performs property checks, you will need to either extend or import GeneratorDrivenPropertyChecks,
  * like this:
  * {{{
  *   class DocExamples extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {
  * }}}
  * There's nothing special about [[FlatSpec]], though -- you may use any of ScalaTest's styles
  * with property checks. [[GeneratorDrivenPropertyChecks]] extends [[CommonGenerators]],
  * so it also provides access to the many utilities found there.
  *
  *
  * ==What Does a Property Look Like?==
  *
  * Let's check a simple property of Strings -- that if you concatenate a String to itself, its
  * length will be doubled:
  * {{{
  *   "Strings" should "have the correct length when doubled" in {
  *     forAll { (s: String) =>
  *       val s2 = s * 2
  *       s2.length should equal (s.length * 2)
  *     }
  *   }
  * }}}
  * (Note that the examples here are all using the [[FlatSpec]] style, but will work the same way
  * with any of ScalaTest's styles.)
  *
  * As the name of the tests suggests, the property we are testing is the length of a String that has
  * been doubled.
  *
  * The test begins with `forAll`. This is usually the way you'll want to begin property checks, and
  * that line can be read as, "For all Strings, the following should be true".
  *
  * The test harness will generate a number of Strings, with various contents and lengths. For each one,
  * we compute `s * 2`. (`*` is a function on String, which appends the String to itself as many times
  * as you specify.) And then we
  * check that the length of the doubled String is twice the length of the original one.
  *
  *
  * ==Using Specific Generators==
  *
  * Let's try a more general version of this test, multiplying arbitrary Strings by arbitrary multipliers:
  * {{{
  *   "Strings" should "have the correct length when multiplied" in {
  *     forAll { (s: String, n: PosZInt) =>
  *       val s2 = s * n.value
  *       s2.length should equal (s.length * n.value)
  *     }
  *   }
  * }}}
  * Again, you can read the first line of the test as "For all Strings, and all non-negative Integers,
  * the following should be true". ([[PosZInt]] is a type defined in Scalactic, which can be any positive
  * integer, including zero. It is appropriate to use here, since multiplying a String by a negative number
  * doesn't make sense.)
  *
  * This intuitively makes sense, but when we try to run it, we get a JVM Out of Memory error! Why? Because
  * the test system tries to test with the "edge cases" first, and one of the more important edge cases
  * is [[Int.MaxValue]]. It is trying to multiply a String by that, which is far larger than the memory
  * of even a big computer, and crashing.
  *
  * So we want to constrain our test to sane values of `n`, so that it doesn't crash. We can do this by
  * using more specific '''Generators'''.
  *
  * When we write a `forAll` test like the above, ScalaTest has to generate the values to be tested -- the
  * semi-random Strings, Ints and other types that you are testing. It does this by calling on an implicit
  * [[Generator]] for the desired type. The Generator generates values to test, starting with the edge cases
  * and then moving on to randomly-selected values.
  *
  * ScalaTest has built-in Generators for many major types, including String and PosZInt, but these Generators
  * are generic: they will try ''any'' value, including values that can break your test, as shown above. But
  * it also provides tools to let you be more specific.
  *
  * Here is the fixed version of the above test:
  * {{{
  *   "Strings" should "have the correct length when multiplied" in {
  *     forAll(strings, posZIntsBetween(0, 1000))
  *     { (s: String, n: PosZInt) =>
  *       val s2 = s * n.value
  *       s2.length should equal (s.length * n.value)
  *     }
  *   }
  * }}}
  * This is using a variant of forAll, which lets you specify the Generators to use instead of
  * just picking the implicit one. [[CommonGenerators.strings]] is the built-in Generator for
  * Strings, the same one you were getting implicitly. (The other built-ins can be found in
  * [[CommonGenerators]]. They are mixed into [[GeneratorDrivenPropertyChecks]], so they
  * are readily available.)
  *
  * But [[CommonGenerators.posZIntsBetween]] is a function that
  * ''creates'' a Generator that selects from the given values. In this case, it will create
  * a Generator that only creates numbers from 0 to 1000 -- small enough to not blow up our
  * computer's memory. If you try this test, this runs correctly.
  *
  * The moral of the story is that, while using the built-in Generators is very convenient,
  * and works most of the time, you should think about the data you are trying to test, and
  * pick or create a more-specific [[Generator]] when the test calls for it.
  *
  * [[CommonGenerators]] contains many functions that are helpful in common cases. In particular:
  *
  *   - `xxsBetween` (where `xxs` might be Int, Long, Float or most other significant numeric types)
  *     gives you a value of the desired type in the given range, as in the `posZIntsBetween()` example
  *     above.
  *   - [[CommonGenerators.specificValue]] and [[CommonGenerators.specificValues]] create Generators
  *     that produce either one specific value every time, or one of several values randomly. This is
  *     useful for enumerations and types that behave like enumerations.
  *   - [[CommonGenerators.evenly]] and [[CommonGenerators.frequency]] create higher-level
  *     Generators that call other Generators, either more or less equally or with a distribution
  *     you define.
  *
  *
  * ==Testing Your Own Types==
  *
  * Testing the built-in types isn't very interesting, though. Usually, you have your own
  * types that you want to check the properties of. So let's build up an example piece by piece.
  *
  * Say you have this simple type:
  * {{{
  *   sealed trait Shape {
  *     def area: Double
  *   }
  *   case class Rectangle(width: Int, height: Int) extends Shape {
  *     require(width > 0)
  *     require(height > 0)
  *     def area: Double = width * height
  *   }
  * }}}
  * Let's confirm a nice straightforward property that is surely true: that the area is greater than zero:
  * {{{
  *  "Rectangles" should "have a positive area" in {
  *     forAll { (w: PosInt, h: PosInt) =>
  *       val rect = Rectangle(w, h)
  *       rect.area should be > 0.0
  *     }
  *   }
  * }}}
  * Note that, even though our class takes ordinary Ints as parameters (and checks the values at runtime),
  * it is actually easier to generate the legal values using Scalactic's [[PosInt]] type.
  *
  * This should work, right? Actually, it doesn't -- if we run it a few times, we quickly hit an error!
  * {{{
  * [info] Rectangles
  * [info] - should have a positive area *** FAILED ***
  * [info]   GeneratorDrivenPropertyCheckFailedException was thrown during property evaluation.
  * [info]    (DocExamples.scala:42)
  * [info]     Falsified after 2 successful property evaluations.
  * [info]     Location: (DocExamples.scala:42)
  * [info]     Occurred when passed generated values (
  * [info]       None = PosInt(399455539),
  * [info]       None = PosInt(703518968)
  * [info]     )
  * [info]     Init Seed: 1568878346200
  * }}}
  * '''TODO:''' fix the above error to reflect the better errors we should get when we merge in
  * the code being forward-ported from 3.0.5.
  *
  * Looking at it, we can see that the numbers being used are pretty large. What happens when we
  * multiply them together?
  * {{{
  * scala> 399455539 * 703518968
  * res0: Int = -2046258840
  * }}}
  * We're hitting an Int overflow problem here: the numbers are too big to multiply together and
  * still get an Int. So we have to fix our `area` function:
  * {{{
  *   case class Rectangle(width: Int, height: Int) extends Shape {
  *     require(width > 0)
  *     require(height > 0)
  *     def area: Double = width.toLong * height.toLong
  *   }
  * }}}
  * Now, when we run our property check, it consistently passes. Excellent -- we've caught a bug,
  * because ScalaTest tried sufficiently large numbers.
  *
  *
  * ===Composing Your Own Generators===
  *
  * Doing things as shown above works, but having to generate the parameters and construct a
  * `Rectangle` every time is a nuisance. What we really want is to create our own [[Generator]]
  * that just hands us Rectangles, the same way we can do for `PosInt`. Fortunately, this
  * is easy.
  *
  * [[Generator]]s can be ''composed'' in `for` comprehensions. So we can create our own Generator
  * for Rectangle like this:
  * {{{
  *   implicit val rectGenerator = for {
  *     w <- posInts
  *     h <- posInts
  *   }
  *     yield Rectangle(w, h)
  * }}}
  * Taking that line by line:
  * {{{
  *     w <- posInts
  * }}}
  * [[CommonGenerators.posInts]] is the built-in Generator for positive Ints. So this line puts
  * a randomly-generated positive Int in `w`, and
  * {{{
  *     h <- posInts
  * }}}
  * this line puts another one in `h`. Finally, this line:
  * {{{
  *       yield Rectangle(w, h)
  * }}}
  * combines `w` and `h` to make a `Rectangle`.
  *
  * That's pretty much all you need in order to build any normal `case class` -- just build it
  * out of the Generators for the type of each field. (And if the fields are complex data
  * structures themselves, build Generators for them the same way, until you are just using
  * primitives.)
  *
  * Now, our property check becomes simpler:
  * {{{
  *  "Generated Rectangles" should "have a positive area" in {
  *     forAll { (rect: Rectangle) =>
  *       rect.area should be > 0.0
  *     }
  *   }
  * }}}
  * That's about as close to plain English as we can reasonably hope for!
  *
  *
  * ==Filtering Values with whenever()==
  *
  * Sometimes, not all of your generated values make sense for the property you want to
  * check -- you know (via external information) that some of these values will never come
  * up. In cases like this, you ''can'' create a custom [[Generator]] that only creates the
  * values you do want, but it's often easier to just use [[Whenever.whenever]].
  * ([[Whenever]] is mixed into [[GeneratorDrivenPropertyChecks]], so this is available
  * when you need it.)
  *
  * The [[Whenever.whenever]] function can be used inside of [[GeneratorDrivenPropertyChecks.forAll]]. It says that only the
  * filtered values should be used, and anything else should be discarded. For example,
  * look at this property:
  * {{{
  *   "Fractions" should "get smaller when squared" in {
  *     forAll { (n: Float) =>
  *       whenever(n > 0 && n < 1) {
  *         (n * n) should be < n
  *       }
  *     }
  *   }
  * }}}
  * We are testing a property of numbers less than 1, so we filter away everything that
  * is ''not'' the numbers we want. This property check succeeds, because we've screened
  * out the values that would make it fail.
  *
  * ===Discard Limits===
  *
  * You shouldn't push [[Whenever.whenever]] too far, though. This system is all about trying random
  * data, but if too much of the random data simply isn't usable, you can't get valid
  * answers, and the system tracks that.
  *
  * For example, consider this apparently-reasonable test:
  * {{{
  *   "Space Chars" should "not also be letters" in {
  *     forAll { (c: Char) =>
  *       whenever (c.isSpaceChar) {
  *         assert(!c.isLetter)
  *       }
  *     }
  *   }
  * }}}
  * Although the property is true, this test will fail with an error like this:
  * {{{
  * [info] Lowercase Chars
  * [info] - should upper-case correctly *** FAILED ***
  * [info]   Gave up after 0 successful property evaluations. 49 evaluations were discarded.
  * [info]   Init Seed: 1568855247784
  * }}}
  * Because the vast majority of [[Char]]s are not spaces, nearly all of the generated values
  * are being discarded. As a result, the system gives up after a while. In cases like this,
  * you usually should write a custom Generator instead.
  *
  * The proportion of how many discards to permit, relative to the number of successful
  * checks, is configuration-controllable. See [[GeneratorDrivenPropertyChecks]] for more details.
  *
  *
  * ==Randomization==
  *
  * The point of [[Generator]] is to create pseudo-random values for checking properties. But
  * it turns out to be very inconvenient if those values are ''actually'' random -- that
  * would mean that, when a property check fails occasionally, you have no good way to invoke that
  * specific set of circumstances again for debugging. We want "randomness", but we also want
  * it to be deterministic, and reproducible when you need it.
  *
  * To support this, all "randomness" in ScalaTest's property checking system uses the
  * [[Randomizer]] class. You start by creating a [[Randomizer]] using an initial
  * seed value, and call that to get your "random" value. Each call to a [[Randomizer]]
  * function returns a new [[Randomizer]], which you should use to fetch the next value.
  *
  * [[GeneratorDrivenPropertyChecks.forAll]] uses [[Randomizer]] under the hood: each time
  * you run a `forAll`-based test, it will automatically create a new [[Randomizer]],
  * which by default is seeded based on the current system time. You can override this,
  * as discussed below.
  *
  * Since [[Randomizer]] is actually deterministic (the "random" values are unobvious, but
  * will always be the same given the same initial seed), this means that re-running a test
  * with the same seed will produce the same values.
  *
  * If you need random data for your own [[Generator]]s and property checks, you should
  * use [[Randomizer]] in the same way; that way, your tests will also be re-runnable,
  * when needed for debugging.
  *
  *
  * ==Debugging, and Re-running a Failed Property Check==
  *
  * In '''Testing Your Own Types''' above, we found to our surprise that the property check
  * failed with this error:
  * {{{
  * [info] Rectangles
  * [info] - should have a positive area *** FAILED ***
  * [info]   GeneratorDrivenPropertyCheckFailedException was thrown during property evaluation.
  * [info]    (DocExamples.scala:42)
  * [info]     Falsified after 2 successful property evaluations.
  * [info]     Location: (DocExamples.scala:42)
  * [info]     Occurred when passed generated values (
  * [info]       None = PosInt(399455539),
  * [info]       None = PosInt(703518968)
  * [info]     )
  * [info]     Init Seed: 1568878346200
  * }}}
  * There must be a bug here -- but once we've fixed it, how can we make sure that we are
  * re-testing exactly the same case that failed?
  *
  * This is where the pseudo-random nature of [[Randomizer]] comes in, and why it is so
  * important to use it consistently. So long as all of our "random" data comes from that,
  * then all we need to do is re-run with the same seed.
  *
  * That's why the `Init Seed` shown in the message above is crucial. We can re-use that
  * seed -- and therefore get exactly the same "random" data -- by using the `-S` flag to
  * ScalaTest.
  *
  * So you can run this command in sbt to re-run exactly the same property check:
  * {{{
  *   testOnly *DocExamples -- -z "have a positive area" -S 1568878346200
  * }}}
  * Taking that apart:
  *
  *   - `testOnly *DocExamples` says that we only want to run suites whose paths end with `DocExamples`
  *   - `-z "have a positive area"` says to only run tests whose names include that string.
  *   - `-S 1568878346200` says to run all tests with a "random" seed of `1568878346200`
  *
  * By combining these flags, you can re-run exactly the property check you need, with the
  * right random seed to make sure you are re-creating the failed test. You should get
  * exactly the same failure over and over until you fix the bug, and then you can confirm
  * your fix with confidence.
  *
  *
  * ==Configuration==
  *
  * In general, `forAll()` works well out of the box. But you can tune several configuration parameters
  * when needed. See [[GeneratorDrivenPropertyChecks]] for info on how to set
  * configuration parameters for your test.
  *
  *
  * ==Table-Driven Properties==
  *
  * Sometimes, you want something in between traditional hard-coded unit tests and Generator-driven,
  * randomized tests. Instead, you sometimes want to check your properties against a specific set of
  * inputs.
  *
  * (This is particularly useful for regression tests, when you have found certain inputs that have
  * caused problems in the past, and want to make sure that they get consistently re-tested.)
  *
  * ScalaTest supports these, by mixing in [[TableDrivenPropertyChecks]]. See the documentation for
  * that class for the full details.
  */
package object prop {
  /**
    * Deterministically generate a value for the given Generator.
    *
    * This function takes a set of anywhere from 1-22 parameters, plus a "multiplier". It combines these to
    * generate a pseudo-random (but deterministic) seed, feeds that into the Generator, and returns the
    * result. Since the results are deterministic, calling this repeatedly with the same parameters will produce
    * the same output.
    *
    * This is mainly helpful when generating random Functions -- since the inputs for a test run are
    * complex, you need more than a simple random seed to reproduce the same results. In order to make
    * this more useful, the `toString` of a instance of a Function [[Generator]] shows how to invoke
    * `valueOf()` to reproduce the same result.
    *
    * @param first The first parameter to use for calculating the seed.
    * @param others Any additional parameters to use for calculating the seed.
    * @param multiplier A number to combine with the other parameters, to calculate the seed.
    * @param genOfA A Generator. (Usually a Function Generator.)
    * @tparam A The type of the Generator.
    * @return An instance of A, computed by feeding the calculated seed into the Generator.
    */
  def valueOf[A](first: Any, others: Any*)(multiplier: Int)(implicit genOfA: Generator[A]): A = {
    val combinedHashCode: Int =
      others.foldLeft(first.hashCode) { (acc, next) =>
        (37 * (acc + 37)) + next.hashCode
      }
    val seed = combinedHashCode.toLong * multiplier
    val rnd = Randomizer(seed)
    val maxSize = PosZInt(20)
    val (size, nextRnd) = rnd.choosePosZInt(1, maxSize) // size will be positive because between 1 and 20, inclusive
    val (roseTreeOfA, _, _) = genOfA.next(SizeParam(PosZInt(0), maxSize, size), Nil, nextRnd)
    roseTreeOfA.value
  }
}
