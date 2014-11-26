
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

import org.scalatest._
import prop.GeneratorDrivenPropertyChecks._
import algebra._
import exceptions.TestFailedException

class OptionLawsSpec extends UnitSpec with CheckedEquality {

  class OptionFunctorProxy[T](underlying: Option[T]) extends FunctorProxy[Option, T] {
    def map[U](f: T => U): Option[U]  = underlying.map(f)
  }
  implicit object OptionFunctor extends Functor[Option] {
    def apply[T](opt: Option[T]): FunctorProxy[Option, T] = new OptionFunctorProxy[T](opt)
  }
  "Option's map method" should "obey the functor laws" in {
    def id[T] = (o: T) => o
    forAll { opt: Option[Int] =>
      opt.map(id) shouldEqual opt
    }
    forAll { (opt: Option[Int], f: Int => Int, g: Int => Int) =>
      (opt.map(g)).map(f) shouldEqual opt.map(f compose g)
    }
  }
  "A FunctorProxy[Option, T]'s map method" should "obey the functor laws" in {
    def id[T] = (o: T) => o
    forAll { opt: Option[Int] =>
      OptionFunctor(opt).map(id) shouldEqual opt
    }
    forAll { (opt: Option[Int], f: String => String, g: Int => String) =>
      OptionFunctor((OptionFunctor(opt).map(g))).map(f) shouldEqual OptionFunctor(opt).map(f compose g)
    }
  }

  import scala.language.higherKinds
  def id[T] = (o: T) => o

  def identity[Context[_], T](o: Context[T])(implicit functor: Functor[Context], equality: Equality[Context[T]]): Unit =
    functor(o).map(id) shouldEqual o

  def composite[Context[_], T, U, V](o: Context[T], f: T => U, g: U => V)(implicit functor: Functor[Context], equality: Equality[Context[V]]): Unit =
    functor((functor(o).map(f))).map(g) shouldEqual functor(o).map(g compose f) // g(f(x))

  "Option" should "obey the functor laws via its map method" in {
    forAll { (opt: Option[Int]) => identity(opt) }
    forAll { (opt: Option[Int], f: Int => String, g: String => String) => composite(opt, f, g) }
  }

  import org.scalacheck.Arbitrary
  import org.scalacheck.Shrink
  def assertObeysTheFunctorLaws[Context[_]](
      // wraps to gen
      implicit arbContextInt: Arbitrary[Context[Int]],
      // failure might have a list of 1000 keep shrinking till finds smallest, pass it in for every time.
      shrContextInt: Shrink[Context[Int]],
      arbIntToString: Arbitrary[Int => String],
      shrIntToString: Shrink[Int => String],
      // testing for the laws, composition
      arbStringToChar: Arbitrary[String => Char],
      shrStringToChar: Shrink[Context[String => Char]],
      functor: Functor[Context],
      equalityOfContextOfInt: Equality[Context[Int]],
      equalityOfContextOfChar: Equality[Context[Char]]): Unit = {
    forAll { (opt: Context[Int]) => identity(opt) }
    forAll { (opt: Context[Int], f: Int => String, g: String => Char) => composite(opt, f, g) }
  }

  it should "obey the functor laws via its map method more generically" in {
    assertObeysTheFunctorLaws[Option]
  }

  "Or" should "obey the functor laws via its badMap method" in {

    trait OrWithGood[G] {
      type AndBad[B] = G Or B
    }

    class BadOrFunctorProxy[G, B](underlying: G Or B) extends FunctorProxy[OrWithGood[G]#AndBad, B] {
      def map[C](f: B => C): G Or C  = underlying.badMap(f)
    }

    implicit def badOrFunctor[G]: Functor[OrWithGood[G]#AndBad] =
      new Functor[OrWithGood[G]#AndBad] {
        def apply[B](opt: G Or B): FunctorProxy[OrWithGood[G]#AndBad, B] = new BadOrFunctorProxy[G, B](opt)
      }

    import org.scalacheck.Gen
    implicit def orArb[G, B](implicit arbG: Arbitrary[G], arbB: Arbitrary[B]): Arbitrary[G Or B] =
      Arbitrary(
        for (either <- Arbitrary.arbEither[B, G].arbitrary) yield Or.from(either)
      )
    assertObeysTheFunctorLaws[OrWithGood[Int]#AndBad]

    // instancesOf[OrWithGood[Int]#AndBad] shouldObey theFunctorLaws
    // instancesOf[OrWithGood[Int]#AndBad] should obey the functorLaws
  }
  
   "Or" should "obey the functor laws via its good map method" in {

       trait OrWithBad[B] {
         type AndGood[G] = G Or B
       }

       class GoodOrFunctorProxy[G, B](underlying: G Or B) extends FunctorProxy[OrWithBad[B]#AndGood, G] {
         def map[C](f: G => C): C Or B = underlying.map(f)
       }

       implicit def goodOrFunctor[B]: Functor[OrWithBad[B]#AndGood] =
         new Functor[OrWithBad[B]#AndGood] {
           def apply[G](or: G Or B): FunctorProxy[OrWithBad[B]#AndGood, G] = new GoodOrFunctorProxy[G, B](or)
         }

       import org.scalacheck.Gen
       implicit def orArb[G, B](implicit arbG: Arbitrary[G], arbB: Arbitrary[B]): Arbitrary[G Or B] =
         Arbitrary(
           for (either <- Arbitrary.arbEither[B, G].arbitrary) yield Or.from(either)
         )

         assertObeysTheFunctorLaws[OrWithBad[Int]#AndGood]
  }

/* Need ClassTag, but then don't implement the interface.
  "The map method of unmutated Arrays" should "obey the functor laws despite its unfortunate equals method" in {
    class ArrayFunctorProxy[T](underlying: Array[T]) extends FunctorProxy[Array, T] {
      def map[U](f: T => U): Array[U]  = underlying.map(f)
    }
    implicit object ArrayFunctor extends Functor[Array] {
      def apply[T](arr: Array[T]): FunctorProxy[Array, T] = new ArrayFunctorProxy[T](arr)
    }
    assertObeysTheFunctorLaws[Array]
  }
*/

  "The obey functor laws syntax" should "take an Equality" in {
    class ListFunctorProxy[T](underlying: List[T]) extends FunctorProxy[List, T] {
      def map[U](f: T => U): List[U]  = underlying.map(f)
    }
    implicit object ListFunctor extends Functor[List] {
      def apply[T](arr: List[T]): FunctorProxy[List, T] = new ListFunctorProxy[T](arr)
    }
    implicit def listEq[T]: Equality[List[T]] =
      new Equality[List[T]] {
        def areEqual(a: List[T], b: Any): Boolean =
          b match {
            case bAnyRef: AnyRef => a eq bAnyRef
            case _ => false
          }
      }
    a [TestFailedException] should be thrownBy {
      assertObeysTheFunctorLaws[List]
    }
  }
  "The map method of List" should "obey the functor laws" in {
    class ListFunctorProxy[T](underlying: List[T]) extends FunctorProxy[List, T] {
      def map[U](f: T => U): List[U]  = underlying.map(f)
    }
    implicit object ListFunctor extends Functor[List] {
      def apply[T](arr: List[T]): FunctorProxy[List, T] = new ListFunctorProxy[T](arr)
    }
    assertObeysTheFunctorLaws[List]
    // assertObeys[Option](functorLaws)
    // assertObeys[Option](functorLawsUsing[Int, String, Char])
  }
}


