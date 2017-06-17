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
package org.scalactic.choices

import org.scalatest._
import org.scalactic._
import scala.util.Failure
import scala.util.Success
import scala.util.Try
// SKIP-SCALATESTJS-START
import SharedHelpers.serializeRoundtrip
// SKIP-SCALATESTJS-END

/*
val x: (Int Choice String) Or ErrorMessage

val x: (Int Choice String) Or ErrorMessage

val x = West(3).elseEast[String]
val y = West[Int].elseEast("III")


val x = West(3).elseEast[String]
val y = West[Int].elseEast("III")

val x = West(3).elseEast[String]
val y = West[Int].elseEast("III")

val x = West(3).elseEast[String]
val y = West[Int].elseEast("III")

num match {
  case West(e) => e + 10
  case East(w) => w.length
}

num match {
  case West(e) => e + 10
  case East(w) => w.length
}

YinYang[B, W]
*/

class ChoiceSpec extends UnitSpec with Accumulation with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "A Choice" can "be either West or East" in {
    West(7).isWest shouldBe true
    West(7).isEast shouldBe false
    East("oops").isEast shouldBe true
    East("oops").isWest shouldBe false

    West(7) shouldBe a [Choice[_, _]]
    West(7) shouldBe a [West[_]]

    East("oops") shouldBe a [Choice[_, _]]
    East("oops") shouldBe an [East[_]]
  }
  it can "have its non-inferred type widened by an apply call with a type param" in {
    /*
      scala> West[Int].elseEast("hi")
      res0: org.scalautils.East[Int,String] = East(hi)

      scala> West(3).elseEast[String]
      res1: org.scalautils.West[Int,String] = West(3)

      scala> West(3).elseEast[ErrorMessage]
      res2: org.scalautils.West[Int,org.scalautils.ErrorMessage] = West(3)

      scala> West(3).elseEast("oops")
      <console>:11: error: type mismatch;
       found   : String("oops")
       required: <:<[Nothing,?]
                    West(3).elseEast("oops")
                                  ^

      scala> West[Int].elseEast[String]
      <console>:11: error: missing arguments for method elseEast in class WestieWestieGumdrop;
      follow this method with `_' if you want to treat it as a partially applied function
                    West[Int].elseEast[String]
                                   ^
    */
    // If the expected type is known, then you can just say West or East:
    West(3) shouldBe West(3)
    East("oops") shouldBe East("oops")

    // But if the expected type is not known, the inferred type of the other side will be Nothing:
    // West(3) will be a West[Int, Nothing]
    // East("oops") will be a East[Nothing, String]

    // If you want to specify a more specific type than Nothing, you can use this syntax:
    West(3).elseEast[String] shouldBe West(3)
    West[Int].elseEast("oops") shouldBe East("oops")

    // You could also do it this way:
    West[Int](3) shouldBe West(3)
    East[String]("oops") shouldBe East("oops")

    // But that requires that you also give a type that would be inferred from the value. This
    // would only be necessary if you wanted a more general type than that which
    // would otherwise be inferred from the given value, such as:
    West[AnyVal](3) shouldBe West(3)
    East[AnyRef]("oops") shouldBe East("oops")

    // In that case, though, I recommend a type ascription, because I think it is easier to read:
    (West(3): AnyVal Choice String) shouldBe West(3)
    (East("oops"): Int Choice AnyRef) shouldBe East("oops")
  }
  it can "be used in infix notation" in {
    def div(a: Int, b: Int): Int Choice ArithmeticException = {
      try West(a / b)
      catch { case ae: ArithmeticException => East(ae) }
      if (b == 0)
        East(new ArithmeticException("/ by zero"))
      else
        West(a / b)
    }
    div(1, 1) shouldEqual West(1)
    div(6, 2) shouldEqual West(3)
    div(6, 2) shouldEqual West(3)
    div(1, 0).isEast shouldBe true
    val ae = div(1, 0) match {
      case East(ae) => ae
      case result => fail("didn't get an East" + result)
    }
    ae should have message "/ by zero"
  }
  it can "be used with westMap" in {
    West(8) westMap (_ + 1) should equal (West(9))
    West[Int].elseEast("eight") westMap (_ + 1) should equal (East("eight"))
  }
  it can "be used with eastMap" in {
    West(8).elseEast[ErrorMessage] eastMap (_.toUpperCase) should equal (West(8))
    West[Int].elseEast("eight") eastMap (_.toUpperCase) should equal (East("EIGHT"))
  }
  it can "be used with transform" in {
    West(12).elseEast[String].transform((i: Int) => West(i + 1), (s: String) => East(s.toUpperCase)) should === (West(13))
    West[Int].elseEast("hi").transform((i: Int) => West(i + 1), (s: String) => East(s.toUpperCase)) should === (East("HI"))
    West(12).elseEast[String].transform((i: Int) => East(i + 1), (s: String) => West(s.toUpperCase)) should === (East(13))
    West[Int].elseEast("hi").transform((i: Int) => East(i + 1), (s: String) => West(s.toUpperCase)) should === (West("HI"))
  }
  it can "be used with swap" in {
    West(12).elseEast[String].swap should === (West[String].elseEast(12))
    West[Int].elseEast("hi").swap should === (West("hi").elseEast[Int])
  }
  it can "be folded with fold" in {
    West(3).elseEast[String].fold(_ + 1, _.length) shouldBe 4
    West[Int].elseEast("howdy").fold(_ + 1, _.length) shouldBe 5
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(West(12)) shouldBe West(12)
    serializeRoundtrip(East("twelve")) shouldBe East("twelve")
  }
  // SKIP-SCALATESTJS-END
  "The Choice companion" should "offer a concise type lambda syntax" in {
    import scala.language.higherKinds
    trait Functor[Context[_]] {
      def map[A, B](ca: Context[A])(f: A => B): Context[B]
    }
    class WestChoiceFunctor[WHITE] extends Functor[Choice.E[WHITE]#W] {
      override def map[B, C](ca: B Choice WHITE)(f: B => C): C Choice WHITE = ???
    }
    class EastChoiceFunctor[BLACK] extends Functor[Choice.W[BLACK]#E] {
      override def map[W, X](ca: BLACK Choice W)(f: W => X): BLACK Choice X = ???
    }
  }
}

