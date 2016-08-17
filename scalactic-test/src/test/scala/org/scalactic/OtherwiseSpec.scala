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
import scala.util.Failure
import scala.util.Success
import scala.util.Try
// SKIP-SCALATESTJS-START
import SharedHelpers.serializeRoundtrip
// SKIP-SCALATESTJS-END

/*
val x: (Int Otherwise String) Or ErrorMessage

val x: (Int Else String) Or ErrorMessage

val x = Black(3).elseWhite[String]
val y = Black[Int].elseWhite("III")


val x = Black(3).elseWhite[String]
val y = Black[Int].elseWhite("III")

val x = Black(3).otherwiseWhite[String]
val y = Black[Int].otherwiseWhite("III")

val x = Black(3).otherwiseWhite[String]
val y = Black[Int].otherwiseWhite("III")

num match {
  case Black(e) => e + 10
  case White(w) => w.length
}

num match {
  case Black(e) => e + 10
  case White(w) => w.length
}

YinYang[B, W]
*/

class OtherwiseSpec extends UnitSpec with Accumulation with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "An Otherwise" can "be either Black or White" in {
    Black(7).isBlack shouldBe true
    Black(7).isWhite shouldBe false
    White("oops").isWhite shouldBe true
    White("oops").isBlack shouldBe false

    Black(7) shouldBe an [Otherwise[_, _]]
    Black(7) shouldBe a [Black[_]]

    White("oops") shouldBe an [Otherwise[_, _]]
    White("oops") shouldBe an [White[_]]
  }
  it can "have its non-inferred type widened by an apply call with a type param" in {
    /*
      scala> Black[Int].otherwiseWhite("hi")
      res0: org.scalautils.White[Int,String] = White(hi)

      scala> Black(3).otherwiseWhite[String]
      res1: org.scalautils.Black[Int,String] = Black(3)

      scala> Black(3).otherwiseWhite[ErrorMessage]
      res2: org.scalautils.Black[Int,org.scalautils.ErrorMessage] = Black(3)

      scala> Black(3).otherwiseWhite("oops")
      <console>:11: error: type mismatch;
       found   : String("oops")
       required: <:<[Nothing,?]
                    Black(3).otherwiseWhite("oops")
                                  ^

      scala> Black[Int].otherwiseWhite[String]
      <console>:11: error: missing arguments for method otherwiseWhite in class BlackieBlackieGumdrop;
      follow this method with `_' if you want to treat it as a partially applied function
                    Black[Int].otherwiseWhite[String]
                                   ^
    */
    // If the expected type is known, then you can just say Black or White:
    Black(3) shouldBe Black(3)
    White("oops") shouldBe White("oops")

    // But if the expected type is not known, the inferred type of the other side will be Nothing:
    // Black(3) will be a Black[Int, Nothing]
    // White("oops") will be a White[Nothing, String]

    // If you want to specify a more specific type than Nothing, you can use this syntax:
    Black(3).otherwiseWhite[String] shouldBe Black(3)
    Black[Int].otherwiseWhite("oops") shouldBe White("oops")

    // You could also do it this way:
    Black[Int](3) shouldBe Black(3)
    White[String]("oops") shouldBe White("oops")

    // But that requires that you also give a type that would be inferred from the value. This
    // would only be necessary if you wanted a more general type than that which
    // would otherwise be inferred from the given value, such as:
    Black[AnyVal](3) shouldBe Black(3)
    White[AnyRef]("oops") shouldBe White("oops")

    // In that case, though, I recommend a type ascription, because I think it is easier to read:
    (Black(3): AnyVal Otherwise String) shouldBe Black(3)
    (White("oops"): Int Otherwise AnyRef) shouldBe White("oops")
  }
  it can "be used in infix notation" in {
    def div(a: Int, b: Int): Int Otherwise ArithmeticException = {
      try Black(a / b)
      catch { case ae: ArithmeticException => White(ae) }
      if (b == 0)
        White(new ArithmeticException("/ by zero"))
      else
        Black(a / b)
    }
    div(1, 1) shouldEqual Black(1)
    div(6, 2) shouldEqual Black(3)
    div(6, 2) shouldEqual Black(3)
    div(1, 0).isWhite shouldBe true
    val ae = div(1, 0) match {
      case White(ae) => ae
      case result => fail("didn't get an White" + result)
    }
    ae should have message "/ by zero"
  }
  it can "be used with transform" in {
    Black(12).otherwiseWhite[String].transform((i: Int) => Black(i + 1), (s: String) => White(s.toUpperCase)) should === (Black(13))
    Black[Int].otherwiseWhite("hi").transform((i: Int) => Black(i + 1), (s: String) => White(s.toUpperCase)) should === (White("HI"))
    Black(12).otherwiseWhite[String].transform((i: Int) => White(i + 1), (s: String) => Black(s.toUpperCase)) should === (White(13))
    Black[Int].otherwiseWhite("hi").transform((i: Int) => White(i + 1), (s: String) => Black(s.toUpperCase)) should === (Black("HI"))
  }
  it can "be used with swap" in {
    Black(12).otherwiseWhite[String].swap should === (Black[String].otherwiseWhite(12))
    Black[Int].otherwiseWhite("hi").swap should === (Black("hi").otherwiseWhite[Int])
  }
  it can "be folded with fold" in {
    Black(3).otherwiseWhite[String].fold(_ + 1, _.length) shouldBe 4
    Black[Int].otherwiseWhite("howdy").fold(_ + 1, _.length) shouldBe 5
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Black(12)) shouldBe Black(12)
    serializeRoundtrip(White("twelve")) shouldBe White("twelve")
  }
  // SKIP-SCALATESTJS-END
  "The Otherwise companion" should "offer a concise type lambda syntax" in {
    import scala.language.higherKinds
    trait Functor[Context[_]] {
      def map[A, B](ca: Context[A])(f: A => B): Context[B]
    }
    class BlackOtherwiseFunctor[WHITE] extends Functor[Otherwise.W[WHITE]#B] {
      override def map[B, C](ca: B Otherwise WHITE)(f: B => C): C Otherwise WHITE = ???
    }
    class WhiteOtherwiseFunctor[BLACK] extends Functor[Otherwise.B[BLACK]#W] {
      override def map[W, X](ca: BLACK Otherwise W)(f: W => X): BLACK Otherwise X = ???
    }
  }
}

