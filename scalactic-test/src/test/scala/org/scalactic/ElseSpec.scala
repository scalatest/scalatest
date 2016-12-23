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
val x: (Int Else String) Or ErrorMessage

val x: (Int Else String) Or ErrorMessage

val x = First(3).elseSecond[String]
val y = First[Int].elseSecond("III")


val x = First(3).elseSecond[String]
val y = First[Int].elseSecond("III")

val x = First(3).elseSecond[String]
val y = First[Int].elseSecond("III")

val x = First(3).elseSecond[String]
val y = First[Int].elseSecond("III")

num match {
  case First(e) => e + 10
  case Second(w) => w.length
}

num match {
  case First(e) => e + 10
  case Second(w) => w.length
}

YinYang[B, W]
*/

class ElseSpec extends UnitSpec with Accumulation with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "An Else" can "be either First or Second" in {
    First(7).isFirst shouldBe true
    First(7).isSecond shouldBe false
    Second("oops").isSecond shouldBe true
    Second("oops").isFirst shouldBe false

    First(7) shouldBe an [Else[_, _]]
    First(7) shouldBe a [First[_]]

    Second("oops") shouldBe an [Else[_, _]]
    Second("oops") shouldBe an [Second[_]]
  }
  it can "have its non-inferred type widened by an apply call with a type param" in {
    /*
      scala> First[Int].elseSecond("hi")
      res0: org.scalautils.Second[Int,String] = Second(hi)

      scala> First(3).elseSecond[String]
      res1: org.scalautils.First[Int,String] = First(3)

      scala> First(3).elseSecond[ErrorMessage]
      res2: org.scalautils.First[Int,org.scalautils.ErrorMessage] = First(3)

      scala> First(3).elseSecond("oops")
      <console>:11: error: type mismatch;
       found   : String("oops")
       required: <:<[Nothing,?]
                    First(3).elseSecond("oops")
                                  ^

      scala> First[Int].elseSecond[String]
      <console>:11: error: missing arguments for method elseSecond in class FirstieFirstieGumdrop;
      follow this method with `_' if you want to treat it as a partially applied function
                    First[Int].elseSecond[String]
                                   ^
    */
    // If the expected type is known, then you can just say First or Second:
    First(3) shouldBe First(3)
    Second("oops") shouldBe Second("oops")

    // But if the expected type is not known, the inferred type of the other side will be Nothing:
    // First(3) will be a First[Int, Nothing]
    // Second("oops") will be a Second[Nothing, String]

    // If you want to specify a more specific type than Nothing, you can use this syntax:
    First(3).elseSecond[String] shouldBe First(3)
    First[Int].elseSecond("oops") shouldBe Second("oops")

    // You could also do it this way:
    First[Int](3) shouldBe First(3)
    Second[String]("oops") shouldBe Second("oops")

    // But that requires that you also give a type that would be inferred from the value. This
    // would only be necessary if you wanted a more general type than that which
    // would otherwise be inferred from the given value, such as:
    First[AnyVal](3) shouldBe First(3)
    Second[AnyRef]("oops") shouldBe Second("oops")

    // In that case, though, I recommend a type ascription, because I think it is easier to read:
    (First(3): AnyVal Else String) shouldBe First(3)
    (Second("oops"): Int Else AnyRef) shouldBe Second("oops")
  }
  it can "be used in infix notation" in {
    def div(a: Int, b: Int): Int Else ArithmeticException = {
      try First(a / b)
      catch { case ae: ArithmeticException => Second(ae) }
      if (b == 0)
        Second(new ArithmeticException("/ by zero"))
      else
        First(a / b)
    }
    div(1, 1) shouldEqual First(1)
    div(6, 2) shouldEqual First(3)
    div(6, 2) shouldEqual First(3)
    div(1, 0).isSecond shouldBe true
    val ae = div(1, 0) match {
      case Second(ae) => ae
      case result => fail("didn't get an Second" + result)
    }
    ae should have message "/ by zero"
  }
  it can "be used with firstMap" in {
    First(8) firstMap (_ + 1) should equal (First(9))
    First[Int].elseSecond("eight") firstMap (_ + 1) should equal (Second("eight"))
  }
  it can "be used with secondMap" in {
    First(8).elseSecond[ErrorMessage] secondMap (_.toUpperCase) should equal (First(8))
    First[Int].elseSecond("eight") secondMap (_.toUpperCase) should equal (Second("EIGHT"))
  }
  it can "be used with transform" in {
    First(12).elseSecond[String].transform((i: Int) => First(i + 1), (s: String) => Second(s.toUpperCase)) should === (First(13))
    First[Int].elseSecond("hi").transform((i: Int) => First(i + 1), (s: String) => Second(s.toUpperCase)) should === (Second("HI"))
    First(12).elseSecond[String].transform((i: Int) => Second(i + 1), (s: String) => First(s.toUpperCase)) should === (Second(13))
    First[Int].elseSecond("hi").transform((i: Int) => Second(i + 1), (s: String) => First(s.toUpperCase)) should === (First("HI"))
  }
  it can "be used with swap" in {
    First(12).elseSecond[String].swap should === (First[String].elseSecond(12))
    First[Int].elseSecond("hi").swap should === (First("hi").elseSecond[Int])
  }
  it can "be folded with fold" in {
    First(3).elseSecond[String].fold(_ + 1, _.length) shouldBe 4
    First[Int].elseSecond("howdy").fold(_ + 1, _.length) shouldBe 5
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(First(12)) shouldBe First(12)
    serializeRoundtrip(Second("twelve")) shouldBe Second("twelve")
  }
  // SKIP-SCALATESTJS-END
  "The Else companion" should "offer a concise type lambda syntax" in {
    import scala.language.higherKinds
    trait Functor[Context[_]] {
      def map[A, B](ca: Context[A])(f: A => B): Context[B]
    }
    class FirstElseFunctor[WHITE] extends Functor[Else.W[WHITE]#B] {
      override def map[B, C](ca: B Else WHITE)(f: B => C): C Else WHITE = ???
    }
    class SecondElseFunctor[BLACK] extends Functor[Else.B[BLACK]#W] {
      override def map[W, X](ca: BLACK Else W)(f: W => X): BLACK Else X = ???
    }
  }
}

