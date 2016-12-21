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

val x = Port(3).elseStar[String]
val y = Port[Int].elseStar("III")


val x = Port(3).elseStar[String]
val y = Port[Int].elseStar("III")

val x = Port(3).elseStar[String]
val y = Port[Int].elseStar("III")

val x = Port(3).elseStar[String]
val y = Port[Int].elseStar("III")

num match {
  case Port(e) => e + 10
  case Star(w) => w.length
}

num match {
  case Port(e) => e + 10
  case Star(w) => w.length
}

YinYang[B, W]
*/

class ElseSpec extends UnitSpec with Accumulation with TypeCheckedTripleEquals {

  def isRound(i: Int): Validation[ErrorMessage] =
    if (i % 10 != 0) Fail(i + " was not a round number") else Pass

  def isDivBy3(i: Int): Validation[ErrorMessage] =
    if (i % 3 != 0) Fail(i + " was not divisible by 3") else Pass

  "An Else" can "be either Port or Star" in {
    Port(7).isPort shouldBe true
    Port(7).isStar shouldBe false
    Star("oops").isStar shouldBe true
    Star("oops").isPort shouldBe false

    Port(7) shouldBe an [Else[_, _]]
    Port(7) shouldBe a [Port[_]]

    Star("oops") shouldBe an [Else[_, _]]
    Star("oops") shouldBe an [Star[_]]
  }
  it can "have its non-inferred type widened by an apply call with a type param" in {
    /*
      scala> Port[Int].elseStar("hi")
      res0: org.scalautils.Star[Int,String] = Star(hi)

      scala> Port(3).elseStar[String]
      res1: org.scalautils.Port[Int,String] = Port(3)

      scala> Port(3).elseStar[ErrorMessage]
      res2: org.scalautils.Port[Int,org.scalautils.ErrorMessage] = Port(3)

      scala> Port(3).elseStar("oops")
      <console>:11: error: type mismatch;
       found   : String("oops")
       required: <:<[Nothing,?]
                    Port(3).elseStar("oops")
                                  ^

      scala> Port[Int].elseStar[String]
      <console>:11: error: missing arguments for method elseStar in class PortiePortieGumdrop;
      follow this method with `_' if you want to treat it as a partially applied function
                    Port[Int].elseStar[String]
                                   ^
    */
    // If the expected type is known, then you can just say Port or Star:
    Port(3) shouldBe Port(3)
    Star("oops") shouldBe Star("oops")

    // But if the expected type is not known, the inferred type of the other side will be Nothing:
    // Port(3) will be a Port[Int, Nothing]
    // Star("oops") will be a Star[Nothing, String]

    // If you want to specify a more specific type than Nothing, you can use this syntax:
    Port(3).elseStar[String] shouldBe Port(3)
    Port[Int].elseStar("oops") shouldBe Star("oops")

    // You could also do it this way:
    Port[Int](3) shouldBe Port(3)
    Star[String]("oops") shouldBe Star("oops")

    // But that requires that you also give a type that would be inferred from the value. This
    // would only be necessary if you wanted a more general type than that which
    // would otherwise be inferred from the given value, such as:
    Port[AnyVal](3) shouldBe Port(3)
    Star[AnyRef]("oops") shouldBe Star("oops")

    // In that case, though, I recommend a type ascription, because I think it is easier to read:
    (Port(3): AnyVal Else String) shouldBe Port(3)
    (Star("oops"): Int Else AnyRef) shouldBe Star("oops")
  }
  it can "be used in infix notation" in {
    def div(a: Int, b: Int): Int Else ArithmeticException = {
      try Port(a / b)
      catch { case ae: ArithmeticException => Star(ae) }
      if (b == 0)
        Star(new ArithmeticException("/ by zero"))
      else
        Port(a / b)
    }
    div(1, 1) shouldEqual Port(1)
    div(6, 2) shouldEqual Port(3)
    div(6, 2) shouldEqual Port(3)
    div(1, 0).isStar shouldBe true
    val ae = div(1, 0) match {
      case Star(ae) => ae
      case result => fail("didn't get an Star" + result)
    }
    ae should have message "/ by zero"
  }
  it can "be used with portMap" in {
    Port(8) portMap (_ + 1) should equal (Port(9))
    Port[Int].elseStar("eight") portMap (_ + 1) should equal (Star("eight"))
  }
  it can "be used with starMap" in {
    Port(8).elseStar[ErrorMessage] starMap (_.toUpperCase) should equal (Port(8))
    Port[Int].elseStar("eight") starMap (_.toUpperCase) should equal (Star("EIGHT"))
  }
  it can "be used with transform" in {
    Port(12).elseStar[String].transform((i: Int) => Port(i + 1), (s: String) => Star(s.toUpperCase)) should === (Port(13))
    Port[Int].elseStar("hi").transform((i: Int) => Port(i + 1), (s: String) => Star(s.toUpperCase)) should === (Star("HI"))
    Port(12).elseStar[String].transform((i: Int) => Star(i + 1), (s: String) => Port(s.toUpperCase)) should === (Star(13))
    Port[Int].elseStar("hi").transform((i: Int) => Star(i + 1), (s: String) => Port(s.toUpperCase)) should === (Port("HI"))
  }
  it can "be used with swap" in {
    Port(12).elseStar[String].swap should === (Port[String].elseStar(12))
    Port[Int].elseStar("hi").swap should === (Port("hi").elseStar[Int])
  }
  it can "be folded with fold" in {
    Port(3).elseStar[String].fold(_ + 1, _.length) shouldBe 4
    Port[Int].elseStar("howdy").fold(_ + 1, _.length) shouldBe 5
  }
  // SKIP-SCALATESTJS-START
  it can "be serialized correctly" in {
    serializeRoundtrip(Port(12)) shouldBe Port(12)
    serializeRoundtrip(Star("twelve")) shouldBe Star("twelve")
  }
  // SKIP-SCALATESTJS-END
  "The Else companion" should "offer a concise type lambda syntax" in {
    import scala.language.higherKinds
    trait Functor[Context[_]] {
      def map[A, B](ca: Context[A])(f: A => B): Context[B]
    }
    class PortElseFunctor[WHITE] extends Functor[Else.W[WHITE]#B] {
      override def map[B, C](ca: B Else WHITE)(f: B => C): C Else WHITE = ???
    }
    class StarElseFunctor[BLACK] extends Functor[Else.B[BLACK]#W] {
      override def map[W, X](ca: BLACK Else W)(f: W => X): BLACK Else X = ???
    }
  }
}

