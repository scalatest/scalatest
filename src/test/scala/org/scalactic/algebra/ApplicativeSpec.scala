/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.algebra

import org.scalactic.UnitSpec
//import org.scalatest.Matchers._

class ApplicativeSpec extends UnitSpec {

  "Option" should "obey the applicative functor laws" in {
    class OptionApplicativeAdapter[A](ap: Applicative[Option], optA: Option[A]) extends ApplicativeAdapter[Option, A](ap, optA) {
      override def applying[B](optAB: Option[A => B]): Option[B] = optAB.flatMap(ab => optA.map(ab))
    }
    class OptionApplicativeAdapter2[A, B](ap: Applicative[Option], optA: Option[A], optB: Option[B]) extends ApplicativeAdapter2[Option, A, B](ap, optA, optB)
    class OptionApplicativeAdapter3[A, B, C](ap: Applicative[Option], optA: Option[A], optB: Option[B], optC: Option[C]) extends ApplicativeAdapter3[Option, A, B, C](ap, optA, optB, optC)

    implicit object OptionApplicative extends Applicative[Option] {
      def apply[A](opt: Option[A]) = new OptionApplicativeAdapter[A](this, opt)
      def apply[A, B](optA: Option[A], optB: Option[B]) = new OptionApplicativeAdapter2(this, optA, optB)
      def apply[A, B, C](optA: Option[A], optB: Option[B], optC: Option[C]) = new OptionApplicativeAdapter3(this, optA, optB, optC)

      // a.k.a. pure, point
      override def insert[T](t: T): Option[T] = Option(t)
    }

    new ApplicativeLaws[Option]().assert()

  }

}
