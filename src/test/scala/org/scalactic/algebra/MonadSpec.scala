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

import org.scalacheck.Arbitrary
import org.scalactic.Or.B
import org.scalactic.{Good, Or, UnitSpec}
import org.scalatest.laws.MonadLaws

import scala.language.implicitConversions

class MonadSpec extends UnitSpec {

  "List" should "obey the monad laws" in {

    class ListMonad extends Monad[List] {
      override def flatMap[A, B](ca: List[A])(f: (A) => List[B]): List[B] = ca.flatMap(f)
      override def insert[A](a: A): List[A] = List(a)
    }

    implicit val listMonad = new ListMonad

    new MonadLaws[List].assert()
  }

  "Option" should "obey the monad laws" in {
    class OptionMonad extends Monad[Option] {
      override def flatMap[A, B](ca: Option[A])(f: (A) => Option[B]): Option[B] = ca.flatMap(f)
      override def insert[A](a: A): Option[A] = Option(a)
    }

    implicit val optionMonad = new OptionMonad

    new MonadLaws[Option].assert()
  }

  "The good nature of Or" should "obey the monad laws" in {
    class OrMonad[BAD] extends Monad[Or.B[BAD]#G] {
      override def flatMap[A, B](ca: Or.B[BAD]#G[A])(f: (A) => Or.B[BAD]#G[B]): Or.B[BAD]#G[B] =
        ca.flatMap(f)
      override def insert[A](a: A): B[BAD]#G[A] = Good(a)
    }

    implicit val orMonad = new OrMonad[Int]
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))

    new MonadLaws[Or.B[Int]#G].assert()
  }

  "A Monad" should "offer a flatten method" in {
    class ListMonad extends Monad[List] {
      override def flatMap[A, B](ca: List[A])(f: (A) => List[B]): List[B] = ca.flatMap(f)
      override def insert[A](a: A): List[A] = List(a)
    }

    implicit val listMonad = new ListMonad

    Monad[List].flatten(List(List(1, 2), List(3, 4), List(5, 6))) shouldEqual List(1, 2, 3, 4, 5, 6)
  }
  "A Monad Adapter" should "offer a flatten method" in {
    class ListMonad extends Monad[List] {
      override def flatMap[A, B](ca: List[A])(f: (A) => List[B]): List[B] = ca.flatMap(f)
      override def insert[A](a: A): List[A] = List(a)
    }

    implicit val listMonad = new ListMonad

    val adapted = new Monad.Adapter[List, List[Int]]((List(List(1, 2), List(3, 4), List(5, 6))))
    adapted.flatten shouldEqual List(1, 2, 3, 4, 5, 6)
  }
}

