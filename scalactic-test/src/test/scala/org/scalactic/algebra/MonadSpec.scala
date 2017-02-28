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

  "A Monad" should "offer a flatten method" in {
    class ListMonad extends Monad[List] {
      override def flatMap[A, B](ca: List[A])(f: (A) => List[B]): List[B] = ca.flatMap(f)
      override def insert[A](a: A): List[A] = List(a)
    }

    implicit val listMonad = new ListMonad

    Monad[List].flatten(List(List(1, 2), List(3, 4), List(5, 6))) shouldEqual List(1, 2, 3, 4, 5, 6)
  }

  it should "provide an instance for List" in {
    MonadLaws[List].check()
  }
  it should "provide an instance for Option" in {
    MonadLaws[Option].check()
  }
  it should "provide an instance for Or, which abstracts over the Good side" in {
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))
    MonadLaws[Or.B[Int]#G].check()
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

