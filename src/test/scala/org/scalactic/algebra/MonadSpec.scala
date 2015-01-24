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
      override def applying[A, B](list: List[A])(listAB: List[A => B]): List[B] = list.flatMap(a => listAB.map(ab => ab(a)))
    }

    implicit val listMonad = new ListMonad

    new MonadLaws[List].assert()
  }


  "Option" should "obey the monad laws" in {
    class OptionMonad extends Monad[Option] {
      override def flatMap[A, B](ca: Option[A])(f: (A) => Option[B]): Option[B] = ca.flatMap(f)
      override def insert[A](a: A): Option[A] = Option(a)
      override def applying[A, B](ca: Option[A])(cab: Option[(A) => B]): Option[B] = ca.flatMap(a => cab.map(ab => ab(a)))
    }

    implicit val optionMonad = new OptionMonad

    new MonadLaws[Option].assert()
  }

  "The good " should "obey the monad laws" in {
    class OrMonad[BAD] extends Monad[Or.B[BAD]#G] {
      override def flatMap[A, B](ca: Or.B[BAD]#G[A])(f: (A) => Or.B[BAD]#G[B]): Or.B[BAD]#G[B] =
        ca.flatMap(f)
      override def insert[A](a: A): B[BAD]#G[A] = Good(a)
      override def applying[A, B](ca: Or.B[BAD]#G[A])(cab: Or.B[BAD]#G[(A) => B]): Or.B[BAD]#G[B] =
        ca.flatMap(a => cab.map(ab => ab(a)))
    }

    implicit val orMonad = new OrMonad[Int]
    implicit def orArbGood[G, B](implicit arbG: Arbitrary[G]): Arbitrary[G Or B] = Arbitrary(for (g <- Arbitrary.arbitrary[G]) yield Good(g))

    new MonadLaws[Or.B[Int]#G].assert()
  }


  /**
  class OptionMonadProxy[T](underlying: Option[T]) extends MonadProxy[Option, T] {
    def map[U](f: T => U): Option[U]  = underlying.map(f)
    def flatMap[U](f: T => Option[U]): Option[U]  = underlying.flatMap(f)
  }

  class OptionMonad extends Monad[Option] {
    def apply[T](opt: Option[T]): MonadProxy[Option, T] = new OptionMonadProxy[T](opt)
    def insert[T](o: T): Option[T] = Option(o) 
  }

  class ListMonadProxy[T](underlying: List[T]) extends MonadProxy[List, T] {
     def map[U](f: T => U): List[U]  = underlying.map(f)
     def flatMap[U](f: T => List[U]): List[U]  = underlying.flatMap(f)
   }

   class ListMonad extends Monad[List] {
     def apply[T](ls: List[T]): MonadProxy[List, T] = new ListMonadProxy[T](ls)
     def insert[T](o: T): List[T] = List(o)
   }

  "A MonadProxy" should "offer a map method that has the usual signature" in {
    val proxy: MonadProxy[Option, Int] = new OptionMonadProxy(Some(3))
    proxy.map(_ + 1) shouldEqual Some(4)
  }
  
  it should "also offer a flatmap" in {
    val proxy: MonadProxy[Option, Int] = new OptionMonadProxy(Some(3))
    val f: Int => Option[Int] = (x: Int) => Some(x + 1) 
    proxy.flatMap(f) shouldEqual Some(4)
  }

  "A Monad" should "offer an apply method that takes a TC[_] instance" in {
    val opt = Some(3)
    val optFun: Monad[Option] = new OptionMonad
    val proxy = optFun(opt)
    proxy.map(_ + 1) shouldEqual Some(4)
  }

  it should "offer an insert method that given a T return a TC[T]" in {
    val optFun: Monad[Option] = new OptionMonad
    optFun.insert(3) shouldEqual Some(3)
  }
  
  it should "allow you to nest a TC inside a TC" in {
    val optFun: Monad[Option] = new OptionMonad
    optFun.insert(Option(3)) shouldEqual Some(Some(3))
  }
  **/
}

