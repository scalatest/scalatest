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

class MonadSpec extends UnitSpec {

  class OptionMonadProxy[T](underlying: Option[T]) extends MonadProxy[Option, T] {
    def map[U](f: T => U): Option[U]  = underlying.map(f)
    def flatMap[U](f: T => Option[U]): Option[U]  = underlying.flatMap(f)
  }

  class OptionMonad extends Monad[Option] {
    def apply[T](opt: Option[T]): MonadProxy[Option, T] = new OptionMonadProxy[T](opt)
    def insert[T](o: T): Option[T] = Option(o) 
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
}

