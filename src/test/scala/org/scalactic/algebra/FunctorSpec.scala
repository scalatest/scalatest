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

class FunctorSpec extends UnitSpec {

  class OptionFunctorProxy[T](underlying: Option[T]) extends FunctorAdapter[Option, T] {
    def map[U](f: T => U): Option[U]  = underlying.map(f)
  }
  "A FunctorProxy" should "offer a map method that has the usual signature" in {
    val proxy = new OptionFunctorProxy(Some(3))
    proxy.map(_ + 1) shouldEqual Some(4)
  }

  "A Functor" should "offer an apply method that takes a TC[_] instance" in {
    class OptionFunctor extends Functor[Option] {
      def apply[T](opt: Option[T]): FunctorAdapter[Option, T] = new OptionFunctorProxy[T](opt)
    }
    val opt = Some(3)
    val optFun = new OptionFunctor
    val proxy = optFun(opt)
    proxy.map(_ + 1) shouldEqual Some(4)
  }
}

