
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
import prop.GeneratorDrivenPropertyChecks._
import algebra._

class OptionLawsSpec extends UnitSpec with CheckedEquality {

  class OptionFunctorProxy[T](underlying: Option[T]) extends FunctorProxy[Option, T] {
    def map[U](f: T => U): Option[U]  = underlying.map(f)
  }
  object OptionFunctor extends Functor[Option] {
    def apply[T](opt: Option[T]): FunctorProxy[Option, T] = new OptionFunctorProxy[T](opt)
  }
  "Option's map method" should "obey the functor laws" in {
    def id[T] = (o: T) => o
    forAll { opt: Option[Int] =>
      opt.map(id) shouldEqual opt
    }
    forAll { (opt: Option[Int], f: Int => Int, g: Int => Int) =>
      (opt.map(g)).map(f) shouldEqual opt.map(f compose g)
    }
  }
  "A FunctorProxy[Option, T]'s map method" should "obey the functor laws" in {
    def id[T] = (o: T) => o
    forAll { opt: Option[Int] =>
      OptionFunctor(opt).map(id) shouldEqual opt
    }
    forAll { (opt: Option[Int], f: String => String, g: Int => String) =>
      OptionFunctor((OptionFunctor(opt).map(g))).map(f) shouldEqual OptionFunctor(opt).map(f compose g)
    }
  }
}

