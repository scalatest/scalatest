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
import org.scalactic.Every

class AssociativeSpec extends UnitSpec {
  
  // Every Associative
  class EveryAssociative[A] extends Associative[Every[A]] {
    def apply(a: Every[A]): EveryAssociativeAdapter[A] = EveryAssociativeAdapter(a)
  }
  
  case class EveryAssociativeAdapter[A](underlying: Every[A]) extends AssociativeAdapter[Every[A]] {
    def op(other: Every[A]): EveryAssociativeAdapter[A] = EveryAssociativeAdapter(underlying ++ other)
  }

  implicit def Every2EveryAssociativeAdapter[A](every: Every[A]) = new EveryAssociative[A].apply(every)
  implicit def EveryAssociativeAdapter2Every[A](adapt: EveryAssociativeAdapter[A]): Every[A] = adapt.underlying
  
  // Int Multiplication Associatvie
  class IntMultiAssociative extends Associative[Int] {
    def apply(x: Int): IntMultiAA = IntMultiAA(x)
  }

  case class IntMultiAA(underlying: Int) extends AssociativeAdapter[Int] {
    override 
    def op(other: Int): IntMultiAA = IntMultiAA(underlying * other)
  }
  implicit def Int2IntMultiAA(x: Int): IntMultiAA = new IntMultiAssociative().apply(x)
  implicit def IntMultiAA2Int(adapt: IntMultiAA): Int = adapt.underlying
  
  // Bad case Substraction Associative, should fail.
  class BadSubstractionAssociative extends Associative[Int] {
    override 
    def apply(x: Int): BadSubstractionAA = BadSubstractionAA(x)
  }
  
  case class BadSubstractionAA(underlying: Int) extends AssociativeAdapter[Int] {
    override 
    def op(other: Int): BadSubstractionAA = BadSubstractionAA(underlying - other)
  }
  implicit def Int2IntBadSubAssociativeAdapter(x: Int): BadSubstractionAA = new BadSubstractionAssociative().apply(x)
  implicit def BadSubstractionAA2Int(adapt: BadSubstractionAA): Int = adapt.underlying
  
  "An Every Associative (Semigroup) " should " have a binaray associative op" in {
    val a = Every(1,2)
    val b = Every(5,6,7)
    val c = Every(9,9)
    ((a op b) op c).underlying shouldEqual (a op (b op c)).underlying
  }
  
  "An Int Associative (Semigroup) " should  "have a binaray associative op" in {
    val intAssoc = new IntMultiAssociative()
    val a = intAssoc(1)
    val b = intAssoc(64)
    val c = intAssoc(256)
    ((a op b) op c) shouldEqual (a op (b op c))
  }
  
  "A BadSubstractionAssociative " should  " fail to be associative" in {
    val badSubAssoc = new BadSubstractionAssociative()
    val a = badSubAssoc(1)
    val b = badSubAssoc(64)
    val c = badSubAssoc(256)
    ((a op b) op c) should not be (a op (b op c))
  }
  
}