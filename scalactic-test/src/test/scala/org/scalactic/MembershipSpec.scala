/*
 * Copyright 2001-2015 Artima, Inc.
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

class MembershipSpec extends UnitSpec {

  val pos = (i: Int) => i > 0
  val neg = (i: Int) => i < 0
  val small = (i: Int) => i >= -5 && i <= 5
  val posInts = Membership(pos)
  val negInts = Membership(neg)
  val smallInts = Membership(small)

  "An Membership" should "offer a constructor that takes a predicate" in {
    val pos = (i: Int) => i > 0
    val posInts = new Membership(pos)
    assert(posInts.contains(Int.MaxValue))
    assert(posInts.contains(88))
    assert(posInts.contains(1))
    assert(!posInts.contains(0))
    assert(!posInts.contains(-1))
    assert(!posInts.contains(Int.MinValue))
  }
  it should "offer an apply factory method that takes a predicate" in {
    val shortStrings = Membership { (s: String) => s.length <= 3 }
    assert(shortStrings.contains("cat"))
    assert(shortStrings.contains("in"))
    assert(shortStrings.contains("a"))
    assert(shortStrings.contains(""))
    assert(!shortStrings.contains("flat"))
    assert(!shortStrings.contains("top hat"))
  }
  it should "offer an intersect method" in {
    val posAndNeg = posInts intersect negInts
    val posAndSmall = posInts intersect smallInts
    val negAndSmall = negInts intersect smallInts
    assert(!posAndNeg.contains(-1))
    assert(!posAndNeg.contains(0))
    assert(!posAndNeg.contains(1))
    assert(!posAndSmall.contains(-1))
    assert(!posAndSmall.contains(0))
    assert(posAndSmall.contains(1))
    assert(negAndSmall.contains(-1))
    assert(!negAndSmall.contains(0))
    assert(!negAndSmall.contains(1))
  }
  it should "have a pretty toString" in {
    Membership[Int](i => true).toString shouldEqual "<membership>"
  }
  it should "offer a union method" in {
    val posOrNeg = posInts union negInts
    val posOrSmall = posInts union smallInts
    val negOrSmall = negInts union smallInts
    assert(posOrNeg.contains(-1))
    assert(!posOrNeg.contains(0))
    assert(posOrNeg.contains(1))
    assert(!posOrSmall.contains(-9))
    assert(posOrSmall.contains(-1))
    assert(posOrSmall.contains(0))
    assert(posOrSmall.contains(1))
    assert(posOrSmall.contains(9))
    assert(negOrSmall.contains(-9))
    assert(negOrSmall.contains(-1))
    assert(negOrSmall.contains(0))
    assert(negOrSmall.contains(1))
    assert(!negOrSmall.contains(9))
  }
  it should "offer a diff method" in {
    val posAndNotNeg = posInts diff negInts
    val posAndNotSmall = posInts diff smallInts
    val negAndNotSmall = negInts diff smallInts
    assert(posAndNotNeg.contains(-1))
    assert(posAndNotNeg.contains(0))
    assert(posAndNotNeg.contains(1))
    assert(posAndNotSmall.contains(-1))
    assert(posAndNotSmall.contains(0))
    assert(!posAndNotSmall.contains(1))
    assert(!negAndNotSmall.contains(-1))
    assert(negAndNotSmall.contains(0))
    assert(negAndNotSmall.contains(1))
  }
  it should "offer a complement method" in {
    val nonPosInts = posInts.complement
    val nonNegInts = negInts.complement
    val nonSmallInts = smallInts.complement
    assert(nonPosInts.contains(-1))
    assert(nonPosInts.contains(0))
    assert(!nonPosInts.contains(1))
    assert(!nonNegInts.contains(-1))
    assert(nonNegInts.contains(0))
    assert(nonNegInts.contains(1))
    assert(nonSmallInts.contains(9))
    assert(!nonSmallInts.contains(-1))
    assert(!nonSmallInts.contains(0))
    assert(!nonSmallInts.contains(1))
    assert(nonSmallInts.contains(-9))
  }
}

