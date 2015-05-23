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
  val isPosInt = Membership(pos)
  val isNegInt = Membership(neg)
  val isSmallInt = Membership(small)

  "An Membership" should "offer a constructor that takes a predicate" in {
    val pos = (i: Int) => i > 0
    val isPosInt = new Membership(pos)
    assert(isPosInt(Int.MaxValue))
    assert(isPosInt(88))
    assert(isPosInt(1))
    assert(!isPosInt(0))
    assert(!isPosInt(-1))
    assert(!isPosInt(Int.MinValue))
  }
  it should "offer an apply factory method that takes a predicate" in {
    val isShortString = Membership { (s: String) => s.length <= 3 }
    assert(isShortString("cat"))
    assert(isShortString("in"))
    assert(isShortString("a"))
    assert(isShortString(""))
    assert(!isShortString("flat"))
    assert(!isShortString("top hat"))
  }
  it should "offer an intersect method" in {
    val isPosAndNeg = isPosInt intersect isNegInt
    val isPosAndSmall = isPosInt intersect isSmallInt
    val isNegAndSmall = isNegInt intersect isSmallInt
    assert(!isPosAndNeg(-1))
    assert(!isPosAndNeg(0))
    assert(!isPosAndNeg(1))
    assert(!isPosAndSmall(-1))
    assert(!isPosAndSmall(0))
    assert(isPosAndSmall(1))
    assert(isNegAndSmall(-1))
    assert(!isNegAndSmall(0))
    assert(!isNegAndSmall(1))
  }
  it should "have a pretty toString" in {
    Membership[Int](i => true).toString shouldEqual "<membership>"
  }
  it should "offer a union method" in {
    val isPosOrNeg = isPosInt union isNegInt
    val isPosOrSmall = isPosInt union isSmallInt
    val isNegOrSmall = isNegInt union isSmallInt
    assert(isPosOrNeg(-1))
    assert(!isPosOrNeg(0))
    assert(isPosOrNeg(1))
    assert(!isPosOrSmall(-9))
    assert(isPosOrSmall(-1))
    assert(isPosOrSmall(0))
    assert(isPosOrSmall(1))
    assert(isPosOrSmall(9))
    assert(isNegOrSmall(-9))
    assert(isNegOrSmall(-1))
    assert(isNegOrSmall(0))
    assert(isNegOrSmall(1))
    assert(!isNegOrSmall(9))
  }
  it should "offer a diff method" in {
    val isPosAndNotNeg = isPosInt diff isNegInt
    val isPosAndNotSmall = isPosInt diff isSmallInt
    val isNegAndNotSmall = isNegInt diff isSmallInt
    assert(isPosAndNotNeg(-1))
    assert(isPosAndNotNeg(0))
    assert(isPosAndNotNeg(1))
    assert(isPosAndNotSmall(-1))
    assert(isPosAndNotSmall(0))
    assert(!isPosAndNotSmall(1))
    assert(!isNegAndNotSmall(-1))
    assert(isNegAndNotSmall(0))
    assert(isNegAndNotSmall(1))
  }
  it should "offer a complement method" in {
    val isNonPosInt = isPosInt.complement
    val isNonNegInt = isNegInt.complement
    val isNonSmallInt = isSmallInt.complement
    assert(isNonPosInt(-1))
    assert(isNonPosInt(0))
    assert(!isNonPosInt(1))
    assert(!isNonNegInt(-1))
    assert(isNonNegInt(0))
    assert(isNonNegInt(1))
    assert(isNonSmallInt(9))
    assert(!isNonSmallInt(-1))
    assert(!isNonSmallInt(0))
    assert(!isNonSmallInt(1))
    assert(isNonSmallInt(-9))
  }
}

